; c2-codegen-emit.pbi
; Instruction emission for code generation
; V1.035.8: Initial creation - extracted from c2-codegen-v08.pbi
;
; Purpose: Centralized EmitInt procedure for bytecode emission with optimizations
; Dependencies:
;   - c2-codegen-rules.pbi (for GetStoreOpcodeByFlags, GetMovOpcodeByFlags, etc.)

; V1.031.29: OS-agnostic debug macro - outputs to stdout for console builds
; Set #OSDEBUG = 1 to enable debug output, 0 to disable
#OSDEBUG = 0

Macro OSDebug(msg)
   CompilerIf #OSDEBUG
      PrintN(msg)
   CompilerEndIf
EndMacro

;- ============================================================================
;- Helper: Check if variable is local
;- ============================================================================

; Helper: Check if variable should use local opcodes (LocalVars array)
; Returns true for both parameters and local variables in functions
;
; Parameters:
;   varIndex - Variable slot index
;
; Returns:
;   True if variable is local, False otherwise
;
Procedure.b IsLocalVar(varIndex.i)
   If varIndex < 0 Or varIndex >= gnLastVariable
      ProcedureReturn #False
   EndIf

   ; V1.034.37: Constants are NEVER local (always global storage)
   If gVarMeta(varIndex)\flags & #C2FLAG_CONST
      ProcedureReturn #False
   EndIf

   ; Parameters use LocalVars array
   If gVarMeta(varIndex)\flags & #C2FLAG_PARAM
      ProcedureReturn #True
   EndIf

   ; V1.029.10: Any variable with valid paramOffset is local
   ; This covers struct fields of local parameters (e.g., r\bottomRight\x)
   If gVarMeta(varIndex)\paramOffset >= 0 And gCurrentFunctionName <> ""
      ProcedureReturn #True
   EndIf

   ; Non-parameter locals: check if name is mangled with function name OR synthetic ($temp)
   If gCurrentFunctionName <> ""
      If LCase(Left(gVarMeta(varIndex)\name, Len(gCurrentFunctionName) + 1)) = LCase(gCurrentFunctionName + "_")
         ProcedureReturn #True
      EndIf
      ; Synthetic temporaries (starting with $) are also local when inside a function
      ; V1.022.28: UNLESS paramOffset = -1 (forced global for slot-only optimization)
      If Left(gVarMeta(varIndex)\name, 1) = "$"
         If gVarMeta(varIndex)\paramOffset >= 0
            ProcedureReturn #True
         EndIf
      EndIf
   EndIf

   ProcedureReturn #False
EndProcedure

;- ============================================================================
;- Helper: Mark variable for preloading
;- ============================================================================

; Mark variable for preloading when assigned from constant
; This copies constant values to the destination's gVarMeta for template building
;
; Parameters:
;   srcSlot - Source slot (constant)
;   dstSlot - Destination slot
;
Procedure MarkPreloadable(srcSlot.i, dstSlot.i)
   ; V1.027.9: First check if destination already has PRELOAD or ASSIGNED
   If gVarMeta(dstSlot)\flags & (#C2FLAG_PRELOAD | #C2FLAG_ASSIGNED)
      ; Already marked, skip
      CompilerIf #DEBUG
         If gVarMeta(dstSlot)\flags & #C2FLAG_PRELOAD
            Debug "V1.027.9: MarkPreloadable SKIPPED '" + gVarMeta(dstSlot)\name + "' already PRELOAD (srcSlot=" + Str(srcSlot) + " value=" + Str(gVarMeta(srcSlot)\valueInt) + ")"
         Else
            Debug "V1.027.9: MarkPreloadable SKIPPED '" + gVarMeta(dstSlot)\name + "' already ASSIGNED (srcSlot=" + Str(srcSlot) + " value=" + Str(gVarMeta(srcSlot)\valueInt) + ")"
         EndIf
      CompilerEndIf
   ElseIf gVarMeta(srcSlot)\flags & #C2FLAG_CONST
      ; Source is constant and destination not already marked - set PRELOAD
      ; Copy constant value to destination's gVarMeta
      If gVarMeta(srcSlot)\flags & #C2FLAG_INT
         gVarMeta(dstSlot)\valueInt = gVarMeta(srcSlot)\valueInt
         CompilerIf #DEBUG
            Debug "V1.027.9: MarkPreloadable SET '" + gVarMeta(dstSlot)\name + "' valueInt=" + Str(gVarMeta(srcSlot)\valueInt) + " (srcSlot=" + Str(srcSlot) + " dstSlot=" + Str(dstSlot) + ")"
         CompilerEndIf
      ElseIf gVarMeta(srcSlot)\flags & #C2FLAG_FLOAT
         gVarMeta(dstSlot)\valueFloat = gVarMeta(srcSlot)\valueFloat
      ElseIf gVarMeta(srcSlot)\flags & #C2FLAG_STR
         gVarMeta(dstSlot)\valueString = gVarMeta(srcSlot)\valueString
      EndIf
      ; Mark as preloadable
      gVarMeta(dstSlot)\flags = gVarMeta(dstSlot)\flags | #C2FLAG_PRELOAD
      CompilerIf #DEBUG
         Debug "V1.023.0: Marked slot " + Str(dstSlot) + " (" + gVarMeta(dstSlot)\name + ") for preload from const slot " + Str(srcSlot)
      CompilerEndIf
   Else
      ; V1.027.9: Source is not constant - set ASSIGNED to prevent late PRELOAD marking
      ; Only for global variables (paramOffset = -1)
      If gVarMeta(dstSlot)\paramOffset = -1
         gVarMeta(dstSlot)\flags = gVarMeta(dstSlot)\flags | #C2FLAG_ASSIGNED
         CompilerIf #DEBUG
            Debug "V1.027.9: MarkPreloadable SET ASSIGNED '" + gVarMeta(dstSlot)\name + "' (non-const MOV, srcSlot=" + Str(srcSlot) + ")"
         CompilerEndIf
      EndIf
   EndIf
EndProcedure

;- ============================================================================
;- Main Instruction Emission Procedure
;- ============================================================================

; Emit a bytecode instruction with optional variable reference
; Handles PUSH+STORE, FETCH+STORE optimizations via MOV fusion
;
; Parameters:
;   op   - Opcode to emit
;   nVar - Variable slot index (-1 for none)
;
Procedure EmitInt(op.i, nVar.i = -1)
   Protected sourceFlags.w, destFlags.w
   Protected isSourceLocal.b, isDestLocal.b
   Protected sourceFlags2.w, destFlags2.w
   Protected localOffset.i, localOffset2.i, localOffset3.i, localOffset4.i
   Protected savedSource.i, savedSrc2.i
   Protected inTernary2.b
   Protected currentCode.i
   ; V1.029.39: Struct field store variables (moved to top)
   Protected ssBaseSlot.i, ssByteOffset.i, ssIsLocal.b, ssFieldType.w
   Protected ssStructByteSize.i  ; V1.029.40: For lazy STRUCT_ALLOC

   ; V1.029.37: Struct field store - check if destination is a struct field with \ptr storage
   ; Handle this BEFORE PUSH+STORE optimization since struct fields use different opcodes
   If nVar >= 0 And (op = #ljSTORE Or op = #ljSTOREF Or op = #ljSTORES Or op = #ljPOP Or op = #ljPOPF Or op = #ljPOPS)
      ; V1.030.63: Debug - track struct field base issues
      If FindString(gVarMeta(nVar)\name, "_w") Or FindString(gVarMeta(nVar)\name, "_h")
         Debug "V1.030.63 EMITINT: slot=" + Str(nVar) + " name='" + gVarMeta(nVar)\name + "' structFieldBase=" + Str(gVarMeta(nVar)\structFieldBase) + " op=" + Str(op)
      EndIf
      If gVarMeta(nVar)\structFieldBase >= 0
         ; This is a struct field assignment - emit STRUCT_STORE_* instead
         ssBaseSlot = gVarMeta(nVar)\structFieldBase
         ssByteOffset = gVarMeta(nVar)\structFieldOffset
         ssIsLocal = Bool(gVarMeta(ssBaseSlot)\paramOffset >= 0)

         ; V1.035.2: Use LookupStructFieldType from c2-codegen-rules.pbi
         ; Handles nested structs by walking the type chain
         ssFieldType = 0
         If gVarMeta(ssBaseSlot)\structType <> ""
            ssFieldType = LookupStructFieldType(gVarMeta(ssBaseSlot)\structType, ssByteOffset)
         EndIf

         ; V1.029.40: Lazy STRUCT_ALLOC_LOCAL - emit on first field access for LOCAL structs
         ; Global structs are pre-allocated by VM in vmTransferMetaToRuntime()
         ; V1.029.65: Skip for struct PARAMETERS - they receive pointer from caller via FETCH_STRUCT
         Protected ssIsParam.b = Bool(gVarMeta(ssBaseSlot)\flags & #C2FLAG_PARAM)
         If ssIsLocal And Not ssIsParam And Not gVarMeta(ssBaseSlot)\structAllocEmitted
            ; Calculate byte size from struct definition
            ssStructByteSize = 8  ; Default 8 bytes (1 field)
            If gVarMeta(ssBaseSlot)\structType <> "" And FindMapElement(mapStructDefs(), gVarMeta(ssBaseSlot)\structType)
               ssStructByteSize = mapStructDefs()\totalSize * 8  ; 8 bytes per field
            EndIf

            ; Emit STRUCT_ALLOC_LOCAL before the store
            gEmitIntLastOp = AddElement(llObjects())
            llObjects()\code = #ljSTRUCT_ALLOC_LOCAL
            llObjects()\i = gVarMeta(ssBaseSlot)\paramOffset
            llObjects()\j = ssStructByteSize

            ; Mark as allocated
            gVarMeta(ssBaseSlot)\structAllocEmitted = #True
         EndIf

         ; V1.035.2: Add new element and use GetStructFieldOpcode for type dispatch
         gEmitIntLastOp = AddElement(llObjects())
         llObjects()\code = GetStructFieldOpcode(#True, ssIsLocal, ssFieldType)
         If ssIsLocal
            llObjects()\i = gVarMeta(ssBaseSlot)\paramOffset
         Else
            llObjects()\i = ssBaseSlot
         EndIf
         llObjects()\j = ssByteOffset  ; Byte offset within struct

         gEmitIntCmd = llObjects()\code
         ProcedureReturn
      EndIf
   EndIf

   If gEmitIntCmd = #ljpush And op = #ljStore
      ; PUSH+STORE optimization
      ; Don't optimize inside ternary expressions - both branches need stack values
      ; Check if PUSH instruction is marked as part of ternary
      Protected inTernary.b = (llObjects()\flags & #INST_FLAG_TERNARY)

      sourceFlags = gVarMeta(llObjects()\i)\flags
      destFlags = gVarMeta(nVar)\flags
      isSourceLocal = IsLocalVar(llObjects()\i)
      isDestLocal = IsLocalVar(nVar)

      ; Only optimize to MOV if BOTH are not local (globals can use MOV)
      ; Or if destination is local (use LMOV)
      If Not inTernary And Not ((sourceFlags & #C2FLAG_PARAM) Or (destFlags & #C2FLAG_PARAM))
         ; Neither is parameter - can optimize to MOV
         If isDestLocal
            ; Destination is local - use LMOV
            ; For LMOV: i = paramOffset (destination), j = source varIndex
            savedSource = llObjects()\i  ; Save source BEFORE overwriting
            localOffset = gVarMeta(nVar)\paramOffset

            ; Safety check: Ensure paramOffset is valid (should be >= 0 and < 20)
            ; If paramOffset looks invalid (too large - likely a slot number), fall back to PUSH+STORE
            If localOffset < 0 Or localOffset >= 20
               ; paramOffset not set or suspiciously large - fall back to PUSH+STORE
               gEmitIntLastOp = AddElement(llObjects())
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetStoreOpcodeByFlags(destFlags)
               llObjects()\i = nVar
            Else
               ; V1.034.17: Unified MOV with n field for locality
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetMovOpcodeByFlags(sourceFlags, destFlags)
               ; V1.035.6: Use rule-based locality computation
               Protected srcOffset0.i = gVarMeta(savedSource)\paramOffset
               Protected outN0.Integer, outJ0.Integer, outI0.Integer
               ComputeMovLocality(savedSource, nVar, isSourceLocal, #True, srcOffset0, localOffset, @outN0, @outJ0, @outI0)
               llObjects()\n = outN0\i
               llObjects()\j = outJ0\i
               llObjects()\i = outI0\i
               ; V1.023.0: Mark for preloading if assigning from constant
               MarkPreloadable(savedSource, nVar)
            EndIf
         Else
            ; V1.034.17: Global destination - use unified MOV with n field for locality
            ; V1.022.18: Preserve #C2FLAG_STRUCT when updating type flags for struct base slots
            ; V1.027.8: Also preserve #C2FLAG_PRELOAD to keep preload optimization working
            ; V1.027.9: Also preserve #C2FLAG_ASSIGNED to prevent late PRELOAD marking
            ; V1.035.6: Use rule-based opcode selection
            llObjects()\code = GetMovOpcodeByFlags(sourceFlags, destFlags)
            ; Update destination type flags (unless pointer)
            Protected preserveMask1.w = #C2FLAG_STRUCT | #C2FLAG_PRELOAD | #C2FLAG_ASSIGNED
            If sourceFlags & #C2FLAG_STR
               gVarMeta(nVar)\flags = (gVarMeta(nVar)\flags & preserveMask1) | #C2FLAG_IDENT | #C2FLAG_STR
            ElseIf sourceFlags & #C2FLAG_FLOAT
               gVarMeta(nVar)\flags = (gVarMeta(nVar)\flags & preserveMask1) | #C2FLAG_IDENT | #C2FLAG_FLOAT
            ElseIf Not (destFlags & #C2FLAG_POINTER)
               gVarMeta(nVar)\flags = (gVarMeta(nVar)\flags & preserveMask1) | #C2FLAG_IDENT | #C2FLAG_INT
            EndIf
            ; V1.035.6: Use rule-based locality computation
            Protected savedSrc1.i = llObjects()\i
            Protected srcOffset1.i = gVarMeta(savedSrc1)\paramOffset
            Protected outN1.Integer, outJ1.Integer, outI1.Integer
            ComputeMovLocality(savedSrc1, nVar, isSourceLocal, #False, srcOffset1, 0, @outN1, @outJ1, @outI1)
            llObjects()\n = outN1\i
            llObjects()\j = outJ1\i
            llObjects()\i = outI1\i
            ; V1.023.0: Mark for preloading if assigning from constant
            MarkPreloadable(savedSrc1, nVar)
         EndIf
      Else
         ; One is a parameter - keep as PUSH+STORE but use local version if dest is local
         gEmitIntLastOp = AddElement(llObjects())
         If isDestLocal
            localOffset3 = gVarMeta(nVar)\paramOffset

            ; Safety check: Ensure paramOffset is valid (should be >= 0 and < 20)
            If localOffset3 >= 0 And localOffset3 < 20
               ; V1.034.19: Use unified STORE with j=1 instead of LSTORE
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetStoreOpcodeByFlags(destFlags)
               ; Set the local variable index (paramOffset) and local flag
               llObjects()\i = localOffset3
               llObjects()\j = 1  ; j=1 means local
            Else
               ; paramOffset not set - use global STORE
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetStoreOpcodeByFlags(destFlags)
               llObjects()\i = nVar
            EndIf
         Else
            llObjects()\code = op
         EndIf
      EndIf
   ElseIf gEmitIntCmd = #ljfetch And op = #ljstore
      ; FETCH+STORE optimization
      ; Don't optimize inside ternary expressions - both branches need stack values
      ; Check if FETCH instruction is marked as part of ternary
      inTernary2 = (llObjects()\flags & #INST_FLAG_TERNARY)

      ; V1.034.32: FIX - When llObjects()\j = 1, the source is a LOCAL variable.
      ; In this case, llObjects()\i contains paramOffset, NOT a slot number!
      ; We cannot safely look up gVarMeta(llObjects()\i) for local sources.
      ; For local sources, treat as if PARAM flag is set to skip MOV optimization.
      Protected isSourceLocalFetch.b = Bool(llObjects()\j = 1)
      If isSourceLocalFetch
         ; Source is local - cannot look up gVarMeta, assume PARAM to skip MOV
         sourceFlags2 = #C2FLAG_PARAM
      Else
         sourceFlags2 = gVarMeta(llObjects()\i)\flags
      EndIf
      destFlags2 = gVarMeta(nVar)\flags
      isSourceLocal = Bool(isSourceLocalFetch Or IsLocalVar(llObjects()\i))
      isDestLocal = IsLocalVar(nVar)

      If Not inTernary2 And Not ((sourceFlags2 & #C2FLAG_PARAM) Or (destFlags2 & #C2FLAG_PARAM))
         ; Can optimize to MOV or LMOV
         If isDestLocal
            ; Use LMOV for local destination
            localOffset2 = gVarMeta(nVar)\paramOffset

            ; Safety check: Ensure paramOffset is valid (should be >= 0 and < 20)
            If localOffset2 < 0 Or localOffset2 >= 20
               ; paramOffset not set - fall back to FETCH+STORE
               gEmitIntLastOp = AddElement(llObjects())
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetStoreOpcodeByFlags(destFlags2)
               llObjects()\i = nVar
            Else
               ; V1.034.17: Unified MOV with n field for locality
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetMovOpcodeByFlags(sourceFlags2, destFlags2)
               savedSrc2 = llObjects()\i
               ; V1.035.6: Use rule-based locality computation
               Protected srcOffset2.i = gVarMeta(savedSrc2)\paramOffset
               Protected outN2.Integer, outJ2.Integer, outI2.Integer
               ComputeMovLocality(savedSrc2, nVar, isSourceLocal, #True, srcOffset2, localOffset2, @outN2, @outJ2, @outI2)
               llObjects()\n = outN2\i
               llObjects()\j = outJ2\i
               llObjects()\i = outI2\i
               ; V1.023.0: Mark for preloading if assigning from constant
               MarkPreloadable(savedSrc2, nVar)
            EndIf
         Else
            ; V1.034.17: Unified MOV for global destination with n field
            ; V1.035.6: Use rule-based opcode selection
            llObjects()\code = GetMovOpcodeByFlags(sourceFlags2, destFlags2)
            ; V1.035.6: Use rule-based locality computation
            Protected savedSrc3.i = llObjects()\i
            Protected srcOffset3.i = gVarMeta(savedSrc3)\paramOffset
            Protected outN3.Integer, outJ3.Integer, outI3.Integer
            ComputeMovLocality(savedSrc3, nVar, isSourceLocal, #False, srcOffset3, 0, @outN3, @outJ3, @outI3)
            llObjects()\n = outN3\i
            llObjects()\j = outJ3\i
            llObjects()\i = outI3\i
            ; V1.023.0: Mark for preloading if assigning from constant
            MarkPreloadable(savedSrc3, nVar)
         EndIf
      Else
         ; Keep as FETCH+STORE but use local version if appropriate
         gEmitIntLastOp = AddElement(llObjects())
         If isDestLocal
            localOffset4 = gVarMeta(nVar)\paramOffset

            ; Safety check: Ensure paramOffset is valid (should be >= 0 and < 20)
            If localOffset4 >= 0 And localOffset4 < 20
               ; V1.034.19: Use unified STORE with j=1 instead of LSTORE
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetStoreOpcodeByFlags(destFlags2)
               ; Set the local variable index (paramOffset) and local flag
               llObjects()\i = localOffset4
               llObjects()\j = 1  ; j=1 means local
            Else
               ; paramOffset not set - use global STORE
               ; V1.035.6: Use rule-based opcode selection
               llObjects()\code = GetStoreOpcodeByFlags(destFlags2)
               llObjects()\i = nVar
            EndIf
         Else
            llObjects()\code = op
         EndIf
      EndIf

   ; V1.022.26: PUSHF+STOREF optimization -> MOVF (float slot-only)
   ElseIf gEmitIntCmd = #ljPUSHF And op = #ljSTOREF
      Protected inTernaryF.b = (llObjects()\flags & #INST_FLAG_TERNARY)
      Protected sourceFlagsF.w = gVarMeta(llObjects()\i)\flags
      Protected destFlagsF.w = gVarMeta(nVar)\flags
      Protected isSourceLocalF.b = IsLocalVar(llObjects()\i)
      Protected isDestLocalF.b = IsLocalVar(nVar)

      If Not inTernaryF And Not ((sourceFlagsF & #C2FLAG_PARAM) Or (destFlagsF & #C2FLAG_PARAM))
         ; V1.035.6: Use rule-based locality computation for PUSHF+STOREF -> MOVF
         Protected localOffsetF.i = gVarMeta(nVar)\paramOffset
         Protected savedSourceF.i = llObjects()\i
         Protected srcOffsetF.i = gVarMeta(savedSourceF)\paramOffset
         Protected outNF.Integer, outJF.Integer, outIF.Integer

         If isDestLocalF And localOffsetF >= 0 And localOffsetF < 20
            ; Local destination with valid paramOffset - use MOVF
            llObjects()\code = #ljMOVF
            ComputeMovLocality(savedSourceF, nVar, isSourceLocalF, #True, srcOffsetF, localOffsetF, @outNF, @outJF, @outIF)
            llObjects()\n = outNF\i
            llObjects()\j = outJF\i
            llObjects()\i = outIF\i
            MarkPreloadable(savedSourceF, nVar)
         ElseIf isDestLocalF
            ; Local destination but invalid paramOffset - fall back to STOREF
            gEmitIntLastOp = AddElement(llObjects())
            llObjects()\code = #ljSTOREF
            llObjects()\i = nVar
         Else
            ; Global destination - use MOVF
            llObjects()\code = #ljMOVF
            ComputeMovLocality(savedSourceF, nVar, isSourceLocalF, #False, srcOffsetF, 0, @outNF, @outJF, @outIF)
            llObjects()\n = outNF\i
            llObjects()\j = outJF\i
            llObjects()\i = outIF\i
            MarkPreloadable(savedSourceF, nVar)
         EndIf
      Else
         ; Keep as PUSHF+STOREF
         gEmitIntLastOp = AddElement(llObjects())
         If isDestLocalF
            ; V1.034.19: Use unified STOREF with j=1 instead of LSTOREF
            Protected localOffsetF2.i = gVarMeta(nVar)\paramOffset
            If localOffsetF2 >= 0 And localOffsetF2 < 20
               llObjects()\code = #ljSTOREF
               llObjects()\i = localOffsetF2
               llObjects()\j = 1  ; j=1 means local
            Else
               llObjects()\code = #ljSTOREF
               llObjects()\i = nVar
            EndIf
         Else
            llObjects()\code = #ljSTOREF
            llObjects()\i = nVar
         EndIf
      EndIf

   ; V1.022.26: PUSHS+STORES optimization -> MOVS (string slot-only)
   ElseIf gEmitIntCmd = #ljPUSHS And op = #ljSTORES
      Protected inTernaryS.b = (llObjects()\flags & #INST_FLAG_TERNARY)
      Protected sourceFlagsS.w = gVarMeta(llObjects()\i)\flags
      Protected destFlagsS.w = gVarMeta(nVar)\flags
      Protected isSourceLocalS.b = IsLocalVar(llObjects()\i)
      Protected isDestLocalS.b = IsLocalVar(nVar)

      If Not inTernaryS And Not ((sourceFlagsS & #C2FLAG_PARAM) Or (destFlagsS & #C2FLAG_PARAM))
         ; V1.035.6: Use rule-based locality computation for PUSHS+STORES -> MOVS
         Protected localOffsetS.i = gVarMeta(nVar)\paramOffset
         Protected savedSourceS.i = llObjects()\i
         Protected srcOffsetS.i = gVarMeta(savedSourceS)\paramOffset
         Protected outNS.Integer, outJS.Integer, outIS.Integer

         If isDestLocalS And localOffsetS >= 0 And localOffsetS < 20
            ; Local destination with valid paramOffset - use MOVS
            llObjects()\code = #ljMOVS
            ComputeMovLocality(savedSourceS, nVar, isSourceLocalS, #True, srcOffsetS, localOffsetS, @outNS, @outJS, @outIS)
            llObjects()\n = outNS\i
            llObjects()\j = outJS\i
            llObjects()\i = outIS\i
            MarkPreloadable(savedSourceS, nVar)
         ElseIf isDestLocalS
            ; Local destination but invalid paramOffset - fall back to STORES
            gEmitIntLastOp = AddElement(llObjects())
            llObjects()\code = #ljSTORES
            llObjects()\i = nVar
         Else
            ; Global destination - use MOVS
            llObjects()\code = #ljMOVS
            ComputeMovLocality(savedSourceS, nVar, isSourceLocalS, #False, srcOffsetS, 0, @outNS, @outJS, @outIS)
            llObjects()\n = outNS\i
            llObjects()\j = outJS\i
            llObjects()\i = outIS\i
            MarkPreloadable(savedSourceS, nVar)
         EndIf
      Else
         ; Keep as PUSHS+STORES
         gEmitIntLastOp = AddElement(llObjects())
         If isDestLocalS
            ; V1.034.19: Use unified STORES with j=1 instead of LSTORES
            Protected localOffsetS2.i = gVarMeta(nVar)\paramOffset
            If localOffsetS2 >= 0 And localOffsetS2 < 20
               llObjects()\code = #ljSTORES
               llObjects()\i = localOffsetS2
               llObjects()\j = 1  ; j=1 means local
            Else
               llObjects()\code = #ljSTORES
               llObjects()\i = nVar
            EndIf
         Else
            llObjects()\code = #ljSTORES
            llObjects()\i = nVar
         EndIf
      EndIf

   Else
      ; Standard emission - check if we should use local opcode
      gEmitIntLastOp = AddElement(llObjects())

      If nVar >= 0 And IsLocalVar(nVar)
         ; V1.034.15: Unified opcode approach - keep base opcode, set j=1 for local
         ; This allows VM to use _SLOT(j, offset) for no-If access pattern
         ; V1.034.31: Debug output to trace local variable detection
         If op = #ljStore Or op = #ljSTORES Or op = #ljSTOREF
            OSDebug("EMITINT LOCAL STORE: nVar=" + Str(nVar) + " name='" + gVarMeta(nVar)\name + "' paramOffset=" + Str(gVarMeta(nVar)\paramOffset) + " gCurrentFunctionName='" + gCurrentFunctionName + "'")
         EndIf
         Select op
            Case #ljFetch, #ljFETCHS, #ljFETCHF
               ; Keep base opcode, mark as local with j=1
               llObjects()\code = op
               llObjects()\j = 1
               llObjects()\i = gVarMeta(nVar)\paramOffset
            Case #ljStore
               ; V1.023.21: Check if source is a pointer (previous opcode produces pointer)
               ; or destination has pointer flag - use PSTORE to preserve ptr/ptrtype metadata
               ; V1.034.19: Use unified PSTORE with j=1 instead of PLSTORE
               If gEmitIntCmd = #ljGETSTRUCTADDR Or gEmitIntCmd = #ljGETADDR Or gEmitIntCmd = #ljGETADDRF Or gEmitIntCmd = #ljGETADDRS Or gEmitIntCmd = #ljGETARRAYADDR Or gEmitIntCmd = #ljGETARRAYADDRF Or gEmitIntCmd = #ljGETARRAYADDRS Or gVarMeta(nVar)\flags & #C2FLAG_POINTER
                  llObjects()\code = #ljPSTORE
                  llObjects()\j = 1
               Else
                  llObjects()\code = op
                  llObjects()\j = 1
               EndIf
               llObjects()\i = gVarMeta(nVar)\paramOffset
            Case #ljSTORES, #ljSTOREF
               llObjects()\code = op
               llObjects()\j = 1
               llObjects()\i = gVarMeta(nVar)\paramOffset
            Default
               llObjects()\code = op
         EndSelect
      Else
         ; V1.034.31: Debug - IsLocalVar returned false
         If op = #ljStore Or op = #ljSTORES Or op = #ljSTOREF
            If nVar >= 0
               OSDebug("EMITINT GLOBAL STORE: nVar=" + Str(nVar) + " name='" + gVarMeta(nVar)\name + "' paramOffset=" + Str(gVarMeta(nVar)\paramOffset) + " gCurrentFunctionName='" + gCurrentFunctionName + "'")
            EndIf
         EndIf
         llObjects()\code = op
      EndIf
   EndIf

   ; Only set llObjects()\i for variable-related opcodes
   ; Skip this for opcodes that don't operate on variables (CALL uses funcId, not varSlot)
   ; Note: For local opcodes (LFETCH, LSTORE, etc.), \i was already set in optimization paths above
   ; V1.023.15: Changed from nVar > -1 to nVar <> -1 to allow negative encoded local pointers (e.g., -4 for PTRSTRUCTSTORE)
   If nVar <> -1
      ; Check if this is an opcode that operates on variables
      currentCode = llObjects()\code
      Select currentCode
         ; V1.034.24: Opcodes with \i already set (PMOV uses n field for locality)
         ; LOCAL opcodes eliminated - now using unified opcodes with j=1
         Case #ljPMOV
            ; Do nothing - \i already contains correct paramOffset from optimization paths

         ; Unified opcodes - need to set \i to variable slot (unless j=1 which means local)
         ; V1.034.24: Added FETCH_STRUCT (now unified with j=1 for locals)
         Case #ljFetch, #ljFETCHS, #ljFETCHF, #ljStore, #ljSTORES, #ljSTOREF, #ljSTORE_STRUCT, #ljFETCH_STRUCT, #ljPSTORE,
              #ljPush, #ljPUSHS, #ljPUSHF, #ljPOP, #ljPOPS, #ljPOPF,
              #ljINC_VAR_PRE, #ljINC_VAR_POST, #ljDEC_VAR_PRE, #ljDEC_VAR_POST,
              #ljMOV, #ljMOVS, #ljMOVF
            ; V1.034.16: Don't overwrite \i if j=1 (local variable - \i already has paramOffset)
            If llObjects()\j = 0
               llObjects()\i = nVar
            EndIf
            ; V1.027.9: Mark global variables as ASSIGNED when stored/modified
            ; This prevents late PRELOAD marking if a non-const store happens before const store
            ; V1.029.84: Include STORE_STRUCT for struct variable assignment tracking
            If currentCode = #ljStore Or currentCode = #ljSTORES Or currentCode = #ljSTOREF Or currentCode = #ljSTORE_STRUCT Or currentCode = #ljPSTORE Or currentCode = #ljINC_VAR_PRE Or currentCode = #ljINC_VAR_POST Or currentCode = #ljDEC_VAR_PRE Or currentCode = #ljDEC_VAR_POST
               If gVarMeta(nVar)\paramOffset = -1  ; Global variable
                  gVarMeta(nVar)\flags = gVarMeta(nVar)\flags | #C2FLAG_ASSIGNED
               EndIf
            EndIf

         Default
            ; Non-variable opcode (CALL, JMP, etc.) - store nVar as-is
            If currentCode >= #ljSTRUCT_FETCH_INT_LOCAL And currentCode <= #ljSTRUCT_STORE_STR_LOCAL
               Debug "EMITINT DEFAULT: code=" + Str(currentCode) + " nVar(paramOffset)=" + Str(nVar)
            EndIf
            llObjects()\i = nVar
      EndSelect
   EndIf

   ; Mark instruction if inside ternary expression
   If gInTernary
      llObjects()\flags = llObjects()\flags | #INST_FLAG_TERNARY
   EndIf

   gEmitIntCmd = llObjects()\code
EndProcedure

;- ============================================================================
;- End of c2-codegen-emit.pbi
;- ============================================================================

; IDE Options = PureBasic 6.10 (Windows - x64)
; CursorPosition = 1
; Folding = --
