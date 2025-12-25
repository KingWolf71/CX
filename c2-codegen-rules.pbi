; c2-codegen-rules.pbi
; Rule-based type dispatch tables for code generation
; V1.035.0: Initial creation - consolidates duplicate type-dispatch patterns
;
; Purpose: Replace 30+ duplicate if/elseif type-checking patterns with
; table-driven opcode selection.

;- ============================================================================
;- Type Dispatch Rule Structure
;- ============================================================================

; Maps a base operation to its typed variants (INT/FLOAT/STR/POINTER)
Structure stTypeDispatchRule
   opcodeInt.i          ; Opcode for INT variant
   opcodeFloat.i        ; Opcode for FLOAT variant
   opcodeStr.i          ; Opcode for STR variant
   opcodePointer.i      ; Opcode for POINTER variant (0 = use INT)
EndStructure

; Global rule map: base opcode (as string key) -> typed variants
Global NewMap mapOpcodeTypeVariants.stTypeDispatchRule()

; Flag to track if rules are initialized
Global gRulesInitialized.b = #False

;- ============================================================================
;- Locality Encoding
;- ============================================================================

; Encodes source/dest locality into n field for MOV optimization
; This replaces ~200 lines of scattered duplicate logic
;
; Returns:
;   0 = GG (Global source, Global dest)
;   1 = LG (Local source, Global dest)
;   2 = GL (Global source, Local dest)
;   3 = LL (Local source, Local dest)
;
Procedure.i EncodeLocality(srcIsLocal.b, dstIsLocal.b)
   If srcIsLocal
      If dstIsLocal
         ProcedureReturn 3  ; LL
      Else
         ProcedureReturn 1  ; LG
      EndIf
   Else
      If dstIsLocal
         ProcedureReturn 2  ; GL
      Else
         ProcedureReturn 0  ; GG
      EndIf
   EndIf
EndProcedure

; Decode locality n value back to source/dest flags
Procedure DecodeLocality(n.i, *srcIsLocal.Integer, *dstIsLocal.Integer)
   *srcIsLocal\i = Bool(n & 1)    ; Bit 0 = source local
   *dstIsLocal\i = Bool(n & 2)    ; Bit 1 = dest local
EndProcedure

; Check if paramOffset is valid for local variable access (0-19)
Procedure.b IsValidLocalOffset(offset.i)
   ProcedureReturn Bool(offset >= 0 And offset < 20)
EndProcedure

;- ============================================================================
;- Rule Table Initialization
;- ============================================================================

; Helper to add a type dispatch rule
Procedure AddTypeRule(baseOpcode.i, opcInt.i, opcFloat.i, opcStr.i, opcPointer.i = 0)
   Protected key.s = Str(baseOpcode)

   AddMapElement(mapOpcodeTypeVariants(), key)
   mapOpcodeTypeVariants()\opcodeInt = opcInt
   mapOpcodeTypeVariants()\opcodeFloat = opcFloat
   mapOpcodeTypeVariants()\opcodeStr = opcStr
   mapOpcodeTypeVariants()\opcodePointer = opcPointer
EndProcedure

; Initialize all type dispatch rules
; Call this once at compiler startup
Procedure InitTypeDispatchRules()
   If gRulesInitialized
      ProcedureReturn
   EndIf

   ClearMap(mapOpcodeTypeVariants())

   ;- FETCH variants (load variable to stack)
   AddTypeRule(#ljFetch, #ljFetch, #ljFETCHF, #ljFETCHS)

   ;- STORE variants (pop stack to variable)
   AddTypeRule(#ljStore, #ljStore, #ljSTOREF, #ljSTORES, #ljPSTORE)

   ;- PUSH variants (push constant/slot to stack)
   AddTypeRule(#ljPush, #ljPush, #ljPUSHF, #ljPUSHS)

   ;- MOV variants (direct variable-to-variable copy)
   AddTypeRule(#ljMOV, #ljMOV, #ljMOVF, #ljMOVS, #ljPMOV)

   ;- RETURN variants (return value from function)
   AddTypeRule(#ljreturn, #ljreturn, #ljreturnF, #ljreturnS)

   ;- GETADDR variants (address-of operator &var)
   AddTypeRule(#ljGETADDR, #ljGETADDR, #ljGETADDRF, #ljGETADDRS)

   ;- STRUCT_FETCH variants (global struct field access)
   AddTypeRule(#ljSTRUCT_FETCH_INT, #ljSTRUCT_FETCH_INT, #ljSTRUCT_FETCH_FLOAT, #ljSTRUCT_FETCH_STR)

   ;- STRUCT_FETCH_LOCAL variants (local struct field access)
   AddTypeRule(#ljSTRUCT_FETCH_INT_LOCAL, #ljSTRUCT_FETCH_INT_LOCAL, #ljSTRUCT_FETCH_FLOAT_LOCAL, #ljSTRUCT_FETCH_STR_LOCAL)

   ;- STRUCT_STORE variants (global struct field store)
   AddTypeRule(#ljSTRUCT_STORE_INT, #ljSTRUCT_STORE_INT, #ljSTRUCT_STORE_FLOAT, #ljSTRUCT_STORE_STR)

   ;- STRUCT_STORE_LOCAL variants (local struct field store)
   AddTypeRule(#ljSTRUCT_STORE_INT_LOCAL, #ljSTRUCT_STORE_INT_LOCAL, #ljSTRUCT_STORE_FLOAT_LOCAL, #ljSTRUCT_STORE_STR_LOCAL)

   gRulesInitialized = #True
EndProcedure

;- ============================================================================
;- Type Dispatch Helper Functions
;- ============================================================================

; Returns the typed variant of baseOpcode based on typeFlags
; This replaces ~300 lines of repeated if/elseif patterns
;
; Parameters:
;   baseOpcode - The base opcode (e.g., #ljFetch, #ljStore, #ljMOV)
;   typeFlags  - Type flags from variable (#C2FLAG_INT, #C2FLAG_FLOAT, #C2FLAG_STR)
;   isPointer  - True if this is a pointer operation
;
; Returns:
;   The appropriate typed opcode, or baseOpcode if no mapping exists
;
Procedure.i GetTypedOpcode(baseOpcode.i, typeFlags.w, isPointer.b = #False)
   Protected key.s = Str(baseOpcode)

   ; Ensure rules are initialized
   If Not gRulesInitialized
      InitTypeDispatchRules()
   EndIf

   If FindMapElement(mapOpcodeTypeVariants(), key)
      ; Pointer operations have priority if available
      If isPointer And mapOpcodeTypeVariants()\opcodePointer
         ProcedureReturn mapOpcodeTypeVariants()\opcodePointer
      EndIf

      ; Type-based selection
      If typeFlags & #C2FLAG_FLOAT
         ProcedureReturn mapOpcodeTypeVariants()\opcodeFloat
      ElseIf typeFlags & #C2FLAG_STR
         ProcedureReturn mapOpcodeTypeVariants()\opcodeStr
      Else
         ; Default to INT (includes #C2FLAG_INT and no type flag)
         ProcedureReturn mapOpcodeTypeVariants()\opcodeInt
      EndIf
   EndIf

   ; No mapping found - return base opcode unchanged
   ProcedureReturn baseOpcode
EndProcedure

; Get typed opcode from slot index (looks up type from gVarMeta)
Procedure.i GetTypedOpcodeForSlot(baseOpcode.i, slot.i)
   Protected typeFlags.w, isPointer.b

   If slot >= 0 And slot < gnLastVariable
      typeFlags = gVarMeta(slot)\flags & #C2FLAG_TYPE
      isPointer = Bool(gVarMeta(slot)\flags & #C2FLAG_POINTER)
      ProcedureReturn GetTypedOpcode(baseOpcode, typeFlags, isPointer)
   EndIf

   ; Invalid slot - return base opcode
   ProcedureReturn baseOpcode
EndProcedure

; Get struct field opcode based on locality and field type
; This consolidates the struct fetch/store opcode selection
Procedure.i GetStructFieldOpcode(isStore.b, isLocal.b, fieldType.w)
   Protected baseOpcode.i

   If isStore
      If isLocal
         baseOpcode = #ljSTRUCT_STORE_INT_LOCAL
      Else
         baseOpcode = #ljSTRUCT_STORE_INT
      EndIf
   Else
      If isLocal
         baseOpcode = #ljSTRUCT_FETCH_INT_LOCAL
      Else
         baseOpcode = #ljSTRUCT_FETCH_INT
      EndIf
   EndIf

   ProcedureReturn GetTypedOpcode(baseOpcode, fieldType)
EndProcedure

;- ============================================================================
;- Type Detection Helpers
;- ============================================================================

; Get human-readable type name from flags
Procedure.s GetTypeNameFromFlags(typeFlags.w)
   If typeFlags & #C2FLAG_FLOAT
      ProcedureReturn "float"
   ElseIf typeFlags & #C2FLAG_STR
      ProcedureReturn "string"
   ElseIf typeFlags & #C2FLAG_INT
      ProcedureReturn "int"
   Else
      ProcedureReturn "unknown"
   EndIf
EndProcedure

; Check if type flags indicate a pointer
Procedure.b IsPointerType(typeFlags.w)
   ProcedureReturn Bool(typeFlags & #C2FLAG_POINTER)
EndProcedure

; Check if type flags indicate a struct
Procedure.b IsStructType(typeFlags.w)
   ProcedureReturn Bool(typeFlags & #C2FLAG_STRUCT)
EndProcedure

; Check if type flags indicate an array
Procedure.b IsArrayType(typeFlags.w)
   ProcedureReturn Bool(typeFlags & #C2FLAG_ARRAY)
EndProcedure

;- ============================================================================
;- Optimization Pattern Helpers
;- ============================================================================

; Check if opcode is a PUSH variant (for PUSH+STORE → MOV optimization)
Procedure.b IsPushOpcode(opcode.i)
   Select opcode
      Case #ljPush, #ljPUSHF, #ljPUSHS
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Check if opcode is a FETCH variant (for FETCH+STORE → MOV optimization)
Procedure.b IsFetchOpcode(opcode.i)
   Select opcode
      Case #ljFetch, #ljFETCHF, #ljFETCHS
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Check if opcode is a STORE variant
Procedure.b IsStoreOpcode(opcode.i)
   Select opcode
      Case #ljStore, #ljSTOREF, #ljSTORES, #ljPSTORE
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Check if opcode is a MOV variant
Procedure.b IsMovOpcode(opcode.i)
   Select opcode
      Case #ljMOV, #ljMOVF, #ljMOVS, #ljPMOV
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Get the MOV opcode that matches a PUSH or FETCH opcode's type
Procedure.i GetMatchingMovOpcode(pushOrFetchOpcode.i, isPointer.b = #False)
   If isPointer
      ProcedureReturn #ljPMOV
   EndIf

   Select pushOrFetchOpcode
      Case #ljPUSHF, #ljFETCHF
         ProcedureReturn #ljMOVF
      Case #ljPUSHS, #ljFETCHS
         ProcedureReturn #ljMOVS
      Default
         ProcedureReturn #ljMOV
   EndSelect
EndProcedure

; Get the STORE opcode that matches a PUSH or FETCH opcode's type
Procedure.i GetMatchingStoreOpcode(pushOrFetchOpcode.i, isPointer.b = #False)
   If isPointer
      ProcedureReturn #ljPSTORE
   EndIf

   Select pushOrFetchOpcode
      Case #ljPUSHF, #ljFETCHF
         ProcedureReturn #ljSTOREF
      Case #ljPUSHS, #ljFETCHS
         ProcedureReturn #ljSTORES
      Default
         ProcedureReturn #ljStore
   EndSelect
EndProcedure

;- ============================================================================
;- MOV Optimization Result Structure
;- ============================================================================

; Result structure for TryOptimizeMov function
Structure stMovOptResult
   canOptimize.b        ; True if PUSH/FETCH+STORE can be optimized to MOV
   movOpcode.i          ; The MOV opcode to use (MOV/MOVF/MOVS/PMOV)
   locality.i           ; Encoded locality: 0=GG, 1=LG, 2=GL, 3=LL
   srcValue.i           ; Source: paramOffset (if local) or slot (if global)
   dstValue.i           ; Dest: paramOffset (if local) or slot (if global)
   srcIsLocal.b         ; True if source is local variable
   dstIsLocal.b         ; True if destination is local variable
   fallbackOpcode.i     ; STORE opcode to use if optimization fails
EndStructure

;- ============================================================================
;- Unified MOV Optimization Helper
;- ============================================================================

; Compute MOV optimization parameters for PUSH/FETCH+STORE patterns
; This consolidates ~450 lines of duplicate logic in EmitInt
;
; Parameters:
;   srcSlot         - Source variable slot (from PUSH/FETCH \i)
;   dstSlot         - Destination variable slot
;   srcFlags        - Source variable flags
;   dstFlags        - Destination variable flags
;   srcIsLocal      - True if source is local variable
;   dstIsLocal      - True if destination is local variable
;   prevOpcode      - Previous opcode (PUSH/PUSHF/PUSHS/FETCH/FETCHF/FETCHS)
;   inTernary       - True if inside ternary expression
;   *result         - Output structure for optimization result
;
; Returns:
;   True if optimization can be applied, False otherwise
;
Procedure.b ComputeMovOptimization(srcSlot.i, dstSlot.i, srcFlags.w, dstFlags.w, srcIsLocal.b, dstIsLocal.b, srcParamOffset.i, dstParamOffset.i, prevOpcode.i, inTernary.b, *result.stMovOptResult)
   ; Initialize result
   *result\canOptimize = #False
   *result\movOpcode = #ljMOV
   *result\locality = 0
   *result\srcValue = srcSlot
   *result\dstValue = dstSlot
   *result\srcIsLocal = srcIsLocal
   *result\dstIsLocal = dstIsLocal
   *result\fallbackOpcode = GetMatchingStoreOpcode(prevOpcode, Bool(dstFlags & #C2FLAG_POINTER))

   ; Cannot optimize inside ternary expressions
   If inTernary
      ProcedureReturn #False
   EndIf

   ; Cannot optimize if either is a parameter
   If (srcFlags & #C2FLAG_PARAM) Or (dstFlags & #C2FLAG_PARAM)
      ProcedureReturn #False
   EndIf

   ; Determine MOV opcode based on type
   *result\movOpcode = GetMatchingMovOpcode(prevOpcode, Bool(dstFlags & #C2FLAG_POINTER))

   ; Compute locality encoding and values
   If dstIsLocal
      ; Destination is local - validate paramOffset
      If dstParamOffset < 0 Or dstParamOffset >= 20
         ; Invalid paramOffset - cannot optimize
         ProcedureReturn #False
      EndIf

      *result\dstValue = dstParamOffset

      If srcIsLocal And srcParamOffset >= 0 And srcParamOffset < 20
         ; Both local: n=3 (LL)
         *result\locality = 3
         *result\srcValue = srcParamOffset
      Else
         ; Global source, local dest: n=2 (GL)
         *result\locality = 2
         *result\srcValue = srcSlot
      EndIf
   Else
      ; Destination is global
      *result\dstValue = dstSlot

      If srcIsLocal And srcParamOffset >= 0 And srcParamOffset < 20
         ; Local source, global dest: n=1 (LG)
         *result\locality = 1
         *result\srcValue = srcParamOffset
      Else
         ; Both global: n=0 (GG)
         *result\locality = 0
         *result\srcValue = srcSlot
      EndIf
   EndIf

   *result\canOptimize = #True
   ProcedureReturn #True
EndProcedure

;- ============================================================================
;- Struct Field Opcode Selection
;- ============================================================================

; Get struct field lookup type from byte offset
; Walks nested struct chain to find primitive field type
;
; Parameters:
;   structType      - Base struct type name
;   byteOffset      - Byte offset within struct
;
; Returns:
;   Field type flags (#C2FLAG_INT, #C2FLAG_FLOAT, #C2FLAG_STR) or 0 if not found
;
Procedure.w LookupStructFieldType(structType.s, byteOffset.i)
   Protected lookupType.s = structType
   Protected lookupOffset.i = byteOffset / 8  ; Convert byte offset to field index
   Protected fieldType.w = 0
   Protected found.b = #False

   ; Walk nested struct chain until we find a primitive field
   While Not found And lookupType <> ""
      If FindMapElement(mapStructDefs(), lookupType)
         Protected accumOffset.i = 0
         ForEach mapStructDefs()\fields()
            Protected fieldSize.i = 1  ; Default size for primitives

            ; Check for array fields
            If mapStructDefs()\fields()\isArray And mapStructDefs()\fields()\arraySize > 1
               fieldSize = mapStructDefs()\fields()\arraySize
            ElseIf mapStructDefs()\fields()\structType <> ""
               ; Nested struct - get its total size
               Protected nestedType.s = mapStructDefs()\fields()\structType
               If FindMapElement(mapStructDefs(), nestedType)
                  fieldSize = mapStructDefs()\totalSize
               EndIf
               FindMapElement(mapStructDefs(), lookupType)  ; Restore position
            EndIf

            ; Check if target offset falls within this field
            If lookupOffset >= accumOffset And lookupOffset < accumOffset + fieldSize
               If mapStructDefs()\fields()\structType <> ""
                  ; Nested struct - recurse into it
                  lookupType = mapStructDefs()\fields()\structType
                  lookupOffset = lookupOffset - accumOffset
                  Break  ; Continue outer while loop with nested type
               Else
                  ; Primitive field found
                  fieldType = mapStructDefs()\fields()\fieldType
                  found = #True
                  Break
               EndIf
            EndIf
            accumOffset + fieldSize
         Next
         If ListIndex(mapStructDefs()\fields()) = -1
            Break  ; Field not found, exit
         EndIf
      Else
         Break  ; Struct type not found
      EndIf
   Wend

   ProcedureReturn fieldType
EndProcedure

;- ============================================================================
;- Peephole Optimization Rules
;- ============================================================================

; Rule types for peephole optimization
Enumeration
   #PEEPHOLE_DEAD_CODE       ; PUSH+POP elimination
   #PEEPHOLE_SELF_ASSIGN     ; FETCH x + STORE x elimination
   #PEEPHOLE_MOV_FUSION      ; FETCH+STORE → MOV
   #PEEPHOLE_CONST_FOLD      ; Constant folding
   #PEEPHOLE_IDENTITY        ; Identity operations (+0, *1, /1)
   #PEEPHOLE_DOUBLE_NEG      ; NOT+NOT, NEGATE+NEGATE
   #PEEPHOLE_CMP_FLIP        ; CMP+NOT → flipped CMP
   #PEEPHOLE_JUMP_OPT        ; Jump to next, JZ on constant
   #PEEPHOLE_INC_DEC         ; INC/DEC+POP → simple INC/DEC
EndEnumeration

; Structure for peephole pattern matching
Structure stPeepholePattern
   patternLen.i              ; Number of instructions in pattern
   opcodes.l[4]              ; Pattern opcodes (0=any, -1=end marker)
   resultLen.i               ; Number of result instructions
   resultOpcodes.l[4]        ; Result opcodes (after optimization)
   ruleType.i                ; Rule type from enumeration
EndStructure

; Map of comparison flip rules: original opcode -> flipped opcode
Global NewMap mapCompareFlip.i()

; Map of dead code patterns: opcode -> can be eliminated when followed by POP
Global NewMap mapDeadCodeOpcodes.b()

; Map of identity operations: (opcode << 16) | constant_value -> is_identity
; For example: (#ljADD << 16) | 0 -> True (x+0 = x)
Global NewMap mapIdentityOps.b()

; Initialize comparison flip rules
Procedure InitCompareFlipRules()
   ; Less → GreaterEqual when followed by NOT
   mapCompareFlip(Str(#ljLESS)) = #ljGreaterEqual
   mapCompareFlip(Str(#ljGREATER)) = #ljLESSEQUAL
   mapCompareFlip(Str(#ljLESSEQUAL)) = #ljGREATER
   mapCompareFlip(Str(#ljGreaterEqual)) = #ljLESS
   mapCompareFlip(Str(#ljEQUAL)) = #ljNOTEQUAL
   mapCompareFlip(Str(#ljNOTEQUAL)) = #ljEQUAL
EndProcedure

; Initialize dead code pattern opcodes
Procedure InitDeadCodeRules()
   ; These opcodes can be eliminated when followed by POP (push then pop = nothing)
   mapDeadCodeOpcodes(Str(#ljFetch)) = #True
   mapDeadCodeOpcodes(Str(#ljFETCHF)) = #True
   mapDeadCodeOpcodes(Str(#ljFETCHS)) = #True
   mapDeadCodeOpcodes(Str(#ljPush)) = #True
   mapDeadCodeOpcodes(Str(#ljPUSHF)) = #True
   mapDeadCodeOpcodes(Str(#ljPUSHS)) = #True
   mapDeadCodeOpcodes(Str(#ljLFETCH)) = #True
   mapDeadCodeOpcodes(Str(#ljLFETCHF)) = #True
   mapDeadCodeOpcodes(Str(#ljLFETCHS)) = #True
   mapDeadCodeOpcodes(Str(#ljPUSH_IMM)) = #True
EndProcedure

; Initialize identity operation rules
Procedure InitIdentityRules()
   ; x + 0 = x, x - 0 = x
   mapIdentityOps(Str((#ljADD << 16) | 0)) = #True
   mapIdentityOps(Str((#ljSUBTRACT << 16) | 0)) = #True

   ; x * 1 = x, x / 1 = x
   mapIdentityOps(Str((#ljMULTIPLY << 16) | 1)) = #True
   mapIdentityOps(Str((#ljDIVIDE << 16) | 1)) = #True

   ; Float identity operations (use separate keys for float)
   ; Note: Float 0.0 and 1.0 are compared by slot value, not bit pattern
EndProcedure

; Check if opcode can be eliminated in PUSH+POP pattern
Procedure.b IsDeadCodeOpcode(opcode.i)
   ProcedureReturn Bool(FindMapElement(mapDeadCodeOpcodes(), Str(opcode)))
EndProcedure

; Get flipped comparison opcode (for CMP+NOT optimization)
Procedure.i GetFlippedCompare(opcode.i)
   If FindMapElement(mapCompareFlip(), Str(opcode))
      ProcedureReturn mapCompareFlip()
   EndIf
   ProcedureReturn 0  ; No flip available
EndProcedure

; Check if this is an identity operation (can be eliminated)
Procedure.b IsIdentityOp(opcode.i, constValue.i)
   ProcedureReturn Bool(FindMapElement(mapIdentityOps(), Str((opcode << 16) | constValue)))
EndProcedure

; Check if opcode is a POP variant
Procedure.b IsPopOpcode(opcode.i)
   Select opcode
      Case #ljPOP, #ljPOPS, #ljPOPF, #ljDROP
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Check if opcode is an increment variant
Procedure.b IsIncrementOpcode(opcode.i)
   Select opcode
      Case #ljINC_VAR_PRE, #ljINC_VAR_POST, #ljLINC_VAR_PRE, #ljLINC_VAR_POST
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Check if opcode is a decrement variant
Procedure.b IsDecrementOpcode(opcode.i)
   Select opcode
      Case #ljDEC_VAR_PRE, #ljDEC_VAR_POST, #ljLDEC_VAR_PRE, #ljLDEC_VAR_POST
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Get simple increment opcode (removes pre/post and POP behavior)
Procedure.i GetSimpleIncrementOpcode(opcode.i)
   Select opcode
      Case #ljINC_VAR_PRE, #ljINC_VAR_POST
         ProcedureReturn #ljINC_VAR
      Case #ljLINC_VAR_PRE, #ljLINC_VAR_POST
         ProcedureReturn #ljLINC_VAR
   EndSelect
   ProcedureReturn opcode
EndProcedure

; Get simple decrement opcode (removes pre/post and POP behavior)
Procedure.i GetSimpleDecrementOpcode(opcode.i)
   Select opcode
      Case #ljDEC_VAR_PRE, #ljDEC_VAR_POST
         ProcedureReturn #ljDEC_VAR
      Case #ljLDEC_VAR_PRE, #ljLDEC_VAR_POST
         ProcedureReturn #ljLDEC_VAR
   EndSelect
   ProcedureReturn opcode
EndProcedure

; Check if opcode is a local FETCH variant
Procedure.b IsLocalFetchOpcode(opcode.i)
   Select opcode
      Case #ljLFETCH, #ljLFETCHF, #ljLFETCHS
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Check if opcode is a local STORE variant
Procedure.b IsLocalStoreOpcode(opcode.i)
   Select opcode
      Case #ljLSTORE, #ljLSTOREF, #ljLSTORES
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

; Get local MOV opcode matching type
Procedure.i GetLocalMovOpcode(fetchOpcode.i)
   Select fetchOpcode
      Case #ljLFETCHF
         ProcedureReturn #ljLLMOVF
      Case #ljLFETCHS
         ProcedureReturn #ljLLMOVS
      Default
         ProcedureReturn #ljLLMOV
   EndSelect
EndProcedure

;- ============================================================================
;- Compound Assignment Opcode Tables
;- ============================================================================

; Map arithmetic opcode to compound assignment variant
Global NewMap mapCompoundAssignInt.i()
Global NewMap mapCompoundAssignFloat.i()

Procedure InitCompoundAssignRules()
   ; Integer compound assignments
   mapCompoundAssignInt(Str(#ljADD)) = #ljADD_ASSIGN_VAR
   mapCompoundAssignInt(Str(#ljSUBTRACT)) = #ljSUB_ASSIGN_VAR
   mapCompoundAssignInt(Str(#ljMULTIPLY)) = #ljMUL_ASSIGN_VAR
   mapCompoundAssignInt(Str(#ljDIVIDE)) = #ljDIV_ASSIGN_VAR
   mapCompoundAssignInt(Str(#ljMOD)) = #ljMOD_ASSIGN_VAR

   ; Float compound assignments
   mapCompoundAssignFloat(Str(#ljFLOATADD)) = #ljFLOATADD_ASSIGN_VAR
   mapCompoundAssignFloat(Str(#ljFLOATSUB)) = #ljFLOATSUB_ASSIGN_VAR
   mapCompoundAssignFloat(Str(#ljFLOATMUL)) = #ljFLOATMUL_ASSIGN_VAR
   mapCompoundAssignFloat(Str(#ljFLOATDIV)) = #ljFLOATDIV_ASSIGN_VAR
EndProcedure

; Get compound assignment opcode for given arithmetic opcode
Procedure.i GetCompoundAssignOpcode(arithOpcode.i)
   ; Try integer first
   If FindMapElement(mapCompoundAssignInt(), Str(arithOpcode))
      ProcedureReturn mapCompoundAssignInt()
   EndIf
   ; Try float
   If FindMapElement(mapCompoundAssignFloat(), Str(arithOpcode))
      ProcedureReturn mapCompoundAssignFloat()
   EndIf
   ProcedureReturn 0  ; No compound variant
EndProcedure

; Check if opcode is an arithmetic operation that can be compounded
Procedure.b IsCompoundableArithOp(opcode.i)
   If FindMapElement(mapCompoundAssignInt(), Str(opcode))
      ProcedureReturn #True
   EndIf
   If FindMapElement(mapCompoundAssignFloat(), Str(opcode))
      ProcedureReturn #True
   EndIf
   ProcedureReturn #False
EndProcedure

;- ============================================================================
;- Master Peephole Rule Initialization
;- ============================================================================

; Initialize all peephole optimization rules
; Call once at compiler startup after InitTypeDispatchRules()
Procedure InitPeepholeRules()
   InitCompareFlipRules()
   InitDeadCodeRules()
   InitIdentityRules()
   InitCompoundAssignRules()
EndProcedure

;- ============================================================================
;- FETCH+STORE Fusion Tables
;- ============================================================================

; Structure for tracking FETCH+STORE fusion opportunities
Structure stFetchStoreFusion
   fetchOpcode.i     ; FETCH/FETCHF/FETCHS/LFETCH/LFETCHF/LFETCHS/PFETCH
   storeOpcode.i     ; Matching STORE variant
   movOpcode.i       ; Resulting MOV variant
   localMov.i        ; Local-to-local MOV variant (if applicable)
EndStructure

; Map FETCH opcode to matching store and MOV opcodes
Global NewMap mapFetchStoreFusion.stFetchStoreFusion()

Procedure InitFetchStoreFusion()
   ; Global FETCH variants
   AddMapElement(mapFetchStoreFusion(), Str(#ljFetch))
   mapFetchStoreFusion()\fetchOpcode = #ljFetch
   mapFetchStoreFusion()\storeOpcode = #ljStore
   mapFetchStoreFusion()\movOpcode = #ljMOV
   mapFetchStoreFusion()\localMov = #ljLLMOV

   AddMapElement(mapFetchStoreFusion(), Str(#ljFETCHF))
   mapFetchStoreFusion()\fetchOpcode = #ljFETCHF
   mapFetchStoreFusion()\storeOpcode = #ljSTOREF
   mapFetchStoreFusion()\movOpcode = #ljMOVF
   mapFetchStoreFusion()\localMov = #ljLLMOVF

   AddMapElement(mapFetchStoreFusion(), Str(#ljFETCHS))
   mapFetchStoreFusion()\fetchOpcode = #ljFETCHS
   mapFetchStoreFusion()\storeOpcode = #ljSTORES
   mapFetchStoreFusion()\movOpcode = #ljMOVS
   mapFetchStoreFusion()\localMov = #ljLLMOVS

   ; Local FETCH variants
   AddMapElement(mapFetchStoreFusion(), Str(#ljLFETCH))
   mapFetchStoreFusion()\fetchOpcode = #ljLFETCH
   mapFetchStoreFusion()\storeOpcode = #ljLSTORE
   mapFetchStoreFusion()\movOpcode = #ljMOV
   mapFetchStoreFusion()\localMov = #ljLLMOV

   AddMapElement(mapFetchStoreFusion(), Str(#ljLFETCHF))
   mapFetchStoreFusion()\fetchOpcode = #ljLFETCHF
   mapFetchStoreFusion()\storeOpcode = #ljLSTOREF
   mapFetchStoreFusion()\movOpcode = #ljMOVF
   mapFetchStoreFusion()\localMov = #ljLLMOVF

   AddMapElement(mapFetchStoreFusion(), Str(#ljLFETCHS))
   mapFetchStoreFusion()\fetchOpcode = #ljLFETCHS
   mapFetchStoreFusion()\storeOpcode = #ljLSTORES
   mapFetchStoreFusion()\movOpcode = #ljMOVS
   mapFetchStoreFusion()\localMov = #ljLLMOVS

   ; Pointer FETCH
   AddMapElement(mapFetchStoreFusion(), Str(#ljPFETCH))
   mapFetchStoreFusion()\fetchOpcode = #ljPFETCH
   mapFetchStoreFusion()\storeOpcode = #ljPSTORE
   mapFetchStoreFusion()\movOpcode = #ljPMOV
   mapFetchStoreFusion()\localMov = #ljPMOV
EndProcedure

; Get fusion info for a FETCH opcode
Procedure.b GetFetchStoreFusion(fetchOpcode.i, *fusion.stFetchStoreFusion)
   If FindMapElement(mapFetchStoreFusion(), Str(fetchOpcode))
      CopyStructure(@mapFetchStoreFusion(), *fusion, stFetchStoreFusion)
      ProcedureReturn #True
   EndIf
   ProcedureReturn #False
EndProcedure

;- ============================================================================
;- Master Rule Initialization
;- ============================================================================

; Initialize all optimization rules
; Call this once at compiler startup
Procedure InitAllOptimizationRules()
   InitTypeDispatchRules()
   InitPeepholeRules()
   InitFetchStoreFusion()
EndProcedure

;- ============================================================================
;- DUP Optimization Helpers
;- ============================================================================

; Get typed DUP opcode matching FETCH opcode type
Procedure.i GetDupOpcodeForFetch(fetchOpcode.i)
   Select fetchOpcode
      Case #ljFETCHF, #ljLFETCHF
         ProcedureReturn #ljDUP_F
      Case #ljFETCHS, #ljLFETCHS
         ProcedureReturn #ljDUP_S
      Default  ; #ljFetch, #ljLFETCH, #ljPFETCH
         ProcedureReturn #ljDUP_I
   EndSelect
EndProcedure

; Check if two FETCH instructions access the same variable
; Used for FETCH x + FETCH x → FETCH x + DUP optimization
Procedure.b AreSameFetchTarget(opcode1.i, slot1.i, j1.i, opcode2.i, slot2.i, j2.i)
   ; Must be same opcode type (both FETCH, both FETCHF, etc.)
   If opcode1 <> opcode2
      ProcedureReturn #False
   EndIf

   ; Must be same slot/offset
   If slot1 <> slot2
      ProcedureReturn #False
   EndIf

   ; Must have same locality (both local or both global)
   If j1 <> j2
      ProcedureReturn #False
   EndIf

   ProcedureReturn #True
EndProcedure

; Check if opcode is a global FETCH variant
Procedure.b IsGlobalFetchOpcode(opcode.i)
   Select opcode
      Case #ljFetch, #ljFETCHF, #ljFETCHS
         ProcedureReturn #True
   EndSelect
   ProcedureReturn #False
EndProcedure

;- ============================================================================
;- Direct Type-Flag Opcode Helpers (for EmitInt refactoring)
;- ============================================================================

; Get STORE opcode directly from type flags
; This replaces the repeated if/elseif patterns in EmitInt
;
; Parameters:
;   typeFlags - Variable flags (#C2FLAG_STR, #C2FLAG_FLOAT, #C2FLAG_POINTER, etc.)
;
; Returns:
;   #ljSTORES for strings, #ljSTOREF for floats, #ljPSTORE for pointers, #ljSTORE otherwise
;
Procedure.i GetStoreOpcodeByFlags(typeFlags.w)
   If typeFlags & #C2FLAG_STR
      ProcedureReturn #ljSTORES
   ElseIf typeFlags & #C2FLAG_FLOAT
      ProcedureReturn #ljSTOREF
   ElseIf typeFlags & #C2FLAG_POINTER
      ProcedureReturn #ljPSTORE
   Else
      ProcedureReturn #ljStore
   EndIf
EndProcedure

; Get MOV opcode directly from type flags
; This replaces the repeated if/elseif patterns in EmitInt
;
; Parameters:
;   sourceFlags - Source variable flags
;   destFlags   - Destination variable flags
;
; Returns:
;   #ljMOVS for strings, #ljMOVF for floats, #ljPMOV for pointers, #ljMOV otherwise
;
; Note: Priority is source type for STR/FLOAT, dest type for POINTER
;
Procedure.i GetMovOpcodeByFlags(sourceFlags.w, destFlags.w)
   If sourceFlags & #C2FLAG_STR
      ProcedureReturn #ljMOVS
   ElseIf sourceFlags & #C2FLAG_FLOAT
      ProcedureReturn #ljMOVF
   ElseIf destFlags & #C2FLAG_POINTER
      ProcedureReturn #ljPMOV
   Else
      ProcedureReturn #ljMOV
   EndIf
EndProcedure

; Compute MOV locality parameters from source and destination info
; Returns the n field value (0=GG, 1=LG, 2=GL, 3=LL), j field (source value), and i field (dest value)
;
; Parameters:
;   srcSlot         - Source variable slot (original)
;   dstSlot         - Destination variable slot (original)
;   srcIsLocal      - True if source is local variable
;   dstIsLocal      - True if destination is local variable
;   srcParamOffset  - Source variable paramOffset (if local)
;   dstParamOffset  - Destination variable paramOffset (if local)
;   *outN           - Output: n field value (locality encoding)
;   *outJ           - Output: j field value (source slot/paramOffset)
;   *outI           - Output: i field value (dest slot/paramOffset)
;
; Returns: True if MOV optimization is valid, False if should fallback to STORE
;
Procedure.b ComputeMovLocality(srcSlot.i, dstSlot.i, srcIsLocal.b, dstIsLocal.b, srcParamOffset.i, dstParamOffset.i, *outN.Integer, *outJ.Integer, *outI.Integer)
   ; Default values
   *outN\i = 0
   *outJ\i = srcSlot
   *outI\i = dstSlot

   If dstIsLocal
      ; Destination is local - validate paramOffset
      If dstParamOffset < 0 Or dstParamOffset >= 20
         ProcedureReturn #False  ; Invalid local offset, cannot optimize to MOV
      EndIf

      *outI\i = dstParamOffset

      If srcIsLocal And srcParamOffset >= 0 And srcParamOffset < 20
         ; Both local: n=3 (LL)
         *outN\i = 3
         *outJ\i = srcParamOffset
      Else
         ; Global source, local dest: n=2 (GL)
         *outN\i = 2
         *outJ\i = srcSlot
      EndIf
   Else
      ; Destination is global
      *outI\i = dstSlot

      If srcIsLocal And srcParamOffset >= 0 And srcParamOffset < 20
         ; Local source, global dest: n=1 (LG)
         *outN\i = 1
         *outJ\i = srcParamOffset
      Else
         ; Both global: n=0 (GG)
         *outN\i = 0
         *outJ\i = srcSlot
      EndIf
   EndIf

   ProcedureReturn #True
EndProcedure

;- ============================================================================
;- End of c2-codegen-rules.pbi
;- ============================================================================

; IDE Options = PureBasic 6.10 (Windows - x64)
; CursorPosition = 1
; Folding = --
