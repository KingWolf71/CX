; c2-codegen-struct.pbi
; Unified struct field resolution for code generation
; V1.035.0: Initial creation - consolidates duplicate struct field handling
;
; Purpose: Replace ~260 lines of duplicated DOT and backslash notation handling
; with a single unified implementation.

;- ============================================================================
;- Struct Field Resolution Result Structure
;- ============================================================================

Structure stFieldResolution
   baseSlot.i           ; Slot of the struct base variable
   byteOffset.i         ; Byte offset within struct (fieldIndex * 8)
   fieldIndex.i         ; Field index (0-based, for slot offset)
   fieldType.w          ; #C2FLAG_INT, #C2FLAG_FLOAT, #C2FLAG_STR, or 0
   isLocal.b            ; True if base slot is local (paramOffset >= 0)
   isParam.b            ; True if base slot is a parameter
   structType.s         ; Name of struct type (for nested struct detection)
   found.b              ; True if resolution succeeded
   isTypeAnnotation.b   ; True if fieldChain is actually a struct type name
EndStructure

;- ============================================================================
;- Struct Slot Lookup
;- ============================================================================

; Note: FindStructSlotByName() is already defined in c2-modules-V23.pb with O(1) lookup support
; Use that function instead of defining a new one here.

;- ============================================================================
;- Field Chain Walking
;- ============================================================================

; Walk a field chain to compute accumulated offset
; Handles nested structs (e.g., "topLeft.x" or "topLeft\x")
;
; Parameters:
;   startType   - Starting struct type name
;   fieldChain  - Field path (e.g., "topLeft.x" or "topLeft\x")
;   separator   - Field separator character ("." or "\")
;   *result     - Output: accumulated offset and field type
;
; Returns:
;   True if all fields were found, False otherwise
;
Procedure.b WalkFieldChain(startType.s, fieldChain.s, separator.s, *result.stFieldResolution)
   Protected currentType.s = startType
   Protected remaining.s = fieldChain
   Protected accumOffset.i = 0
   Protected fieldFound.b = #True
   Protected nextSep.i
   Protected currentField.s
   Protected lastFieldType.w = #C2FLAG_INT

   ; Walk through separator-delimited fields
   While remaining <> "" And fieldFound
      ; Find next separator
      nextSep = FindString(remaining, separator)
      If nextSep > 0
         currentField = Left(remaining, nextSep - 1)
         remaining = Mid(remaining, nextSep + 1)
      Else
         currentField = remaining
         remaining = ""
      EndIf

      ; Trim the field name
      currentField = Trim(currentField)
      If currentField = ""
         Continue
      EndIf

      ; Look up field in current struct type
      fieldFound = #False
      If FindMapElement(mapStructDefs(), currentType)
         ForEach mapStructDefs()\fields()
            If LCase(Trim(mapStructDefs()\fields()\name)) = LCase(currentField)
               ; Found the field
               accumOffset + mapStructDefs()\fields()\offset
               lastFieldType = mapStructDefs()\fields()\fieldType
               If lastFieldType = 0
                  lastFieldType = #C2FLAG_INT
               EndIf
               ; Update current type for nested struct traversal
               currentType = mapStructDefs()\fields()\structType
               fieldFound = #True
               Break
            EndIf
         Next
      EndIf
   Wend

   ; Store results
   If *result
      *result\fieldIndex = accumOffset
      *result\byteOffset = accumOffset * 8
      *result\fieldType = lastFieldType
      *result\structType = currentType
      *result\found = fieldFound
   EndIf

   ProcedureReturn fieldFound
EndProcedure

;- ============================================================================
;- Unified Struct Field Resolver
;- ============================================================================

; Main entry point for struct field resolution
; Handles both DOT notation (rect.topLeft.x) and backslash notation (rect\topLeft\x)
;
; Parameters:
;   text            - Full text with struct reference (e.g., "rect.topLeft.x")
;   functionContext - Current function name for local variable lookup
;   *result         - Output structure for resolution results
;
; Returns:
;   Slot index if found, -1 otherwise
;   Result structure is filled with field details
;
Procedure.i ResolveStructFieldAccess(text.s, functionContext.s, *result.stFieldResolution)
   Protected separatorPos.i
   Protected separator.s
   Protected structName.s
   Protected fieldChain.s
   Protected structSlot.i = -1

   ; Initialize result
   If *result
      *result\baseSlot = -1
      *result\byteOffset = 0
      *result\fieldIndex = 0
      *result\fieldType = #C2FLAG_INT
      *result\isLocal = #False
      *result\isParam = #False
      *result\structType = ""
      *result\found = #False
      *result\isTypeAnnotation = #False
   EndIf

   ; Detect separator: DOT (.) or backslash (\)
   Protected dotPos.i = FindString(text, ".")
   Protected bsPos.i = FindString(text, "\")

   ; Choose the first separator found
   If dotPos > 0 And (bsPos = 0 Or dotPos < bsPos)
      separatorPos = dotPos
      separator = "."
   ElseIf bsPos > 0
      separatorPos = bsPos
      separator = "\"
   Else
      ; No separator found - not a struct field access
      ProcedureReturn -1
   EndIf

   ; Split into struct name and field chain
   structName = Trim(Left(text, separatorPos - 1))
   fieldChain = Trim(Mid(text, separatorPos + 1))

   ; Skip if looks like primitive type suffix (.i, .f, .s, .d)
   If separator = "." And (Len(fieldChain) = 1)
      Protected lowerChain.s = LCase(fieldChain)
      If lowerChain = "i" Or lowerChain = "f" Or lowerChain = "s" Or lowerChain = "d"
         ; This is a type annotation, not struct field access
         ; Check if it's actually a struct type (e.g., "local.Point")
         ; If not, this is just primitive type annotation
         ProcedureReturn -1
      EndIf
   EndIf

   ; Check if fieldChain is a struct TYPE name (e.g., "person.Person")
   ; This handles struct type annotations like: person.Person = { }
   If FindMapElement(mapStructDefs(), fieldChain)
      ; fieldChain is a known struct type - this might be a type annotation
      ; First try to find the struct, then determine if it's annotation or field access
   EndIf

   ; Find the struct slot
   structSlot = FindStructSlotByName(structName, functionContext)

   If structSlot >= 0
      ; Found struct - walk the field chain
      Protected walkResult.stFieldResolution

      ; Get struct type for field walking
      Protected structType.s = gVarMeta(structSlot)\structType
      If structType = ""
         ; No struct type - can't walk fields
         ; Check if fieldChain itself is a struct type (type annotation)
         If FindMapElement(mapStructDefs(), fieldChain)
            If *result
               *result\baseSlot = structSlot
               *result\isTypeAnnotation = #True
               *result\structType = fieldChain
               *result\found = #True
               *result\isLocal = Bool(gVarMeta(structSlot)\paramOffset >= 0)
               *result\isParam = Bool(gVarMeta(structSlot)\flags & #C2FLAG_PARAM)
            EndIf
            ProcedureReturn structSlot
         EndIf
         ProcedureReturn -1
      EndIf

      ; Walk the field chain
      If WalkFieldChain(structType, fieldChain, separator, @walkResult)
         ; Successfully resolved field chain
         If *result
            *result\baseSlot = structSlot
            *result\byteOffset = walkResult\byteOffset
            *result\fieldIndex = walkResult\fieldIndex
            *result\fieldType = walkResult\fieldType
            *result\structType = walkResult\structType
            *result\found = #True
            *result\isLocal = Bool(gVarMeta(structSlot)\paramOffset >= 0)
            *result\isParam = Bool(gVarMeta(structSlot)\flags & #C2FLAG_PARAM)
         EndIf
         ProcedureReturn structSlot
      Else
         ; Field chain walk failed
         ; Check if fieldChain is actually the struct TYPE (not a field name)
         If FindMapElement(mapStructDefs(), fieldChain)
            ; fieldChain is a struct type name, not a field!
            ; This is a type annotation referring to existing struct variable.
            If *result
               *result\baseSlot = structSlot
               *result\isTypeAnnotation = #True
               *result\structType = fieldChain
               *result\found = #True
               *result\isLocal = Bool(gVarMeta(structSlot)\paramOffset >= 0)
               *result\isParam = Bool(gVarMeta(structSlot)\flags & #C2FLAG_PARAM)
            EndIf
            ProcedureReturn structSlot
         EndIf
      EndIf
   Else
      ; Struct not found - check if this is a struct type annotation (e.g., "person.Person")
      If FindMapElement(mapStructDefs(), fieldChain)
         ; This is a struct type annotation for a new variable
         If *result
            *result\baseSlot = -1
            *result\isTypeAnnotation = #True
            *result\structType = fieldChain
            *result\found = #False  ; Struct slot doesn't exist yet
         EndIf
      EndIf
   EndIf

   ProcedureReturn -1
EndProcedure

;- ============================================================================
;- Helper: Check if local struct needs paramOffset assignment
;- ============================================================================

; Assign paramOffset to local struct if not already assigned
; This handles late-binding of paramOffset during codegen
;
; Parameters:
;   structSlot      - Slot of the struct variable
;   functionContext - Current function name
;
; Returns:
;   True if paramOffset was assigned, False otherwise
;
Procedure.b AssignLocalStructParamOffset(structSlot.i, functionContext.s)
   ; Skip if already has paramOffset or not in a function
   If gVarMeta(structSlot)\paramOffset >= 0
      ProcedureReturn #False
   EndIf
   If gCodeGenFunction = 0 Or gCodeGenParamIndex >= 0
      ProcedureReturn #False
   EndIf

   ; Check if it's actually a local (mangled name = function_varname)
   If functionContext <> ""
      If LCase(Left(gVarMeta(structSlot)\name, Len(functionContext) + 1)) = LCase(functionContext + "_")
         ; Assign paramOffset
         gVarMeta(structSlot)\paramOffset = gCodeGenLocalIndex
         gCodeGenLocalIndex + 1

         ; Update nLocals in mapModules
         ForEach mapModules()
            If mapModules()\function = gCodeGenFunction
               mapModules()\nLocals = gCodeGenLocalIndex - mapModules()\nParams
               Break
            EndIf
         Next

         ProcedureReturn #True
      EndIf
   EndIf

   ProcedureReturn #False
EndProcedure

;- ============================================================================
;- Helper: Apply field resolution to gVarMeta
;- ============================================================================

; Store field resolution results in gVarMeta for EmitInt to use
;
Procedure ApplyFieldResolutionToMeta(structSlot.i, *result.stFieldResolution)
   If structSlot >= 0 And *result And *result\found
      gVarMeta(structSlot)\structFieldBase = structSlot
      gVarMeta(structSlot)\structFieldOffset = *result\byteOffset
   EndIf
EndProcedure

;- ============================================================================
;- End of c2-codegen-struct.pbi
;- ============================================================================

; IDE Options = PureBasic 6.10 (Windows - x64)
; CursorPosition = 1
; Folding = --
