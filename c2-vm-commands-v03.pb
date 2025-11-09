; -- lexical parser to VM for a simplified C Language 
; Tested in UTF8
; PBx64 v6.20
;
; Based on  https://rosettacode.org/wiki/Compiler/lexical_analyzer
; And
; https://rosettacode.org/wiki/Compiler/syntax_analyzer
; Distribute and use freely
; 
; Kingwolf71 May/2025
; 
;
; VM Module
;- Library functions
Macro                   vm_DebugFunctionName()
   ;Debug #PB_Compiler_Procedure
EndMacro

; Macro for built-in functions: get parameter count
Macro                   vm_GetParamCount()
   _AR()\j
EndMacro

; Macro for built-in functions: pop N parameters from stack
Macro                   vm_PopParams(n)
   sp - n
EndMacro

; Macro for built-in functions: push integer result
Macro                   vm_PushInt(value)
   gVar(sp)\i = value
   ;gVar(sp)\flags = #C2FLAG_INT
   sp + 1
   CompilerIf #DEBUG
      ;If sp % 100 = 0
      ;   Debug "Stack pointer at: " + Str(sp) + " / " + Str(#C2MAXCONSTANTS)
      ;EndIf
   CompilerEndIf
   pc + 1
EndMacro

Procedure.s             Capitalize( sz.s, option.i = 0 )
   Protected            i, j, flag
   Protected.s          new, char
   
   If option = 0
      ProcedureReturn UCase( sz )
   ElseIf option = 1
      ProcedureReturn LCase( sz )
   Else
      j = Len( sz )
      flag = 1
      
      For i = 1 To j
         char = Mid( sz, i, 1 )
         If flag
            new + UCase( char )
            flag = 0
         ElseIf char = " " Or char = #TAB$
            new + char
            flag = 1
         Else
            new + LCase( char )
         EndIf
      Next

      ProcedureReturn new
   EndIf
EndProcedure

Procedure.s             String( sz.s, size )
   Protected.s          new

   While size
      size - 1
      new + sz
   Wend

   ProcedureReturn new
EndProcedure
;- Jump Table Functions

Procedure               C2FetchPush()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   varSlot = _AR()\i
    
   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp ) = gVar( callerSp + gVar(varSlot)\paramOffset )
   Else
      ; Regular global variable
      gVar( sp ) = gVar( varSlot )
   EndIf

   sp + 1

   ;CompilerIf #DEBUG
   ;   If sp % 100 = 0
   ;      Debug "C2FetchPush: sp=" + Str(sp) + " / " + Str(#C2MAXCONSTANTS)
   ;   EndIf
   ;CompilerEndIf

   pc + 1
EndProcedure

Procedure               C2FETCHS()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp )\ss = gVar( callerSp + gVar(varSlot)\paramOffset )\ss
   Else
      ; Regular global variable
      gVar( sp )\ss = gVar( varSlot )\ss
   EndIf

   sp + 1
   pc + 1
EndProcedure

Procedure               C2FETCHF()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   varSlot = _AR()\i

   ;Debug "FETCHF BEFORE: sp=" + Str(sp) + " fetching gVar(" + Str(varSlot) + ")[" + gVar(varSlot)\name + "] f=" + StrD(gVar(varSlot)\f, 6)

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp )\f = gVar( callerSp + gVar(varSlot)\paramOffset )\f
   Else
      ; Regular global variable
      gVar( sp )\f = gVar( varSlot )\f
   EndIf

   ; Flag already set at compile time by PostProcessor

   sp + 1
   pc + 1
EndProcedure

Procedure               C2POP()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   sp - 1
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Write to stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( callerSp + gVar(varSlot)\paramOffset )\i = gVar( sp )\i
   Else
      ; Regular global variable - write to variable slot
      gVar( varSlot )\i = gVar( sp )\i
   EndIf

   pc + 1
EndProcedure

Procedure               C2POPS()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   sp - 1
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Write to stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( callerSp + gVar(varSlot)\paramOffset )\ss = gVar( sp )\ss
   Else
      ; Regular global variable - write to variable slot
      gVar( varSlot )\ss = gVar( sp )\ss
   EndIf

   pc + 1
EndProcedure

Procedure               C2POPF()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   sp - 1
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Write to stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( callerSp + gVar(varSlot)\paramOffset )\f = gVar( sp )\f
   Else
      ; Regular global variable - write to variable slot
      gVar( varSlot )\f = gVar( sp )\f
   EndIf

   pc + 1
EndProcedure

Procedure               C2PUSHS()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp )\ss = gVar( callerSp + gVar(varSlot)\paramOffset )\ss
   Else
      ; Regular global variable
      gVar( sp )\ss = gVar( varSlot )\ss
   EndIf

   sp + 1
   pc + 1
EndProcedure

Procedure               C2PUSHF()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp )\f = gVar( callerSp + gVar(varSlot)\paramOffset )\f
   Else
      ; Regular global variable
      gVar( sp )\f = gVar( varSlot )\f
   EndIf

   sp + 1
   pc + 1
EndProcedure

Procedure               C2Store()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   sp - 1
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Write to stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( callerSp + gVar(varSlot)\paramOffset )\i = gVar( sp )\i
   Else
      ; Regular global variable - write to variable slot
      gVar( varSlot )\i = gVar( sp )\i
   EndIf

   pc + 1
EndProcedure

Procedure               C2STORES()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   sp - 1
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Write to stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( callerSp + gVar(varSlot)\paramOffset )\ss = gVar( sp )\ss
   Else
      ; Regular global variable - write to variable slot
      gVar( varSlot )\ss = gVar( sp )\ss
   EndIf

   pc + 1
EndProcedure

Procedure               C2STOREF()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   sp - 1
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And gFunctionDepth > 0
      ; Write to stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( callerSp + gVar(varSlot)\paramOffset )\f = gVar( sp )\f
   Else
      ; Regular global variable - write to variable slot
      gVar( varSlot )\f = gVar( sp )\f
   EndIf

   pc + 1
EndProcedure

Procedure               C2MOV()
   vm_DebugFunctionName()
   gVar( _AR()\i )\i = gVar( _AR()\j )\i
   pc + 1
EndProcedure

Procedure               C2MOVS()
   vm_DebugFunctionName()
   gVar( _AR()\i )\ss = gVar( _AR()\j )\ss
   pc + 1
EndProcedure

Procedure               C2MOVF()
   vm_DebugFunctionName()
   gVar( _AR()\i )\f = gVar( _AR()\j )\f
   pc + 1
EndProcedure

;- Local Variable Opcodes (use LocalVars array, no flag checks)
Procedure               C2LMOV()
   vm_DebugFunctionName()
   ; Direct array access: LocalVars(offset) = source
   llStack()\LocalVars(_AR()\i)\i = gVar( _AR()\j )\i
   pc + 1
EndProcedure

Procedure               C2LMOVS()
   vm_DebugFunctionName()
   llStack()\LocalVars(_AR()\i)\ss = gVar( _AR()\j )\ss
   pc + 1
EndProcedure

Procedure               C2LMOVF()
   vm_DebugFunctionName()
   llStack()\LocalVars(_AR()\i)\f = gVar( _AR()\j )\f
   pc + 1
EndProcedure

Procedure               C2LFETCH()
   vm_DebugFunctionName()
   ; Fetch local variable to stack
   gVar( sp ) = llStack()\LocalVars(_AR()\i)
   sp + 1
   pc + 1
EndProcedure

Procedure               C2LFETCHS()
   vm_DebugFunctionName()
   gVar( sp )\ss = llStack()\LocalVars(_AR()\i)\ss
   sp + 1
   pc + 1
EndProcedure

Procedure               C2LFETCHF()
   vm_DebugFunctionName()
   gVar( sp )\f = llStack()\LocalVars(_AR()\i)\f
   sp + 1
   pc + 1
EndProcedure

Procedure               C2LSTORE()
   vm_DebugFunctionName()
   ; Store from stack to local variable
   sp - 1
   llStack()\LocalVars(_AR()\i) = gVar( sp )
   pc + 1
EndProcedure

Procedure               C2LSTORES()
   vm_DebugFunctionName()
   sp - 1
   llStack()\LocalVars(_AR()\i)\ss = gVar( sp )\ss
   pc + 1
EndProcedure

Procedure               C2LSTOREF()
   vm_DebugFunctionName()
   sp - 1
   llStack()\LocalVars(_AR()\i)\f = gVar( sp )\f
   pc + 1
EndProcedure

Procedure               C2JMP()
   vm_DebugFunctionName()
   pc + _AR()\i
EndProcedure

Procedure               C2JZ()
   vm_DebugFunctionName()
   sp - 1
   If Not gVar( sp )\i
      pc + _AR()\i
   Else
      pc + 1
   EndIf
EndProcedure

Procedure               C2ADD()
   vm_DebugFunctionName()
   vm_BitOperation( + )
EndProcedure

Procedure               C2ADDSTR()
   vm_DebugFunctionName()
   Protected leftStr.s, rightStr.s

   sp - 1

   ; Convert left operand (sp-1) to string based on its type
   If gVar(sp - 1)\flags & #C2FLAG_STR
      leftStr = gVar(sp - 1)\ss
   ElseIf gVar(sp - 1)\flags & #C2FLAG_FLOAT
      leftStr = StrD(gVar(sp - 1)\f, gDecs)
   Else
      leftStr = Str(gVar(sp - 1)\i)
   EndIf

   ; Convert right operand (sp) to string based on its type
   If gVar(sp)\flags & #C2FLAG_STR
      rightStr = gVar(sp)\ss
   ElseIf gVar(sp)\flags & #C2FLAG_FLOAT
      rightStr = StrD(gVar(sp)\f, gDecs)
   Else
      rightStr = Str(gVar(sp)\i)
   EndIf

   ; Concatenate and store result
   gVar(sp - 1)\ss = leftStr + rightStr
   ; Flag already set at compile time by PostProcessor

   pc + 1
EndProcedure

Procedure               C2FTOS()
   vm_DebugFunctionName()
   ; Convert float to string at stack top
   gVar(sp - 1)\ss = StrD(gVar(sp - 1)\f, gDecs)
   pc + 1
EndProcedure

Procedure               C2ITOS()
   vm_DebugFunctionName()
   ; Convert integer to string at stack top
   gVar(sp - 1)\ss = Str(gVar(sp - 1)\i)
   pc + 1
EndProcedure

Procedure               C2SUBTRACT()
   vm_DebugFunctionName()
   vm_BitOperation( - )
EndProcedure

Procedure               C2GREATER()
   vm_DebugFunctionName()
   vm_Comparators( > )
EndProcedure

Procedure               C2LESS()
   vm_DebugFunctionName()
   vm_Comparators( < )
EndProcedure

Procedure               C2LESSEQUAL()
   vm_DebugFunctionName()
   vm_Comparators( <= )
EndProcedure

Procedure               C2GREATEREQUAL()
   vm_DebugFunctionName()
   vm_Comparators( >= )
EndProcedure

Procedure               C2NOTEQUAL()
   vm_DebugFunctionName()
   vm_Comparators( <> )
EndProcedure

Procedure               C2EQUAL()
   vm_DebugFunctionName()
   vm_Comparators( = )
EndProcedure

Procedure               C2MULTIPLY()
   vm_DebugFunctionName()
   vm_BitOperation( * )
EndProcedure

Procedure               C2AND()
   vm_DebugFunctionName()
   vm_BitOperation( & )
EndProcedure

Procedure               C2OR()
   vm_DebugFunctionName()
   vm_BitOperation( | )
EndProcedure

Procedure               C2XOR()
   vm_DebugFunctionName()
   vm_BitOperation( ! )
EndProcedure

Procedure               C2NOT()
   vm_DebugFunctionName()
   gVar( sp - 1 )\i = Bool(Not gVar( sp - 1 )\i )
   pc + 1
EndProcedure

Procedure               C2NEGATE()
   vm_DebugFunctionName()
   gVar( sp - 1 )\i = -gVar( sp -1 )\i
   pc + 1
EndProcedure

Procedure               C2DIVIDE()
   vm_DebugFunctionName()
   vm_BitOperation( / )
EndProcedure

Procedure               C2MOD()
   vm_DebugFunctionName()
   vm_BitOperation( % )
EndProcedure

Procedure               C2PRTS()
   vm_DebugFunctionName()
   sp - 1
   CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_Console
      gBatchOutput + gVar(sp)\ss
      Print(gVar(sp)\ss)  ; Echo to console
   CompilerElse
      cline = cline + gVar(sp)\ss
      SetGadgetItemText( #edConsole, cy, cline )
   CompilerEndIf
   pc + 1
EndProcedure

Procedure               C2PRTI()
   vm_DebugFunctionName()
   sp - 1
   CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_Console
      gBatchOutput + Str( gVar( sp )\i )
      Print(Str( gVar( sp )\i ))  ; Echo to console
   CompilerElse
      cline = cline + Str( gVar( sp )\i )
      SetGadgetItemText( #edConsole, cy, cline )
   CompilerEndIf
   pc + 1
EndProcedure

Procedure               C2PRTF()
   vm_DebugFunctionName()
   sp - 1
   CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_Console
      gBatchOutput + StrD( gVar( sp )\f, gDecs )
      Print(StrD( gVar( sp )\f, gDecs ))  ; Echo to console
   CompilerElse
      cline = cline + StrD( gVar( sp )\f, gDecs )
      SetGadgetItemText( #edConsole, cy, cline)
   CompilerEndIf
   pc + 1
EndProcedure

Procedure               C2PRTC()
   vm_DebugFunctionName()
   sp - 1

   CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_Console
      gBatchOutput + Chr( gVar( sp )\i )
      If gVar( sp )\i = 10 : PrintN( "" ) : EndIf
   CompilerElse
      If gVar( sp )\i = 10
         cy + 1
         cline = ""
         AddGadgetItem( #edConsole, -1, "" )
      Else
         cline = cline + Chr( gVar( sp )\i )
         SetGadgetItemText( #edConsole, cy, cline )
      EndIf
   CompilerEndIf
   pc + 1
EndProcedure

Procedure               C2FLOATNEGATE()
   vm_DebugFunctionName()
   gVar(sp-1)\f = -gVar(sp-1)\f
   pc + 1
EndProcedure

Procedure               C2FLOATDIVIDE()
   vm_DebugFunctionName()
   vm_FloatOperation( / )
EndProcedure

Procedure               C2FLOATMUL()
   vm_DebugFunctionName()
   vm_FloatOperation( * )
EndProcedure

Procedure               C2FLOATADD()
   vm_FloatOperation( + )
EndProcedure

Procedure               C2FLOATSUB()
   vm_DebugFunctionName()
   vm_FloatOperation( - )
EndProcedure

Procedure               C2FLOATGREATER()
   vm_DebugFunctionName()
   vm_FloatComparators( > )
EndProcedure

Procedure               C2FLOATLESS()
   vm_DebugFunctionName()
   vm_FloatComparators( < )
EndProcedure

Procedure               C2FLOATLESSEQUAL()
   vm_DebugFunctionName()
   vm_FloatComparators( <= )
EndProcedure

Procedure               C2FLOATGREATEREQUAL()
   vm_DebugFunctionName()
   vm_FloatComparators( >= )
EndProcedure

Procedure               C2FLOATNOTEQUAL()
   vm_DebugFunctionName()
   vm_FloatComparators( <> )
EndProcedure

Procedure               C2FLOATEQUAL()
   vm_DebugFunctionName()
   vm_FloatComparators( = )
EndProcedure

Procedure               C2CALL()
   vm_DebugFunctionName()
   Protected nParams.l, nLocals.l, totalVars.l
   Protected i.l, paramSp.l

   ; Read nParams and nLocals from separate fields (no unpacking)
   nParams = _AR()\j
   nLocals = _AR()\n
   totalVars = nParams + nLocals

   ; User-defined function - create stack frame and jump to bytecode address
   AddElement( llStack() )
   llStack()\pc = pc + 1
   llStack()\sp = sp - nParams  ; Save sp BEFORE params were pushed (FIX: prevents stack leak)

   ; Allocate LocalVars array for this function call (params + locals)
   If totalVars > 0
      ReDim llStack()\LocalVars(totalVars - 1)

      ; Copy parameters from stack into LocalVars array
      paramSp = sp - nParams
      For i = 0 To nParams - 1
         llStack()\LocalVars(i) = gVar(paramSp + i)
      Next
      ; Note: Local variables (indices nParams to totalVars-1) are uninitialized
   EndIf

   pc = _AR()\i
   gFunctionDepth + 1  ; Increment function depth counter

EndProcedure

Procedure               C2Return()
   vm_DebugFunctionName()
   Protected returnValue.stVT
   Protected callerSp.i

   ; Initialize to default integer 0 (prevents uninitialized returns)
   returnValue\i = 0
   returnValue\flags = #C2FLAG_INT

   callerSp = llStack()\sp

   ; Save return value from top of stack (sp-1) if there's anything on function's stack
   If sp > callerSp
      returnValue = gVar(sp - 1)
   EndIf

   ; Restore caller's program counter and stack pointer
   pc = llStack()\pc
   sp = callerSp
   DeleteElement( llStack() )
   gFunctionDepth - 1  ; Decrement function depth counter

   ; Push return value onto caller's stack
   gVar(sp) = returnValue
   sp + 1
EndProcedure

Procedure               C2ReturnF()
   vm_DebugFunctionName()

   ; Float return - preserves float return value from stack
   Protected returnValue.stVT
   Protected callerSp.i

   ; Initialize to default float 0.0
   returnValue\f = 0.0
   returnValue\flags = #C2FLAG_FLOAT

   ; Save caller's stack pointer
   callerSp = llStack()\sp

   ; Save float return value from top of stack (sp-1) if there's anything on function's stack
   If sp > callerSp
      returnValue = gVar(sp - 1)
   EndIf

   ; Restore caller's program counter and stack pointer
   pc = llStack()\pc
   sp = callerSp
   DeleteElement( llStack() )
   gFunctionDepth - 1  ; Decrement function depth counter

   ; Push float return value onto caller's stack
   gVar(sp) = returnValue
   sp + 1
EndProcedure

Procedure               C2ReturnS()
   vm_DebugFunctionName()

   ; String return - preserves string return value from stack
   Protected returnValue.stVT
   Protected callerSp.i

   ; Initialize to default empty string
   returnValue\ss = ""
   returnValue\flags = #C2FLAG_STR

   ; Save caller's stack pointer
   callerSp = llStack()\sp

   ; Save string return value from top of stack (sp-1) if there's anything on function's stack
   If sp > callerSp
      returnValue = gVar(sp - 1)
   EndIf

   ; Restore caller's program counter and stack pointer
   pc = llStack()\pc
   sp = callerSp
   DeleteElement( llStack() )
   gFunctionDepth - 1  ; Decrement function depth counter

   ; Push string return value onto caller's stack
   gVar(sp) = returnValue
   sp + 1
EndProcedure

;-
;- Built-in Functions (VM Handlers)
;-

; random() - Returns random integer
; random()         -> 0 to maxint
; random(max)      -> 0 to max-1
; random(min, max) -> min to max-1
Procedure C2BUILTIN_RANDOM()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected minVal.i, maxVal.i, result.i

   Select paramCount
      Case 0
         result = Random(2147483647)
      Case 1
         maxVal = gVar(sp - 1)\i
         If maxVal <= 0 : maxVal = 1 : EndIf
         result = Random(maxVal - 1)
         vm_PopParams(1)
      Case 2
         maxVal = gVar(sp - 1)\i
         minVal = gVar(sp - 2)\i
         If maxVal <= minVal : maxVal = minVal + 1 : EndIf
         result = Random(maxVal - minVal - 1) + minVal
         vm_PopParams(2)
      Default
         result = 0
         vm_PopParams(paramCount)
   EndSelect

   vm_PushInt(result)
EndProcedure

; abs(x) - Absolute value
Procedure C2BUILTIN_ABS()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected result.i

   If paramCount > 0
      result = Abs(gVar(sp - 1)\i)
      vm_PopParams(paramCount)
   Else
      result = 0
   EndIf

   vm_PushInt(result)
EndProcedure

; min(a, b) - Minimum of two values
Procedure C2BUILTIN_MIN()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected a.i, b.i, result.i

   If paramCount >= 2
      b = gVar(sp - 1)\i
      a = gVar(sp - 2)\i
      If a < b
         result = a
      Else
         result = b
      EndIf
      vm_PopParams(paramCount)
   Else
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_PushInt(result)
EndProcedure

; max(a, b) - Maximum of two values
Procedure C2BUILTIN_MAX()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected a.i, b.i, result.i

   If paramCount >= 2
      b = gVar(sp - 1)\i
      a = gVar(sp - 2)\i
      If a > b
         result = a
      Else
         result = b
      EndIf
      vm_PopParams(paramCount)
   Else
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_PushInt(result)
EndProcedure

Procedure               C2NOOP()
   vm_DebugFunctionName()
   ; No operation - just advance program counter
   pc + 1
EndProcedure

Procedure               C2HALT()
   vm_DebugFunctionName()
   ; Do nothing - the VM loop checks for HALT and exits
   pc + 1
EndProcedure

;- End VM functions
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 667
; FirstLine = 656
; Folding = --------------
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; EnablePurifier
; EnableCompileCount = 0
; EnableBuildCount = 0
; EnableExeConstant