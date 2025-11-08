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


;XIncludeFile            "C2-inc-v05.PBI"


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
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And ListSize(llStack()) > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp ) = gVar( callerSp + gVar(varSlot)\paramOffset )
   Else
      ; Regular global variable
      gVar( sp ) = gVar( varSlot )
   EndIf

   sp + 1
   pc + 1
EndProcedure

Procedure               C2FETCHS()
   Protected varSlot.i
   Protected callerSp.i

   vm_DebugFunctionName()
   varSlot = _AR()\i

   ; Check if this is a stack-local parameter AND we're in a function
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And ListSize(llStack()) > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp ) = gVar( callerSp + gVar(varSlot)\paramOffset )
   Else
      ; Regular global variable
      gVar( sp ) = gVar( varSlot )
   EndIf

   gVar( sp )\flags = #C2FLAG_IDENT | #C2FLAG_STR

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
   If (gVar(varSlot)\flags & #C2FLAG_PARAM) And ListSize(llStack()) > 0
      ; Read from stack at callerSp + paramOffset
      callerSp = llStack()\sp
      gVar( sp ) = gVar( callerSp + gVar(varSlot)\paramOffset )
   Else
      ; Regular global variable
      gVar( sp ) = gVar( varSlot )
   EndIf

   gVar( sp )\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT

   ;Debug "FETCHF AFTER: pushed to gVar(" + Str(sp) + "), now incrementing sp"

   sp + 1
   pc + 1
EndProcedure

Procedure               C2POP()
   vm_DebugFunctionName()

   ; OLD CODE - was reading from wrong position AND copying entire structure:
   ; sp - 1
   ; gVar( _AR()\i ) = gVar( sp )  // This overwrites the variable name!

   ; NEW CODE - copy only the data fields, not the name:
   sp - 1
   gVar( _AR()\i )\i = gVar( sp )\i
   ;gVar( _AR()\i )\f = gVar( sp )\f
   ;gVar( _AR()\i )\ss = gVar( sp )\ss
   ;gVar( _AR()\i )\p = gVar( sp )\p
   ; Don't copy: name, flags (those are set by the compiler)

   pc + 1
EndProcedure

Procedure               C2POPS()
   vm_DebugFunctionName()
   ; Pop string value from stack
   sp - 1
   gVar( _AR()\i )\ss = gVar( sp )\ss
   pc + 1
EndProcedure

Procedure               C2POPF()
   vm_DebugFunctionName()
   ; Pop float value from stack
   sp - 1
   gVar( _AR()\i )\f = gVar( sp )\f
   pc + 1
EndProcedure

Procedure               C2PUSHS()
   vm_DebugFunctionName()
   ; Push string value onto stack
   gVar( sp ) = gVar( _AR()\i )
   sp + 1
   pc + 1
EndProcedure

Procedure               C2PUSHF()
   vm_DebugFunctionName()
   ; Push float value onto stack
   gVar( sp ) = gVar( _AR()\i )
   sp + 1
   pc + 1
EndProcedure

Procedure               C2Store()
   vm_DebugFunctionName()
   sp - 1

   ; OLD CODE - copied entire structure, overwriting name:
   ; gVar( _AR()\i ) = gVar( sp )

   ; NEW CODE - copy only data fields:
   gVar( _AR()\i )\i = gVar( sp )\i
   gVar( _AR()\i )\flags = #C2FLAG_IDENT | #C2FLAG_INT
   ;gVar( _AR()\i )\f = gVar( sp )\f
   ;gVar( _AR()\i )\ss = gVar( sp )\ss
   ;gVar( _AR()\i )\p = gVar( sp )\p

   pc + 1
EndProcedure

Procedure               C2STORES()
   vm_DebugFunctionName()
   sp - 1

   ; Copy data fields for string store
   gVar( _AR()\i )\ss = gVar( sp )\ss
   gVar( _AR()\i )\flags = #C2FLAG_IDENT | #C2FLAG_STR

   pc + 1
EndProcedure

Procedure               C2STOREF()
   vm_DebugFunctionName()
   sp - 1

   ; Copy data fields for float store
   gVar( _AR()\i )\f = gVar( sp )\f
   gVar( _AR()\i )\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT

   ;Debug "STOREF: Storing gVar(" + Str(sp) + ")\f=" + StrD(gVar(sp)\f, 6) + " to gVar(" + Str(_AR()\i) + ")[" + gVar(_AR()\i)\name + "]"

   pc + 1
EndProcedure

Procedure               C2MOV()
   vm_DebugFunctionName()

   ; OLD CODE - copied entire structure, overwriting name:
   ; gVar( _AR()\i ) = gVar( _AR()\j )

   ; NEW CODE - copy only data fields:
   gVar( _AR()\i )\i = gVar( _AR()\j )\i
   ;gVar( _AR()\i )\f = gVar( _AR()\j )\f
   ;gVar( _AR()\i )\ss = gVar( _AR()\j )\ss
   ;gVar( _AR()\i )\p = gVar( _AR()\j )\p
   gVar( _AR()\i )\flags = #C2FLAG_IDENT | #C2FLAG_INT

   pc + 1
EndProcedure

Procedure               C2MOVS()
   vm_DebugFunctionName()

   ; Copy data fields for string move
   gVar( _AR()\i )\ss = gVar( _AR()\j )\ss
   gVar( _AR()\i )\flags = #C2FLAG_IDENT | #C2FLAG_STR

   pc + 1
EndProcedure

Procedure               C2MOVF()
   vm_DebugFunctionName()

   ; Copy data fields for float move
   gVar( _AR()\i )\f = gVar( _AR()\j )\f
   gVar( _AR()\i )\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT

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
   gVar(sp - 1)\flags = #C2FLAG_STR  ; Mark result as string

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
   cline = cline + gVar(sp)\ss
   SetGadgetItemText( #edConsole, cy, cline )
   pc + 1
EndProcedure

Procedure               C2PRTI()
   vm_DebugFunctionName()
   sp - 1
   cline = cline + Str( gVar( sp )\i )
   SetGadgetItemText( #edConsole, cy, cline )
   pc + 1
EndProcedure

Procedure               C2PRTF()
   vm_DebugFunctionName()
   sp - 1
   cline = cline + StrD( gVar( sp )\f, gDecs )
   SetGadgetItemText( #edConsole, cy, cline)
   pc + 1
EndProcedure

Procedure               C2PRTC()
   vm_DebugFunctionName()
   sp - 1
   
   If gVar( sp )\i = 10
      cy + 1
      cline = ""
      AddGadgetItem( #edConsole, -1, "" )
   Else
      cline = cline + Chr( gVar( sp )\i )
      SetGadgetItemText( #edConsole, cy, cline )
   EndIf
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
   Protected nParams.i

   nParams = _AR()\j  ; Get parameter count from instruction

   AddElement( llStack() )
   llStack()\pc = pc + 1
   llStack()\sp = sp - nParams  ; Save sp BEFORE params were pushed (FIX: prevents stack leak)
   pc = _AR()\i

   ;Debug "CALL pc=" + Str(pc) + " with " + Str(nParams) + " params, sp=" + Str(sp) + " saving callerSp=" + Str(llStack()\sp)
   ;Debug "s=" + gVar( sp - 3 )\ss + " d=" + Str( gVar( sp - 3 )\i ) + " f=" + StrD( gVar( sp - 3 )\f, 3 )
   ;Debug "s=" + gVar( sp - 2 )\ss + " d=" + Str( gVar( sp - 2 )\i ) + " f=" + StrD( gVar( sp - 2 )\f, 3 )
   ;Debug "s=" + gVar( sp - 1 )\ss + " d=" + Str( gVar( sp - 1 )\i ) + " f=" + StrD( gVar( sp - 1 )\f, 3 )

EndProcedure

Procedure               C2Return()
   vm_DebugFunctionName()

   ; OLD CODE - didn't preserve return value:
   ; pc = llStack()\pc
   ; sp = llStack()\sp
   ; DeleteElement( llStack() )

   ; NEW CODE - preserve return value on stack:
   ; The return value (if any) is at sp-1
   ; We need to copy it to the caller's stack position before restoring sp
   Protected returnValue.stVT
   Protected callerSp.i

   ; Initialize to default integer 0 (prevents uninitialized returns)
   returnValue\i = 0
   returnValue\flags = #C2FLAG_INT

   ; Save caller's stack pointer
   callerSp = llStack()\sp

   ; Save return value from top of stack (sp-1) if there's anything on function's stack
   If sp > callerSp
      returnValue = gVar(sp - 1)
   EndIf

   ; Restore caller's program counter and stack pointer
   pc = llStack()\pc
   sp = callerSp
   DeleteElement( llStack() )

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

   ; Push string return value onto caller's stack
   gVar(sp) = returnValue
   sp + 1
EndProcedure

Procedure               C2HALT()
   vm_DebugFunctionName()
   ; Do nothing - the VM loop checks for HALT and exits
   pc + 1
EndProcedure

;- End VM functions
; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 201
; FirstLine = 164
; Folding = ----------
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; EnablePurifier
; EnableCompileCount = 0
; EnableBuildCount = 0
; EnableExeConstant