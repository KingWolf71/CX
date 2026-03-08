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
; Built-in Functions Module
; Version: 01
;
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
;- Built-in Functions (VM Handlers)

; random() - Returns random integer
; random()         -> 0 to maxint
; random(max)      -> 0 to max-1
; random(min, max) -> min to max-1
Procedure C2BUILTIN_RANDOM()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected minVal.i, maxVal.i, result.i

   Select paramCount
      Case 0
         result = Random(2147483647)
      Case 1
         maxVal = gEvalStack(sp - 1)\i
         If maxVal <= 0 : maxVal = 1 : EndIf
         result = Random(maxVal - 1)
         vm_PopParams(1)
      Case 2
         maxVal = gEvalStack(sp - 1)\i
         minVal = gEvalStack(sp - 2)\i
         If maxVal <= minVal : maxVal = minVal + 1 : EndIf
         result = Random(maxVal - minVal - 1) + minVal
         vm_PopParams(2)
      Default
         result = 0
         vm_PopParams(paramCount)
   EndSelect

   vm_PushInt(result)
   pc + 1
EndProcedure

; abs(x) - Absolute value
Procedure C2BUILTIN_ABS()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected result.i

   If paramCount > 0
      result = Abs(gEvalStack(sp - 1)\i)
      vm_PopParams(paramCount)
   Else
      result = 0
   EndIf

   vm_PushInt(result)
   pc + 1
EndProcedure

; min(a, b) - Minimum of two values
Procedure C2BUILTIN_MIN()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected a.i, b.i, result.i

   If paramCount >= 2
      b = gEvalStack(sp - 1)\i
      a = gEvalStack(sp - 2)\i
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
   pc + 1
EndProcedure

; max(a, b) - Maximum of two values
Procedure C2BUILTIN_MAX()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected a.i, b.i, result.i

   If paramCount >= 2
      b = gEvalStack(sp - 1)\i
      a = gEvalStack(sp - 2)\i
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
   pc + 1
EndProcedure

; assertEqual(expected, actual) - Assert integers are equal
Procedure C2BUILTIN_ASSERT_EQUAL()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected expected.i, actual.i, result.i
   Protected message.s

   If paramCount >= 2
      actual = gEvalStack(sp - 1)\i
      expected = gEvalStack(sp - 2)\i

      If expected = actual
         message = "[PASS] assertEqual: " + Str(expected) + " == " + Str(actual)
         result = 1
      Else
         message = "[FAIL] assertEqual: expected " + Str(expected) + " but got " + Str(actual)
         result = 0
      EndIf

      vm_PopParams(paramCount)
   Else
      message = "[FAIL] assertEqual: requires 2 parameters"
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_AssertPrint( message )
   pc + 1  ; V1.020.053: Assertions don't return values (statement-level calls only)
EndProcedure

; V1.039.56: assert(condition) - Assert condition is non-zero (truthy)
Procedure C2BUILTIN_ASSERT()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected condition.i
   Protected message.s

   If paramCount >= 1
      condition = gEvalStack(sp - 1)\i
      vm_PopParams(paramCount)
      If condition
         message = "[PASS] assert: true"
      Else
         message = "[FAIL] assert: condition is false"
      EndIf
   Else
      message = "[FAIL] assert: requires 1 parameter"
      vm_PopParams(paramCount)
   EndIf

   vm_AssertPrint( message )
   pc + 1
EndProcedure

; assertFloatEqual(expected, actual, tolerance) - Assert floats are equal within tolerance
; If tolerance is omitted, uses #pragma floattolerance value (default: 0.00001)
Procedure C2BUILTIN_ASSERT_FLOAT()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()

   Protected paramCount.i = vm_GetParamCount()
   Protected expected.d, actual.d, tolerance.d, result.i
   Protected message.s

   If paramCount >= 2
      If paramCount >= 3
         tolerance = gEvalStack(sp - 1)\f
         actual = gEvalStack(sp - 2)\f
         expected = gEvalStack(sp - 3)\f
      Else
         tolerance = gFloatTolerance
         actual = gEvalStack(sp - 1)\f
         expected = gEvalStack(sp - 2)\f
      EndIf

      If Abs(expected - actual) <= tolerance
         message = "[PASS] assertFloatEqual: " + StrD(expected, gDecs) + " ~= " + StrD(actual, gDecs) + " (tol=" + StrD(tolerance, gDecs) + ")"
         result = 1
      Else
         message = "[FAIL] assertFloatEqual: expected " + StrD(expected, gDecs) + " but got " + StrD(actual, gDecs) + " (diff=" + StrD(Abs(expected - actual), gDecs) + ", tol=" + StrD(tolerance, gDecs) + ")"
         result = 0
      EndIf

      vm_PopParams(paramCount)
   Else
      message = "[FAIL] assertFloatEqual: requires 2-3 parameters"
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_AssertPrint( message )

   ; V1.034.67: Push return value to match AST-generated POP
   vm_PushInt(result)

   pc + 1
EndProcedure

; assertStringEqual(expected, actual) - Assert strings are equal
Procedure C2BUILTIN_ASSERT_STRING()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected expected.s, actual.s, result.i
   Protected message.s

   If paramCount >= 2
      actual = gEvalStack(sp - 1)\ss
      expected = gEvalStack(sp - 2)\ss

      If expected = actual
         message = ~"[PASS] assertStringEqual: \"" + expected + ~"\" == \"" + actual + ~"\""
         result = 1
      Else
         message = ~"[FAIL] assertStringEqual: expected \"" + expected + ~"\" but got \"" + actual + ~"\""
         result = 0
      EndIf

      vm_PopParams(paramCount)
   Else
      message = "[FAIL] assertStringEqual: requires 2 parameters"
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_AssertPrint( message )
   ; V1.034.67: Push return value to match AST-generated POP
   vm_PushInt(result)
   pc + 1
EndProcedure

; sqrt(x) - Square root (returns float)
Procedure C2BUILTIN_SQRT()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected result.d

   If paramCount > 0
      result = Sqr(gEvalStack(sp - 1)\f)
      vm_PopParams(paramCount)
   Else
      result = 0.0
   EndIf

   vm_PushFloat(result)
   pc + 1
EndProcedure

; pow(base, exp) - Power function (returns float)
Procedure C2BUILTIN_POW()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected base.d, exp.d, result.d

   If paramCount >= 2
      exp = gEvalStack(sp - 1)\f
      base = gEvalStack(sp - 2)\f
      result = Pow(base, exp)
      vm_PopParams(paramCount)
   Else
      result = 0.0
      vm_PopParams(paramCount)
   EndIf

   vm_PushFloat(result)
   pc + 1
EndProcedure

; len(s) - String length (returns integer)
Procedure C2BUILTIN_LEN()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected result.i

   If paramCount > 0
      result = Len(gEvalStack(sp - 1)\ss)
      vm_PopParams(paramCount)
   Else
      result = 0
   EndIf

   vm_PushInt(result)
   pc + 1
EndProcedure

; strcmp(a, b) - String compare (PureBasic & SpiderBasic compatible)
; Returns: -1 if a < b, 0 if a == b, 1 if a > b
Procedure C2BUILTIN_STRCMP()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected result.i
   Protected a.s, b.s

   If paramCount >= 2
      b = gEvalStack(sp - 1)\ss
      a = gEvalStack(sp - 2)\ss
      ; Use native string comparison (works in both PB and SB)
      If a < b
         result = -1
      ElseIf a > b
         result = 1
      Else
         result = 0
      EndIf
      vm_PopParams(paramCount)
   Else
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_PushInt(result)
   pc + 1
EndProcedure

; getc(s, idx) - Get character code at index (0-based)
; Returns: ASCII/Unicode value of character, or 0 if out of bounds
; Uses direct memory access for speed (works in both PB and SB)
Procedure C2BUILTIN_GETC()
   ; V1.31.0: Read params from gEvalStack[]
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected result.i
   Protected *s, idx.i, slen.i

   If paramCount >= 2
      idx = gEvalStack(sp - 1)\i
      *s = @gEvalStack(sp - 2)\ss
      slen = Len(gEvalStack(sp - 2)\ss)
      If idx >= 0 And idx < slen
         result = PeekC(*s + idx * SizeOf(Character))  ; Direct memory access
      Else
         result = 0
      EndIf
      vm_PopParams(paramCount)
   Else
      result = 0
      vm_PopParams(paramCount)
   EndIf

   vm_PushInt(result)
   pc + 1
EndProcedure

;- V1.039.53: Shared macro for printf/sprintf format string processing
; Supports:
;   %d, %i - integer
;   %f     - float (default decimals)
;   %.Nf   - float with N decimal places
;   %s     - string
;   %p, %a - pointer/address (displays integer as hex address)
;   %%     - literal percent
; Escape sequences are processed at compile time in scanner
; Requires: format.s, fmtLen.i, paramCount.i, output.s, i.i, argIndex.i, ch.s, nextCh.s, precision.i, precisionStr.s
Macro _FormatStringBuild()
   i = 1
   While i <= fmtLen
      ch = Mid(format, i, 1)

      If ch = "%"
         i + 1
         If i > fmtLen
            output + "%"
            Break
         EndIf

         nextCh = Mid(format, i, 1)

         Select nextCh
            Case "%"
               output + "%"

            Case "d", "i"
               If argIndex < paramCount
                  output + Str(gEvalStack(sp - paramCount + argIndex)\i)
                  argIndex + 1
               EndIf

            Case "f"
               If argIndex < paramCount
                  output + StrD(gEvalStack(sp - paramCount + argIndex)\f, gDecs)
                  argIndex + 1
               EndIf

            Case "s"
               If argIndex < paramCount
                  output + gEvalStack(sp - paramCount + argIndex)\ss
                  argIndex + 1
               EndIf

            Case "p", "a"
               If argIndex < paramCount
                  output + "0x" + Hex(gEvalStack(sp - paramCount + argIndex)\i)
                  argIndex + 1
               EndIf

            Case "."
               i + 1
               precisionStr = ""
               While i <= fmtLen
                  ch = Mid(format, i, 1)
                  If ch >= "0" And ch <= "9"
                     precisionStr + ch
                     i + 1
                  Else
                     Break
                  EndIf
               Wend

               If i <= fmtLen And Mid(format, i, 1) = "f"
                  precision = Val(precisionStr)
                  If precision > 15 : precision = 15 : EndIf
                  If argIndex < paramCount
                     output + StrD(gEvalStack(sp - paramCount + argIndex)\f, precision)
                     argIndex + 1
                  EndIf
               Else
                  output + "%." + precisionStr
                  i - 1
               EndIf

            Default
               output + "%" + nextCh
         EndSelect
      Else
         output + ch
      EndIf

      i + 1
   Wend
EndMacro

;- V1.035.13: printf() - C-style formatted output (uses _FormatStringBuild macro)
Procedure C2BUILTIN_PRINTF()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected format.s, output.s
   Protected i.i, fmtLen.i, argIndex.i
   Protected ch.s, nextCh.s
   Protected precision.i, precisionStr.s
   Protected outLen.i, outIdx.i, outCh.s

   If paramCount < 1
      pc + 1
      ProcedureReturn
   EndIf

   format = gEvalStack(sp - paramCount)\ss
   fmtLen = gEvalStack(sp - paramCount)\i
   If fmtLen = 0 : fmtLen = Len(format) : EndIf
   argIndex = 1
   output = ""

   _FormatStringBuild()

   ; Output the formatted string
   ; V1.035.13: Handle newlines properly for GUI console output
   CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_Console
      gBatchOutput + output
      gConsoleLine + output
      Print(output)
   CompilerElse
      If gTestMode = #True
         Print(output)
      Else
         ; GUI mode: process output character by character for proper newline handling
         outLen = Len(output)
         For outIdx = 1 To outLen
            outCh = Mid(output, outIdx, 1)
            If outCh = Chr(10)  ; Newline
               If gFastPrint = #True
                  vm_SetGadgetText( #edConsole, cy, cline )
               EndIf
               cy + 1
               cline = ""
               vm_AddGadgetLine( #edConsole, "" )
               vm_ScrollToBottom( #edConsole )
               vm_ScrollGadget( #edConsole )
            ElseIf outCh <> Chr(13)  ; Skip carriage return
               cline = cline + outCh
               If gFastPrint = #False
                  vm_SetGadgetText( #edConsole, cy, cline )
               EndIf
            EndIf
         Next
      EndIf
   CompilerEndIf

   vm_PopParams(paramCount)
   pc + 1
EndProcedure

;- V1.039.53: sprintf() - C-style formatted string (uses _FormatStringBuild macro)
; Returns formatted string instead of printing
Procedure C2BUILTIN_SPRINTF()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected format.s, output.s
   Protected i.i, fmtLen.i, argIndex.i
   Protected ch.s, nextCh.s
   Protected precision.i, precisionStr.s

   If paramCount < 1
      vm_PushString("")
      pc + 1
      ProcedureReturn
   EndIf

   format = gEvalStack(sp - paramCount)\ss
   fmtLen = gEvalStack(sp - paramCount)\i
   If fmtLen = 0 : fmtLen = Len(format) : EndIf
   argIndex = 1
   output = ""

   _FormatStringBuild()

   vm_PopParams(paramCount)
   vm_PushString(output)
   pc + 1
EndProcedure

;- V1.038.0: SpiderBasic Math Library Builtins
; Optimized for speed: direct PureBasic calls, minimal overhead
; V1.039.64: Macro-generated unary float builtins

Macro _DEF_UNARY_FLOAT(name, func)
   Procedure C2BUILTIN_#name()
      vm_DebugFunctionName()
      gEvalStack(sp - 1)\f = func(gEvalStack(sp - 1)\f)
      pc + 1
   EndProcedure
EndMacro

_DEF_UNARY_FLOAT(SIN, Sin)
_DEF_UNARY_FLOAT(COS, Cos)
_DEF_UNARY_FLOAT(TAN, Tan)
_DEF_UNARY_FLOAT(ASIN, ASin)
_DEF_UNARY_FLOAT(ACOS, ACos)
_DEF_UNARY_FLOAT(ATAN, ATan)
_DEF_UNARY_FLOAT(SINH, SinH)
_DEF_UNARY_FLOAT(COSH, CosH)
_DEF_UNARY_FLOAT(TANH, TanH)
_DEF_UNARY_FLOAT(LOG, Log)
_DEF_UNARY_FLOAT(LOG10, Log10)
_DEF_UNARY_FLOAT(EXP, Exp)
_DEF_UNARY_FLOAT(FABS, Abs)

Procedure C2BUILTIN_ATAN2()
   vm_DebugFunctionName()
   gEvalStack(sp - 2)\f = ATan2(gEvalStack(sp - 2)\f, gEvalStack(sp - 1)\f)
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_FLOOR()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\i = Int(gEvalStack(sp - 1)\f)
   If gEvalStack(sp - 1)\f < 0 And gEvalStack(sp - 1)\f <> gEvalStack(sp - 1)\i
      gEvalStack(sp - 1)\i - 1
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_CEIL()
   vm_DebugFunctionName()
   Protected v.d = gEvalStack(sp - 1)\f
   Protected i.i = Int(v)
   If v > i
      i + 1
   EndIf
   gEvalStack(sp - 1)\i = i
   pc + 1
EndProcedure

Procedure C2BUILTIN_ROUND()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\i = Round(gEvalStack(sp - 1)\f, #PB_Round_Nearest)
   pc + 1
EndProcedure

Procedure C2BUILTIN_SIGN()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\i = Sign(gEvalStack(sp - 1)\f)
   pc + 1
EndProcedure

Procedure C2BUILTIN_MOD()
   vm_DebugFunctionName()
   gEvalStack(sp - 2)\f = Mod(gEvalStack(sp - 2)\f, gEvalStack(sp - 1)\f)
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_FMIN()
   vm_DebugFunctionName()
   If gEvalStack(sp - 1)\f < gEvalStack(sp - 2)\f
      gEvalStack(sp - 2)\f = gEvalStack(sp - 1)\f
   EndIf
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_FMAX()
   vm_DebugFunctionName()
   If gEvalStack(sp - 1)\f > gEvalStack(sp - 2)\f
      gEvalStack(sp - 2)\f = gEvalStack(sp - 1)\f
   EndIf
   sp - 1
   pc + 1
EndProcedure

;- V1.038.0: SpiderBasic String Library Builtins
; V1.039.64: Macro-generated unary/binary string builtins

Macro _DEF_UNARY_STRING(name, func)
   Procedure C2BUILTIN_#name()
      vm_DebugFunctionName()
      gEvalStack(sp - 1)\ss = func(gEvalStack(sp - 1)\ss)
      gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)
      pc + 1
   EndProcedure
EndMacro

Macro _DEF_BINARY_STRING_LEN(name, func)
   Procedure C2BUILTIN_#name()
      vm_DebugFunctionName()
      gEvalStack(sp - 2)\ss = func(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\i)
      gEvalStack(sp - 2)\i = Len(gEvalStack(sp - 2)\ss)
      sp - 1
      pc + 1
   EndProcedure
EndMacro

_DEF_BINARY_STRING_LEN(LEFT, Left)
_DEF_BINARY_STRING_LEN(RIGHT, Right)

Procedure C2BUILTIN_MID()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   If paramCount = 3
      gEvalStack(sp - 3)\ss = Mid(gEvalStack(sp - 3)\ss, gEvalStack(sp - 2)\i, gEvalStack(sp - 1)\i)
      gEvalStack(sp - 3)\i = Len(gEvalStack(sp - 3)\ss)  ; V1.039.58: sync cached length
      sp - 2
   Else
      gEvalStack(sp - 2)\ss = Mid(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\i)
      gEvalStack(sp - 2)\i = Len(gEvalStack(sp - 2)\ss)  ; V1.039.58: sync cached length
      sp - 1
   EndIf
   pc + 1
EndProcedure

_DEF_UNARY_STRING(TRIM, Trim)
_DEF_UNARY_STRING(LTRIM, LTrim)
_DEF_UNARY_STRING(RTRIM, RTrim)
_DEF_UNARY_STRING(LCASE, LCase)
_DEF_UNARY_STRING(UCASE, UCase)

Procedure C2BUILTIN_CHR()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\ss = Chr(gEvalStack(sp - 1)\i)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length (overwrites int input)
   pc + 1
EndProcedure

Procedure C2BUILTIN_ASC()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\i = Asc(gEvalStack(sp - 1)\ss)
   pc + 1
EndProcedure

Procedure C2BUILTIN_FINDSTRING()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected startPos.i = 1
   If paramCount = 3
      startPos = gEvalStack(sp - 1)\i
      gEvalStack(sp - 3)\i = FindString(gEvalStack(sp - 3)\ss, gEvalStack(sp - 2)\ss, startPos)
      sp - 2
   Else
      gEvalStack(sp - 2)\i = FindString(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\ss)
      sp - 1
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_REPLACESTRING()
   vm_DebugFunctionName()
   gEvalStack(sp - 3)\ss = ReplaceString(gEvalStack(sp - 3)\ss, gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\ss)
   gEvalStack(sp - 3)\i = Len(gEvalStack(sp - 3)\ss)  ; V1.039.58: sync cached length
   sp - 2
   pc + 1
EndProcedure

Procedure C2BUILTIN_COUNTSTRING()
   vm_DebugFunctionName()
   gEvalStack(sp - 2)\i = CountString(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\ss)
   sp - 1
   pc + 1
EndProcedure

_DEF_UNARY_STRING(REVERSESTRING, ReverseString)

Procedure C2BUILTIN_INSERTSTRING()
   vm_DebugFunctionName()
   gEvalStack(sp - 3)\ss = InsertString(gEvalStack(sp - 3)\ss, gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\i)
   gEvalStack(sp - 3)\i = Len(gEvalStack(sp - 3)\ss)  ; V1.039.58: sync cached length
   sp - 2
   pc + 1
EndProcedure

Procedure C2BUILTIN_REMOVESTRING()
   vm_DebugFunctionName()
   gEvalStack(sp - 2)\ss = RemoveString(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\ss)
   gEvalStack(sp - 2)\i = Len(gEvalStack(sp - 2)\ss)  ; V1.039.58: sync cached length
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_SPACE()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\ss = Space(gEvalStack(sp - 1)\i)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_LSET()
   vm_DebugFunctionName()
   gEvalStack(sp - 2)\ss = LSet(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\i)
   gEvalStack(sp - 2)\i = Len(gEvalStack(sp - 2)\ss)  ; V1.039.58: sync cached length
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_RSET()
   vm_DebugFunctionName()
   gEvalStack(sp - 2)\ss = RSet(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\i)
   gEvalStack(sp - 2)\i = Len(gEvalStack(sp - 2)\ss)  ; V1.039.58: sync cached length
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_STRFLOAT()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\f = ValD(gEvalStack(sp - 1)\ss)
   pc + 1
EndProcedure

Procedure C2BUILTIN_STRINT()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\i = Val(gEvalStack(sp - 1)\ss)
   pc + 1
EndProcedure

Procedure C2BUILTIN_HEX()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\ss = Hex(gEvalStack(sp - 1)\i)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_BIN()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\ss = Bin(gEvalStack(sp - 1)\i)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_VALF()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\f = ValD(gEvalStack(sp - 1)\ss)
   pc + 1
EndProcedure

Procedure C2BUILTIN_VALI()
   vm_DebugFunctionName()
   gEvalStack(sp - 1)\i = Val(gEvalStack(sp - 1)\ss)
   pc + 1
EndProcedure

Procedure C2BUILTIN_CAPITALIZE()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   Protected mode.i = 0
   Protected sz.s, i.i, j.i, flag.i, char.s, new.s

   If paramCount >= 2
      mode = gEvalStack(sp - 1)\i
      sz = gEvalStack(sp - 2)\ss
      sp - 1
   Else
      sz = gEvalStack(sp - 1)\ss
   EndIf

   If mode = 0
      gEvalStack(sp - 1)\ss = UCase(sz)
   ElseIf mode = 1
      gEvalStack(sp - 1)\ss = LCase(sz)
   Else
      ; Title case
      j = Len(sz)
      flag = 1
      new = ""
      For i = 1 To j
         char = Mid(sz, i, 1)
         If flag
            new + UCase(char)
            flag = 0
         ElseIf char = " " Or char = #TAB$
            new + char
            flag = 1
         Else
            new + LCase(char)
         EndIf
      Next
      gEvalStack(sp - 1)\ss = new
   EndIf
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

;- V1.039.63: Common Utility Builtins (SpiderBasic compatible)

Procedure C2BUILTIN_CLAMP()
   vm_DebugFunctionName()
   Protected x.d = gEvalStack(sp - 3)\f
   Protected lo.d = gEvalStack(sp - 2)\f
   Protected hi.d = gEvalStack(sp - 1)\f
   If x < lo : x = lo : EndIf
   If x > hi : x = hi : EndIf
   gEvalStack(sp - 3)\f = x
   sp - 2
   pc + 1
EndProcedure

Procedure C2BUILTIN_LERP()
   vm_DebugFunctionName()
   Protected a.d = gEvalStack(sp - 3)\f
   Protected b.d = gEvalStack(sp - 2)\f
   Protected t.d = gEvalStack(sp - 1)\f
   gEvalStack(sp - 3)\f = a + (b - a) * t
   sp - 2
   pc + 1
EndProcedure

Procedure C2BUILTIN_REMAP()
   vm_DebugFunctionName()
   Protected x.d = gEvalStack(sp - 5)\f
   Protected inLo.d = gEvalStack(sp - 4)\f
   Protected inHi.d = gEvalStack(sp - 3)\f
   Protected outLo.d = gEvalStack(sp - 2)\f
   Protected outHi.d = gEvalStack(sp - 1)\f
   Protected range.d = inHi - inLo
   If range = 0.0
      gEvalStack(sp - 5)\f = outLo
   Else
      gEvalStack(sp - 5)\f = outLo + (x - inLo) * (outHi - outLo) / range
   EndIf
   sp - 4
   pc + 1
EndProcedure

Procedure C2BUILTIN_DISTANCE()
   vm_DebugFunctionName()
   Protected dx.d = gEvalStack(sp - 2)\f - gEvalStack(sp - 4)\f
   Protected dy.d = gEvalStack(sp - 1)\f - gEvalStack(sp - 3)\f
   gEvalStack(sp - 4)\f = Sqr(dx * dx + dy * dy)
   sp - 3
   pc + 1
EndProcedure

Procedure C2BUILTIN_BETWEEN()
   vm_DebugFunctionName()
   Protected x.d = gEvalStack(sp - 3)\f
   Protected lo.d = gEvalStack(sp - 2)\f
   Protected hi.d = gEvalStack(sp - 1)\f
   If x >= lo And x <= hi
      gEvalStack(sp - 3)\i = 1
   Else
      gEvalStack(sp - 3)\i = 0
   EndIf
   sp - 2
   pc + 1
EndProcedure

Procedure C2BUILTIN_CONTAINS()
   vm_DebugFunctionName()
   Protected pos.i = FindString(gEvalStack(sp - 2)\ss, gEvalStack(sp - 1)\ss)
   gEvalStack(sp - 2)\i = pos
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_STARTSWITH()
   vm_DebugFunctionName()
   Protected prefix.s = gEvalStack(sp - 1)\ss
   Protected s.s = gEvalStack(sp - 2)\ss
   If Left(s, Len(prefix)) = prefix
      gEvalStack(sp - 2)\i = 1
   Else
      gEvalStack(sp - 2)\i = 0
   EndIf
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_STRINGFIELD()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 3)\ss
   Protected idx.i = gEvalStack(sp - 2)\i
   Protected delim.s = gEvalStack(sp - 1)\ss
   gEvalStack(sp - 3)\ss = StringField(s, idx, delim)
   gEvalStack(sp - 3)\i = Len(gEvalStack(sp - 3)\ss)
   sp - 2
   pc + 1
EndProcedure

Procedure C2BUILTIN_SUM()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   vm_PopParams(paramCount)
   ; TODO: Implement proper array sum using *gVar(slot)\var(0)\dta\ar()
   pc + 1
EndProcedure

;- V1.038.0: SpiderBasic Sort Builtin
; TODO: sortarray requires deep integration with dta\ar() array structure
; For now, just pop parameters and continue (no-op)
Procedure C2BUILTIN_SORTARRAY()
   vm_DebugFunctionName()
   Protected paramCount.i = vm_GetParamCount()
   vm_PopParams(paramCount)
   ; TODO: Implement proper array sorting using *gVar(slot)\var(0)\dta\ar()
   pc + 1
EndProcedure

;- V1.038.0: SpiderBasic Cipher Library Builtins

Procedure C2BUILTIN_MD5()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   UseMD5Fingerprint()
   gEvalStack(sp - 1)\ss = Fingerprint(@s, StringByteLength(s), #PB_Cipher_MD5)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_SHA1()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   UseSHA1Fingerprint()
   gEvalStack(sp - 1)\ss = Fingerprint(@s, StringByteLength(s), #PB_Cipher_SHA1)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_SHA256()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   UseSHA2Fingerprint()
   gEvalStack(sp - 1)\ss = Fingerprint(@s, StringByteLength(s), #PB_Cipher_SHA2, 256)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_SHA512()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   UseSHA2Fingerprint()
   gEvalStack(sp - 1)\ss = Fingerprint(@s, StringByteLength(s), #PB_Cipher_SHA2, 512)
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_CRC32()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   UseCRC32Fingerprint()
   gEvalStack(sp - 1)\i = Val("$" + Fingerprint(@s, StringByteLength(s), #PB_Cipher_CRC32))
   pc + 1
EndProcedure

Procedure C2BUILTIN_BASE64ENC()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   Protected *buf, bufLen.i
   bufLen = StringByteLength(s, #PB_UTF8)
   If bufLen > 0
      *buf = AllocateMemory(bufLen + 1)
      If *buf
         PokeS(*buf, s, -1, #PB_UTF8)
         gEvalStack(sp - 1)\ss = Base64Encoder(*buf, bufLen)
         FreeMemory(*buf)
      Else
         gEvalStack(sp - 1)\ss = ""
      EndIf
   Else
      gEvalStack(sp - 1)\ss = ""
   EndIf
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_BASE64DEC()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   Protected *buf, bufLen.i, outLen.i
   bufLen = Len(s)
   If bufLen > 0
      *buf = AllocateMemory(bufLen + 16)
      If *buf
         outLen = Base64Decoder(s, *buf, MemorySize(*buf))
         If outLen > 0
            gEvalStack(sp - 1)\ss = PeekS(*buf, outLen, #PB_UTF8)
         Else
            gEvalStack(sp - 1)\ss = ""
         EndIf
         FreeMemory(*buf)
      Else
         gEvalStack(sp - 1)\ss = ""
      EndIf
   Else
      gEvalStack(sp - 1)\ss = ""
   EndIf
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

;- V1.038.0: SpiderBasic JSON Library Builtins

Procedure C2BUILTIN_JSONPARSE()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   gEvalStack(sp - 1)\i = ParseJSON(#PB_Any, s)
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONFREE()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      FreeJSON(h)
   EndIf
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONVALUE()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      gEvalStack(sp - 1)\ss = GetJSONString(JSONValue(h))
   Else
      gEvalStack(sp - 1)\ss = ""
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONTYPE()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      gEvalStack(sp - 1)\i = JSONType(JSONValue(h))
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONMEMBER()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 2)\i
   Protected key.s = gEvalStack(sp - 1)\ss
   If IsJSON(h)
      gEvalStack(sp - 2)\i = GetJSONMember(JSONValue(h), key)
   Else
      gEvalStack(sp - 2)\i = 0
   EndIf
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONELEMENT()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 2)\i
   Protected idx.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      gEvalStack(sp - 2)\i = GetJSONElement(JSONValue(h), idx)
   Else
      gEvalStack(sp - 2)\i = 0
   EndIf
   sp - 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONSIZE()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      gEvalStack(sp - 1)\i = JSONArraySize(JSONValue(h))
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONSTRING()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If h  ; Handle can be JSONValue pointer
      gEvalStack(sp - 1)\ss = GetJSONString(h)
   Else
      gEvalStack(sp - 1)\ss = ""
   EndIf
   gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)  ; V1.039.58: sync cached length
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONNUMBER()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If h
      gEvalStack(sp - 1)\f = GetJSONDouble(h)
   Else
      gEvalStack(sp - 1)\f = 0.0
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONBOOL()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If h
      gEvalStack(sp - 1)\i = GetJSONBoolean(h)
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONCREATE()
   vm_DebugFunctionName()
   Protected h.i = CreateJSON(#PB_Any)
   ; V1.039.57: CreateJSON root is NULL type; must SetJSONObject before AddJSONMember works
   If h : SetJSONObject(JSONValue(h)) : EndIf
   gEvalStack(sp)\i = h
   sp + 1
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONADD()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 3)\i
   Protected key.s = gEvalStack(sp - 2)\ss
   Protected value.s = gEvalStack(sp - 1)\ss
   If IsJSON(h)
      SetJSONString(AddJSONMember(JSONValue(h), key), value)
   EndIf
   sp - 3
   pc + 1
EndProcedure

Procedure C2BUILTIN_JSONEXPORT()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      gEvalStack(sp - 1)\ss = ComposeJSON(h)
      gEvalStack(sp - 1)\i = Len(gEvalStack(sp - 1)\ss)
   Else
      gEvalStack(sp - 1)\ss = ""
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

; V1.039.57: jsonaddnum(handle, key, value.f) - add numeric member to JSON object
Procedure C2BUILTIN_JSONADDNUM()
   vm_DebugFunctionName()
   Protected h.i   = gEvalStack(sp - 3)\i
   Protected key.s = gEvalStack(sp - 2)\ss
   Protected val.d = gEvalStack(sp - 1)\f
   If IsJSON(h)
      SetJSONDouble(AddJSONMember(JSONValue(h), key), val)
   EndIf
   sp - 3
   pc + 1
EndProcedure

; V1.039.57: jsonaddbool(handle, key, value.i) - add boolean member to JSON object
Procedure C2BUILTIN_JSONADDBOOL()
   vm_DebugFunctionName()
   Protected h.i   = gEvalStack(sp - 3)\i
   Protected key.s = gEvalStack(sp - 2)\ss
   Protected val.i = gEvalStack(sp - 1)\i
   If IsJSON(h)
      SetJSONBoolean(AddJSONMember(JSONValue(h), key), val)
   EndIf
   sp - 3
   pc + 1
EndProcedure

;- V1.039.57: XML Builtins
; xmlparse(str) - parse XML string, returns handle (0=error)
Procedure C2BUILTIN_XMLPARSE()
   vm_DebugFunctionName()
   Protected s.s = gEvalStack(sp - 1)\ss
   Protected h.i = ParseXML(#PB_Any, s)
   If h And XMLStatus(h) = #PB_XML_Success
      gEvalStack(sp - 1)\i = h
   Else
      If h : FreeXML(h) : EndIf
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

; xmlfree(handle) - free XML handle
Procedure C2BUILTIN_XMLFREE()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsXML(h)
      FreeXML(h)
   EndIf
   sp - 1
   pc + 1
EndProcedure

; xmlroot(handle) - get document root node (parent of all top-level elements)
; Use xmlchild(xmlroot(h)) to get the first actual element when reading
; Use xmladdnode(xmlroot(h), "tag") to create root element when building
Procedure C2BUILTIN_XMLROOT()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsXML(h)
      gEvalStack(sp - 1)\i = MainXMLNode(h)
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

; xmlchild(node) - get first child node
Procedure C2BUILTIN_XMLCHILD()
   vm_DebugFunctionName()
   Protected node.i = gEvalStack(sp - 1)\i
   If node
      gEvalStack(sp - 1)\i = ChildXMLNode(node)
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

; xmlnext(node) - get next sibling node
Procedure C2BUILTIN_XMLNEXT()
   vm_DebugFunctionName()
   Protected node.i = gEvalStack(sp - 1)\i
   If node
      gEvalStack(sp - 1)\i = NextXMLNode(node)
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

; xmlname(node) - get element tag name
Procedure C2BUILTIN_XMLNAME()
   vm_DebugFunctionName()
   Protected node.i = gEvalStack(sp - 1)\i
   If node
      gEvalStack(sp - 1)\ss = GetXMLNodeName(node)
      gEvalStack(sp - 1)\i  = Len(gEvalStack(sp - 1)\ss)
   Else
      gEvalStack(sp - 1)\ss = ""
      gEvalStack(sp - 1)\i  = 0
   EndIf
   pc + 1
EndProcedure

; xmltext(node) - get element text content (concatenates all child text nodes)
Procedure C2BUILTIN_XMLTEXT()
   vm_DebugFunctionName()
   Protected node.i   = gEvalStack(sp - 1)\i
   Protected result.s = ""
   If node
      result = GetXMLNodeText(node)
   EndIf
   gEvalStack(sp - 1)\ss = result
   gEvalStack(sp - 1)\i  = Len(result)
   pc + 1
EndProcedure

; xmlattr(node, attrname) - get attribute value (empty if not found)
Procedure C2BUILTIN_XMLATTR()
   vm_DebugFunctionName()
   Protected node.i = gEvalStack(sp - 2)\i
   Protected name.s = gEvalStack(sp - 1)\ss
   If node
      gEvalStack(sp - 2)\ss = GetXMLAttribute(node, name)
      gEvalStack(sp - 2)\i  = Len(gEvalStack(sp - 2)\ss)
   Else
      gEvalStack(sp - 2)\ss = ""
      gEvalStack(sp - 2)\i  = 0
   EndIf
   sp - 1
   pc + 1
EndProcedure

; xmltype(node) - node type: 1=element, 2=text, 4=comment, 8=cdata, 16=instruction
Procedure C2BUILTIN_XMLTYPE()
   vm_DebugFunctionName()
   Protected node.i = gEvalStack(sp - 1)\i
   If node
      gEvalStack(sp - 1)\i = XMLNodeType(node)
   Else
      gEvalStack(sp - 1)\i = 0
   EndIf
   pc + 1
EndProcedure

; xmlcreate() - create new empty XML document, returns handle
Procedure C2BUILTIN_XMLCREATE()
   vm_DebugFunctionName()
   Protected h.i = 0
   Protected placeholder.i = 0
   ; Bootstrap: ParseXML gives a valid MainXMLNode(); delete placeholder to start empty
   h = ParseXML(#PB_Any, "<cx_root/>")
   If IsXML(h)
      placeholder = ChildXMLNode(MainXMLNode(h))
      If placeholder : DeleteXMLNode(placeholder) : EndIf
   EndIf
   gEvalStack(sp)\i = h
   sp + 1
   pc + 1
EndProcedure

; xmladdnode(parent, name) - create named child element under parent, returns new node
Procedure C2BUILTIN_XMLADDNODE()
   vm_DebugFunctionName()
   Protected parent.i = gEvalStack(sp - 2)\i
   Protected name.s   = gEvalStack(sp - 1)\ss
   Protected node.i   = 0
   If parent
      node = CreateXMLNode(parent, name)
   EndIf
   gEvalStack(sp - 2)\i = node
   sp - 1
   pc + 1
EndProcedure

; xmlsettext(node, text) - set element text content
Procedure C2BUILTIN_XMLSETTEXT()
   vm_DebugFunctionName()
   Protected node.i = gEvalStack(sp - 2)\i
   Protected text.s = gEvalStack(sp - 1)\ss
   If node
      SetXMLNodeText(node, text)
   EndIf
   sp - 2
   pc + 1
EndProcedure

; xmlsetattr(node, name, value) - set attribute on element
Procedure C2BUILTIN_XMLSETATTR()
   vm_DebugFunctionName()
   Protected node.i  = gEvalStack(sp - 3)\i
   Protected name.s  = gEvalStack(sp - 2)\ss
   Protected value.s = gEvalStack(sp - 1)\ss
   If node
      SetXMLAttribute(node, name, value)
   EndIf
   sp - 3
   pc + 1
EndProcedure

; xmlexport(handle) - serialize XML to string (includes <?xml?> declaration)
Procedure C2BUILTIN_XMLEXPORT()
   vm_DebugFunctionName()
   Protected h.i = gEvalStack(sp - 1)\i
   If IsXML(h)
      gEvalStack(sp - 1)\ss = ComposeXML(h)
      gEvalStack(sp - 1)\i  = Len(gEvalStack(sp - 1)\ss)
   Else
      gEvalStack(sp - 1)\ss = ""
      gEvalStack(sp - 1)\i  = 0
   EndIf
   pc + 1
EndProcedure

;- End Built-in Functions

; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 22
; FirstLine = 4
; Folding = --
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; EnablePurifier
; EnableCompileCount = 0
; EnableBuildCount = 0
; EnableExeConstant
