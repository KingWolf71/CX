; Define a structure to hold each macro’s data
Structure stMacro
  name.s           ; Macro name
  params.s         ; Comma-separated parameter names ("" if none)
  body.s           ; The replacement text
EndStructure

; A global Map from macro name to stMacro
Global NewMap mapMacros.stMacro()

Procedure ParseMacroDefinition(line.s)
  ; Skip leading "#define "
  If Left(line, 7) <> "#define" : ProcedureReturn #False : EndIf
  Protected p = Mid(line, 8)        ; Text after "#define"

  ; Extract macro name: letters, digits, or underscore
  Protected nameEnd = 0
  While p[nameEnd] <> 0 And FindString("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", Mid(p, nameEnd+1,1), 1)
    nameEnd + 1
  Wend
  Protected name.s = Left(p, nameEnd)

  ; Initialize new macro record
  Define m.stMacro
  m\name = name

  ; Move past name and whitespace
  p = Mid(p, nameEnd + 1)
  p = Trim(p)

  ; If next char is '(', parse parameter list
  If Left(p,1) = "("
    Protected depth = 0, i = 1, start = 2
    Repeat
      If Mid(p,i,1) = "(" : depth + 1
      ElseIf Mid(p,i,1) = ")" : depth - 1
      EndIf
      i + 1
    Until depth < 0 Or p[i] = 0

    ; params between positions 2 and i-2
    m\params = Trim(Mid(p, 2, i-3))
    p = Mid(p, i)  ; remainder after ')'
  EndIf

  ; The rest is the macro body
  m\body = Trim(p)

  ; Store in map
  mapMacros(m\name) = m
  Debug "Stored macro: " + m\name + "(" + m\params + ") → " + m\body
  ProcedureReturn #True
EndProcedure

Procedure.s ExpandMacros(input.s)
  Define output.s = ""
  Protected p = 1, lenInput = Len(input)

  While p <= lenInput
    ; If identifier start
    If FindString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", Mid(input,p,1), 1)
      ; Read full identifier
      Protected start = p
      While p <= lenInput And FindString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_", Mid(input,p,1), 1)
        p + 1
      Wend
      Protected ident.s = Mid(input, start, p - start)

      ; Macro lookup
      If FindMapElement(mapMacros(), ident)
        Define m.stMacro = mapMacros(ident)
        ; If function-like, parse args
        If Left(Mid(input,p,1),1) = "(" And m\params <> ""
          ; Consume '('
          p + 1
          Protected depth = 0, argStart = p
          Protected args.s() = StringField(m\params, ",") ; placeholder sizes
          Protected argList.s = "", argCount = 0

          ; Build argument list by counting parentheses
          While p <= lenInput
            If Mid(input,p,1) = "(" : depth + 1
            ElseIf Mid(input,p,1) = ")" 
              If depth = 0 : Exit
              depth - 1
            ElseIf Mid(input,p,1) = "," And depth = 0
              ; Split argument
              argList + Mid(input, argStart, p-argStart) + #LF$
              argStart = p + 1
            EndIf
            p + 1
          Wend
          ; Last argument
          argList + Mid(input, argStart, p-argStart)

          ; Split into array
          Define argArray.s() = StringField(argList, #LF$)

          ; Substitute parameters in body
          Define expanded.s = m\body
          For i = 0 To ArraySize(argArray()) - 1
            expanded = ReplaceString(expanded, StringField(m\params, i+1, ","), Trim(argArray(i)))
          Next i

          ; Recursively expand inside
          expanded = ExpandMacros(expanded)
          output + expanded
          p + 1 ; skip ')'
          Continue
        Else
          ; Object-like macro or no args
          output + ExpandMacros(m\body)
          Continue
        EndIf
      EndIf

      ; Not a macro: just copy the identifier
      output + ident
      Continue
    EndIf

    ; Otherwise copy single character
    output + Mid(input,p,1)
    p + 1
  Wend

  ProcedureReturn output
EndProcedure

; IDE Options = PureBasic 6.20 (Windows - x64)
; CursorPosition = 46
; FirstLine = 39
; Folding = -
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; EnablePurifier
; EnableCompileCount = 4
; EnableBuildCount = 0
; EnableExeConstant