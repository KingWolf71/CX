; PureBasic Lexical Analyzer for simple C-like language
; Reads from file (first parameter) or stdin, writes tokens to stdout

; Token types enumeration
Enumeration TokenType
  #End_of_input
  #Op_multiply
  #Op_divide
  #Op_mod
  #Op_add
  #Op_subtract
  #Op_negate
  #Op_less
  #Op_lessequal
  #Op_greater
  #Op_greaterequal
  #Op_equal
  #Op_notequal
  #Op_not
  #Op_assign
  #Op_and
  #Op_or
  #Keyword_if
  #Keyword_else
  #Keyword_while
  #Keyword_print
  #Keyword_putc
  #LeftParen
  #RightParen
  #LeftBrace
  #RightBrace
  #Semicolon
  #Comma
  #Identifier
  #Integer
  #String
EndEnumeration

; Names for printing
Global Dim TokenName$(#String)
TokenName$(#End_of_input)       = "End_of_input"
TokenName$(#Op_multiply)        = "Op_multiply"
TokenName$(#Op_divide)          = "Op_divide"
TokenName$(#Op_mod)             = "Op_mod"
TokenName$(#Op_add)             = "Op_add"
TokenName$(#Op_subtract)        = "Op_subtract"
TokenName$(#Op_negate)          = "Op_negate"
TokenName$(#Op_less)            = "Op_less"
TokenName$(#Op_lessequal)       = "Op_lessequal"
TokenName$(#Op_greater)         = "Op_greater"
TokenName$(#Op_greaterequal)    = "Op_greaterequal"
TokenName$(#Op_equal)           = "Op_equal"
TokenName$(#Op_notequal)        = "Op_notequal"
TokenName$(#Op_not)             = "Op_not"
TokenName$(#Op_assign)          = "Op_assign"
TokenName$(#Op_and)             = "Op_and"
TokenName$(#Op_or)              = "Op_or"
TokenName$(#Keyword_if)         = "Keyword_if"
TokenName$(#Keyword_else)       = "Keyword_else"
TokenName$(#Keyword_while)      = "Keyword_while"
TokenName$(#Keyword_print)      = "Keyword_print"
TokenName$(#Keyword_putc)       = "Keyword_putc"
TokenName$(#LeftParen)          = "LeftParen"
TokenName$(#RightParen)         = "RightParen"
TokenName$(#LeftBrace)          = "LeftBrace"
TokenName$(#RightBrace)         = "RightBrace"
TokenName$(#Semicolon)          = "Semicolon"
TokenName$(#Comma)              = "Comma"
TokenName$(#Identifier)         = "Identifier"
TokenName$(#Integer)            = "Integer"
TokenName$(#String)             = "String"

; Buffer and position tracking
Global Buffer.s
Global BufPos.i, BufLen.i
Global CurrChar.s
Global Line.i = 1, Column.i = 0

; Read entire input into Buffer
Procedure LoadInput()
  Protected f
  If CountProgramParameters() > 0
    If ReadFile(f, ProgramParameter(0)) = 0
      MessageRequester("Error", "Cannot open file: " + ProgramParameter(0))
      End
    EndIf
    While Eof(f) = #False
      Buffer + ReadString(f)
      Buffer + Chr(10)
    Wend
    CloseFile(f)
  Else
    ; Read stdin
    ;While Not Eof(#PB_StandardInput)
    ;  Buffer + ReadString(#PB_StandardInput, 1024)
    ;Wend
  EndIf
  BufLen = Len(Buffer)
  BufPos = 1
EndProcedure

; Advance to next character
Procedure.s NextChar()
  If BufPos > BufLen
    CurrChar = Chr(0)
  Else
    CurrChar = Mid(Buffer, BufPos, 1)
    BufPos + 1
  EndIf
  If CurrChar = Chr(10)
    Line + 1
    Column = 0
  Else
    Column + 1
  EndIf
  ProcedureReturn CurrChar
EndProcedure

; Peek next character without consuming
Procedure.s PeekChar()
  If BufPos > BufLen
    ProcedureReturn Chr(0)
  Else
    ProcedureReturn Mid(Buffer, BufPos, 1)
  EndIf
EndProcedure

; Emit a token
Procedure AddToken(type.i, value.s, startLine.i, startCol.i)
  Protected OUT.s = Str(startLine) + " " + Str(startCol) + " " + TokenName$(type)
  If Len(value) > 0
    OUT + " " + value
  EndIf
  PrintN(OUT)
EndProcedure

; Report error and exit
Procedure Error(msg.s)
  PrintN("Error at line " + Str(Line) + ": " + msg)
  End
EndProcedure

; Main lexing loop
Procedure Lex()
  Protected startLine.i, startCol.i, ident.s, num.s, raw.s
  ; Prime first character
  NextChar()
  While #True
    ; Skip whitespace and comments
    While CurrChar <= " " Or (CurrChar = "/" And PeekChar() = "*")
      If CurrChar <= " "
        NextChar()
      ElseIf CurrChar = "/" And PeekChar() = "*"
        NextChar(): NextChar() ; skip '/*'
        While Not (CurrChar = "*" And PeekChar() = "/")
          If CurrChar = Chr(0)
            Error("EOF in comment")
          EndIf
          NextChar()
        Wend
        NextChar(): NextChar() ; skip '*/'
      EndIf
    Wend
    If CurrChar = Chr(0)
      Break
    EndIf
    
    startLine = Line: startCol = Column
    ; Identifier or keyword
    If (CurrChar >= "A" And CurrChar <= "Z") Or (CurrChar >= "a" And CurrChar <= "z") Or CurrChar = "_"
      ident = CurrChar
      NextChar()
      While (CurrChar >= "A" And CurrChar <= "Z") Or (CurrChar >= "a" And CurrChar <= "z") Or (CurrChar >= "0" And CurrChar <= "9") Or CurrChar = "_"
        ident + CurrChar
        NextChar()
      Wend
      Select ident
        Case "if":      AddToken(#Keyword_if, "", startLine, startCol)
        Case "else":    AddToken(#Keyword_else, "", startLine, startCol)
        Case "while":   AddToken(#Keyword_while, "", startLine, startCol)
        Case "print":   AddToken(#Keyword_print, "", startLine, startCol)
        Case "putc":    AddToken(#Keyword_putc, "", startLine, startCol)
        Default:        AddToken(#Identifier, ident, startLine, startCol)
      EndSelect
      Continue
    EndIf
    ; Integer literal
    If CurrChar >= "0" And CurrChar <= "9"
      num = CurrChar
      NextChar()
      While CurrChar >= "0" And CurrChar <= "9"
        num + CurrChar
        NextChar()
      Wend
      ; Check invalid trailing
      If (CurrChar >= "A" And CurrChar <= "Z") Or (CurrChar >= "a" And CurrChar <= "z") Or CurrChar = "_"
        Error("Invalid number literal")
      EndIf
      AddToken(#Integer, num, startLine, startCol)
      Continue
    EndIf
    ; Character literal
    If CurrChar = "'"
      raw = CurrChar
      NextChar()
      If CurrChar = Chr(0) Or CurrChar = Chr(10)
        Error("Unterminated char literal")
      EndIf
      ; Escape or single char
      If CurrChar = "\\" And PeekChar() = "n"
        raw + CurrChar
        NextChar()
        raw + CurrChar
        NextChar()
      Else
        raw + CurrChar
        NextChar()
      EndIf
      If CurrChar <> "'"
        Error("Invalid char literal")
      EndIf
      raw + CurrChar
      NextChar()
      AddToken(#Integer, raw, startLine, startCol)
      Continue
    EndIf
    ; String literal
    If CurrChar = '"'
      raw = CurrChar
      NextChar()
      While CurrChar <> '"'
        If CurrChar = Chr(0)
          Error("Unterminated string literal")
        EndIf
        ; Handle escape in string
        If CurrChar = '\\' And PeekChar() = "n"
          raw + CurrChar
          NextChar()
          raw + CurrChar
          NextChar()
        ElseIf CurrChar = '\\' And PeekChar() = "\\"
          raw + CurrChar
          NextChar()
          raw + CurrChar
          NextChar()
        Else
          raw + CurrChar
          NextChar()
        EndIf
      Wend
      raw + CurrChar ; closing quote
      NextChar()
      AddToken(#String, raw, startLine, startCol)
      Continue
    EndIf
    ; Operators and symbols
    Select CurrChar
      Case "<"
        If PeekChar() = "="
          NextChar(): NextChar(): AddToken(#Op_lessequal, "", startLine, startCol)
        Else
          NextChar(): AddToken(#Op_less, "", startLine, startCol)
        EndIf
      Case ">"
        If PeekChar() = "="
          NextChar(): NextChar(): AddToken(#Op_greaterequal, "", startLine, startCol)
        Else
          NextChar(): AddToken(#Op_greater, "", startLine, startCol)
        EndIf
      Case "="
        If PeekChar() = "="
          NextChar(): NextChar(): AddToken(#Op_equal, "", startLine, startCol)
        Else
          NextChar(): AddToken(#Op_assign, "", startLine, startCol)
        EndIf
      Case "!"
        If PeekChar() = "="
          NextChar(): NextChar(): AddToken(#Op_notequal, "", startLine, startCol)
        Else
          NextChar(): AddToken(#Op_not, "", startLine, startCol)
        EndIf
      Case "&"
        If PeekChar() = "&"
          NextChar(): NextChar(): AddToken(#Op_and, "", startLine, startCol)
        Else
          Error("Unrecognized '&' usage")
        EndIf
      Case "|"
        If PeekChar() = "|"
          NextChar(): NextChar(): AddToken(#Op_or, "", startLine, startCol)
        Else
          Error("Unrecognized '|' usage")
        EndIf
      Case "*"
        NextChar(): AddToken(#Op_multiply, "", startLine, startCol)
      Case "/"
        NextChar(): AddToken(#Op_divide, "", startLine, startCol)
      Case "%"
        NextChar(): AddToken(#Op_mod, "", startLine, startCol)
      Case "+"
        NextChar(): AddToken(#Op_add, "", startLine, startCol)
      Case "-"
        NextChar(): AddToken(#Op_subtract, "", startLine, startCol)
      Case "("
        NextChar(): AddToken(#LeftParen, "", startLine, startCol)
      Case ")"
        NextChar(): AddToken(#RightParen, "", startLine, startCol)
      Case "{"
        NextChar(): AddToken(#LeftBrace, "", startLine, startCol)
      Case "}"
        NextChar(): AddToken(#RightBrace, "", startLine, startCol)
      Case ";"
        NextChar(): AddToken(#Semicolon, "", startLine, startCol)
      Case ","
        NextChar(): AddToken(#Comma, "", startLine, startCol)
      Default
        ; Unrecognized character
        Error("Unrecognized character '" + CurrChar + "'")
    EndSelect
  Wend
  ; Emit end-of-input at next line
  AddToken(#End_of_input, "", Line + 1, 1)
EndProcedure

; --- Program Entry ---
LoadInput()
Lex()

; IDE Options = PureBasic 6.20 (Windows - x64)
; CursorPosition = 227
; FirstLine = 223
; Folding = --
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; EnablePurifier
; EnableCompileCount = 0
; EnableBuildCount = 0
; EnableExeConstant