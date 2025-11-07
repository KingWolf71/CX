
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
; Common Structures

; ======================================================================================================
;- Constants
; ======================================================================================================

DisableDebugger

DeclareModule C2Common
   XIncludeFile         "c2-inc-v05.pbi"
EndDeclareModule

Module C2Common
   ;Empty by design
EndModule

DeclareModule C2Lang
   EnableExplicit
   #WithEOL = 1   
   UseModule C2Common
 
   Structure stTree
      NodeType.i
      value.s
      *left.stTree
      *right.stTree
   EndStructure
   
   Declare.s            Error( *error.Integer )
   Declare              Compile()
   Declare              ListCode( gadget = 0 )
   Declare              LoadLJ( file.s )
EndDeclareModule

XIncludeFile            "c2-vm-V04.pb"

Module C2Lang
   EnableExplicit
   
; ======================================================================================================
;- Structures
; ======================================================================================================

   #C2REG_FLOATS        = 1
   #C2FUNCSTART         = 2

   Structure stSymbols
      name.s
      TokenType.i
   EndStructure
   
   Structure stToken
      name.s
      TokenType.l
      TokenExtra.l
      value.s
      row.l
      col.l
      function.l
   EndStructure
   
   Structure stPrec
      bRightAssociation.i
      bBinary.i
      bUnary.i
      Precedence.i
      NodeType.i
   EndStructure
   
   Structure stModInfo
      *code.stTree
      function.l
      params.s
      nParams.i
      Index.l
      row.l
      nCall.u
      *NewPos
      bTypesLocked.i  ; Flag: Types locked on first call
      returnType.w    ; Return type flags (INT/FLOAT/STR)
   EndStructure
   
   Structure stMacro
      name.s
      body.s
      List llParams.s()
   EndStructure
   
   Structure stHole
      mode.l
      id.l
      *src
      *location
   EndStructure
   
   ;Structure stCode Extends stType
   ;   *tk.stToken
   ;EndStructure
; ======================================================================================================
;- Globals
; ======================================================================================================
  
   Global Dim           gPreTable.stPrec( #C2MAXTOKENS )
   
   Global NewList       llSymbols.stSymbols()
   Global NewList       llTokenList.stToken()   
   Global NewList       llHoles.stHole()
   Global NewList       llObjects.stType()
   Global NewMap        mapMacros.stMacro()
   Global NewMap        mapModules.stModInfo()
   
   Global               gLineNumber
   Global               gStack
   Global               gCol
   Global               gMemSize
   Global               gPos
   Global               gExit
   Global               gHoles
   Global               gFileFormat
   Global               gFloats
   Global               gIntegers
   Global               gStrings
   Global               gNextFunction
   Global               gCurrFunction
   
   Global               gszFileText.s
   Global               gszlastError.s
   Global               gNextChar.s
   Global               gLastError
   Global               gszEOF.s          = Chr( 255 )
   
   CompilerIf #PB_OS_Linux
      Global            gszSep.s          = #LF$
   CompilerElse
      Global            gszSep.s          = #CRLF$
   CompilerEndIf
      
   ;Global              gszFloating.s = "^[+-]?([0-9]*[.])?[0-9]+$"
   Global               gszFloating.s = "^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$"
   
   CreateRegularExpression( #C2REG_FLOATS, gszFloating )
   
   Declare              paren_expr()
   ;- =====================================
   ;- Generic
   ;- =====================================
   Macro             TOKEN()
      llTokenList()
   EndMacro
   Macro             Install( symbolname, id  )
      AddElement( llSymbols() )
         llSymbols()\name        = symbolname
         llSymbols()\TokenType   = id
   EndMacro
   Macro                SetError( text, err )
      If err > 0 And err < 10
         gszlastError   = text + " on line " + Str( gLineNumber ) + ", col = " + Str( gCol )
      ElseIf err > 10
         gszlastError   = text + " on line " + Str( llTokenList()\row ) + ", col = " + Str( llTokenList()\col )
      Else
         gszlastError   = text
      EndIf
      
      gLastError     = err
      
      ProcedureReturn err
   EndMacro
   
   Macro             NextToken()
      ;Debug "---[ " + #PB_Compiler_Procedure + " ]---"
      ;Debug Str( ListIndex( llTokenList() ) ) + "   " + llTokenList()\name
      NextElement( llTokenList() )
   EndMacro
   ;-
   Procedure.s          Error( *error.Integer )
      Protected         szerror.s
   
      If gLastError
         
         *error\i       = gLastError
         szerror        = gszlastError
         gLastError     = 0
         gszlastError   = ""
   
         ProcedureReturn szerror
      EndIf
      
      *error\i = 0
      ProcedureReturn ""
   EndProcedure
   
   Procedure            Logging( id, Text.s, pos = -1, UseDebug = 1 )
      If UseDebug
         Debug text
      EndIf
   EndProcedure
   ;- =====================================
   ;- Compiler init   
   ;- =====================================
   Macro                par_AddMacro( vname, value )
      AddMapElement( mapMacros(), vname )
         mapMacros()\name  = vname
         mapMacros()\body  = value
   
   EndMacro
   Macro                par_SetPre2( id, op )
      gPreTable( id )\bRightAssociation   = 0
      gPreTable( id )\bBinary             = 0
      gPreTable( id )\bUnary              = 0
      gPreTable( id )\Precedence          = -1
      gPreTable( id )\NodeType            = op
   EndMacro
   Macro                par_SetPre( id, right, bi, un, prec )
      gPreTable( id )\NodeType            = id
      gPreTable( id )\bRightAssociation   = right
      gPreTable( id )\bBinary             = bi
      gPreTable( id )\bUnary              = un
      gPreTable( id )\Precedence          = prec
   EndMacro
   Macro             par_AddTokenSimple( tkentype )
      AddElement( llTokenList() )
         llTokenList()\TokenType = tkentype
         llTokenList()\TokenExtra= tkentype
         llTokenList()\name      = gszATR( tkentype )\s
         llTokenList()\row       = gLineNumber
         llTokenList()\col       = gCol
         llTokenList()\function  = gCurrFunction
   EndMacro
   
   Macro             par_AddToken( tkentype, tkenextra, text, info )
      AddElement( llTokenList() )
         llTokenList()\TokenType = tkentype
         llTokenList()\TokenExtra= tkenextra
         llTokenList()\row       = gLineNumber
         llTokenList()\col       = gCol
         llTokenList()\function  = gCurrFunction
         
         If text = ""
            If tkentype = #ljSTRING
               gStrings + 1
               llTokenList()\name = "_str" + Str(gStrings)
            ElseIf tkentype = #ljINT
               gIntegers + 1
               llTokenList()\name = "_int" + Str(gIntegers)
            ElseIf tkentype = #ljFLOAT
               gFloats + 1
               llTokenList()\name = "_flt" + Str(gFloats)
            Else
               llTokenList()\name = gszATR( tkenextra )\s
            EndIf
         Else
            llTokenList()\name      = text
         EndIf
         
         llTokenList()\value    = info
   EndMacro
   Macro                par_NextCharacter()
      gNextChar = Mid( gszFileText, gPos, 1 )
      gPos + 1 : gCol + 1
      
      If gNextChar = #LF$
         gCol = 1
         gLineNumber + 1
         
         gNextChar = Mid( gszFileText, gPos, 1 )
         gPos + 1
      EndIf
   EndMacro
   ;-
   Procedure            Init()
      Protected         temp.s
      Protected         i, n, m
      
      For i = 0 To #C2MAXCONSTANTS
         gVar(i)\name   = ""
         gVar(i)\ss     = ""
         gVar(i)\p      = 0
         gVar(i)\i      = 0
         gVar(i)\f      = 0.0
         gVar(i)\flags  = 0
      Next
      
      ;Read tokens
      gnTotalTokens = 0
      Restore c2tokens
      
      Repeat
         Read.s temp
         If temp = "-" : Break : EndIf
         gszATR(gnTotalTokens)\s = temp
         Read m
         Read n
         
         gszATR(gnTotalTokens)\strtoken = n
         gszATR(gnTotalTokens)\flttoken = m
         
         gnTotalTokens + 1
      ForEver
   
      ReDim gPreTable.stPrec(gnTotalTokens)
      ReDim gszATR(gnTotalTokens)
   
      par_SetPre2( #ljEOF, -1 )
      par_SetPre( #ljMULTIPLY,     0, 1, 0, 13 )
      par_SetPre( #ljDIVIDE,       0, 1, 0, 13 )
      par_SetPre( #ljMOD,          0, 1, 0, 13 )
      par_SetPre( #ljADD,          0, 1, 0, 12 )
      par_SetPre( #ljSUBTRACT,     0, 1, 0, 12 )
      par_SetPre( #ljNEGATE,       0, 0, 1, 14 )
      par_SetPre( #ljNOT,          0, 0, 1, 14 )
      par_SetPre( #ljLESS,         0, 1, 0, 10 )
      par_SetPre( #ljLESSEQUAL,    0, 1, 0, 10 )
      par_SetPre( #ljGREATER,      0, 1, 0, 10 )
      par_SetPre( #ljGreaterEqual, 0, 1, 0, 10 )
      par_SetPre( #ljEQUAL,        0, 1, 0, 9 )
      par_SetPre( #ljNotEqual,     0, 1, 0, 9 )
      par_SetPre2( #ljASSIGN,      #ljASSIGN )
      par_SetPre( #ljAND,          0, 1, 0, 5 )
      par_SetPre( #ljOr,           0, 1, 0, 4 )
      par_SetPre2( #ljIF,          #ljIF )
      
      par_SetPre2( #ljElse,        -1 )
      par_SetPre2( #ljWHILE,       #ljWHILE )
      par_SetPre2( #ljPRTS,        -1 )
      par_SetPre2( #ljPRTI,        -1 )
      par_SetPre2( #ljPRTC,        -1 )
      par_SetPre2( #ljLeftParent,  -1 )
      par_SetPre2( #ljLeftBrace,   -1 )
      par_SetPre2( #ljRightParent,  -1 )
      par_SetPre2( #ljRightBrace, -1 )
      par_SetPre2( #ljComma,  -1 )
      par_SetPre2( #ljSemi,  -1 )
      par_SetPre2( #ljfunction,  -1 )
      par_SetPre2( #ljreturn,  -1 )
      par_SetPre2( #ljIDENT,  #ljIDENT )
      par_SetPre2( #ljINT,    #ljINT )
      par_SetPre2( #ljSTRING, #ljSTRING )
      par_SetPre2( #ljFLOAT,    #ljFLOAT )
      
      ClearList( llObjects() )
      ClearList( llTokenList() )
      ClearList( llSymbols() )
      ClearMap( mapPragmas() )
      ClearMap( mapMacros() )
      ClearMap( mapModules() )
      ReDim arCode(1)
      
      gLineNumber    = 1
      gCol           = 1
      gStack         = 0
      gExit          = 0
      gHoles         = 0
      gnLastVariable = 0
      gStrings       = 0
      gFloats        = 0
      gIntegers      = 0
      gNextFunction  = #C2FUNCSTART
      
      Install( "else", #ljElse )
      install( "if",    #ljIF )
      install( "print", #ljPRint )
      install( "putc",  #ljPRTC )
      install( "while", #ljWHILE )
      install( "func", #ljfunction )
      install( "return", #ljreturn )
      install( "call", #ljCall )
      
      mapPragmas("console") = "on"
      mapPragmas("appname") = "Untitled"
      mapPragmas("consolesize") = "600x420"
      
      par_AddMacro( "#True", "1" )
      par_AddMacro( "#False", "0" )
      par_AddMacro( "#PI", "3.14159265359" )

      ; default function return cheat
      ;par_AddToken( #ljIDENT, #ljIDENT, "",  "$ret" )
      ;par_AddTokenSimple( #ljASSIGN )
      
      ;par_AddTokenSimple( #ljSEMI )
   EndProcedure
   
   Procedure            LoadLJ( filename.s )
      Protected         f, *mem
   
      gMemSize = FileSize( filename )
      
      If gMemSize > 0
         f = ReadFile( #PB_Any, filename, #PB_File_NoBuffering )
         gFileFormat = ReadStringFormat( f )    
         
         If Not f
            SetError( "Could not open file", -3 )
         EndIf
         
         If gFileFormat <> #PB_Ascii And gFileFormat <> #PB_UTF8 And gFileFormat <> #PB_Unicode
            gFileFormat = #PB_Unicode
         EndIf
         
         *Mem = AllocateMemory( gMemSize + 16, #PB_Memory_NoClear )
         ReadData( f, *Mem, gMemSize )
         CloseFile( f )
         
         CompilerIf( #WithEOL = 1 )
            gszFileText = PeekS( *mem, -1, gFileFormat ) + gszEOF
         CompilerElse
            gszFileText = PeekS( *mem, -1, gFileFormat )
         CompilerEndIf   
            
         gMemSize = Len( gszFileText )
         FreeMemory( *mem )
         ProcedureReturn 0
      EndIf
      
      SetError( "Invalid file", -2 )
   EndProcedure
   
   
   ;- =====================================
   ;- Preprocessors
   ;- =====================================
   Macro                pre_FindNextWord( tsize, withinv, extra )
      p = Trim( Mid( p, tsize ) )
      
      CompilerIf withinv
         temp = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" + #INV$ + extra
      CompilerElse
         temp = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      CompilerEndIf  
      
      Repeat
         If Not FindString( temp, Mid( p, j, 1 ), #PB_String_NoCase ) : Break : EndIf   
         j + 1
      ForEver
   EndMacro
   Macro                pre_TrimWhiteSpace( string )
      Trim( Trim( string ), #TAB$ )
   EndMacro
   Macro                par_AddModule( modname, mparams )
      
      If FindMapElement( mapModules(), modname )
         SetError( "Function already declared", 16 )
      Else
         AddMapElement( mapModules(), modname )
         mapModules()\function      = gNextFunction
         mapModules()\NewPos        = 0
         mapModules()\params        = mparams
         mapModules()\nParams       = -1
         gNextFunction + 1
         
         Debug "New function: " + modname
      EndIf
   EndMacro
   ;-
   ; We parse for #pragmas and function calls as well as macros
   Procedure            ParseFunctions( line.s, row.i )
      Protected.s       temp, nc, name, p, params
      Protected         i, j
      
      i     = 1 : j = 1
      p     = pre_TrimWhiteSpace( line )
   
      If FindString( p, "func", #PB_String_NoCase ) = 1
         ;It's probably a function
         i + 4
         pre_FindNextWord( 5, 0, "" )
         name  = Left( p, j - 1 )
         temp  = "_" + LCase( name )
         p     = Mid( p, j )
         i = Len( p )
         
         If Mid( p, i, 1 ) = #CR$
            p = Left( p, i - 1 )
         EndIf
         
         p = pre_TrimWhiteSpace( p )
         If Mid( p, 1, 1) = "("
            ; definetely a function
            par_AddModule( temp, p )
            mapModules()\row = row
         EndIf
      EndIf
   EndProcedure
   
   Procedure            ParseDefinitions( line.s )
      Protected         bInv, Bracket
      Protected         i, j
      Protected         tmpMod.stModInfo
      Protected.s       temp, nc, name, p, param
      Protected         depth = 0, start = 2
      Protected         mret = #True
      
      i     = 1 : j = 1
      p     = pre_TrimWhiteSpace( line )
      
      ; It has to be at the beginning 
      If FindString( p, "#pragma", #PB_String_NoCase ) = 1
         pre_FindNextWord( 8, 1, "" )
         name  = LCase( Trim( Left( p, j - 1 ), #INV$ ) )
         p = Mid( p, j + 1 )
         i = Len( p )
         
         If Mid( p, i, 1 ) = #CR$
            p = Trim( Left( p, i - 1 ) )
         EndIf
         
         p = pre_TrimWhiteSpace( p )
         param = Trim( p, #INV$ )
         AddMapElement( mapPragmas(), name )
         mapPragmas() = param
         mret         = #False
         ;Debug name + " --> [" + param + "]," + Str(Len( param))
      ; It has to be at the beginning
      ElseIf FindString( p, "#define", #PB_String_NoCase ) = 1
         pre_FindNextWord( 8, 0, "" )
         name  = Left( p, j - 1 )
         temp  = UCase( name )
         
         AddMapElement( mapMacros(), temp )
         p     = Trim( Mid( p, j ) )
          
         If Left( p, 1 ) = "("
            Repeat
               i + 1
               nc = Mid( p, i, 1 )
               If nc = "" : Break : EndIf
               If nc = "(" : depth + 1
               ElseIf nc = ")" : depth - 1
               EndIf
            Until depth < 0

            ; params between positions 2 and i-2
            temp = Trim( Mid( p, 2, i - 2 ) )
            j = 1
                 
            Repeat
               nc = StringField( temp, j, "," )
               If nc = "" : Break : EndIf
               AddElement( mapMacros()\llParams() )
               mapMacros()\llParams() = Trim( nc )
               j + 1
            ForEver
            
            ;mapMacros()\strparams = temp
            p = Trim ( Mid( p, i + 1 ) ) ; remainder after ')'
         EndIf
         
         mapMacros()\name  = name
         mapMacros()\body  = p
         mret              = #False
      EndIf
      
      ProcedureReturn mret
   EndProcedure
   
   Procedure.s          ExpandMacros( line.s )
      Protected.s       output, temp
      Protected.s       ident, expanded
      Protected.i       depth, argStart
      Protected.i       i
      Protected         m.stMacro
      Protected         p = 1
      Protected         lenInput, start
      Protected NewList ll.s()

      lenInput = Len( line )
      output   = ""
      
      If Mid( line, lenInput , 1 ) = #CR$
         lenInput - 1
      EndIf
      
      While p <= lenInput
         ; If identifier start
         If FindString( "#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", Mid( line, p, 1 ), 1 )
            start = p
         
            While p <= lenInput And FindString( "#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_", Mid( line, p, 1 ), 1 )
              p + 1
            Wend
            
            ident    = Mid( line, start, p - start )
            temp     = UCase( ident )
      
            ; Macro lookup
            If FindMapElement( mapMacros(), temp )
               m = mapMacros()
               
               ; If function-like, parse args
               If Left( Mid( line, p, 1 ), 1 ) = "(" And ListSize( m\llParams() )
                  ; Consume '('
                  p + 1 : depth = 0
                  argStart = p
                  ClearList( ll() )
      
                  ; Build argument list by counting parentheses
                  While p <= lenInput
                     If Mid( line, p, 1 ) = "(" : depth + 1
                     ElseIf Mid( line, p, 1 ) = ")" 
                        If depth = 0 : Break : EndIf
                        depth - 1
                     ElseIf Mid( line, p, 1 ) = "," And depth = 0
                        ; Split argument
                        AddElement( ll() )
                        ll() = Trim( Mid( line, argStart, p - argStart ) )
                        argStart = p + 1
                     EndIf
                  
                     p + 1
                  Wend
                  
                  ; Last argument
                  AddElement( ll() )
                  ll() = Trim( Mid( line, argStart, p - argStart ) )
                  ; Substitute parameters in body
                  expanded = m\body
                  FirstElement( m\llParams() )
                  
                  ForEach ll()
                     expanded = ReplaceString( expanded, m\llParams(), ll() )
                     NextElement( m\llParams() )
                  Next
                  
                  ; Recursively expand inside
                  expanded = ExpandMacros( expanded )
                  output + expanded
                  p + 1 ; skip ')'
                 Continue
               Else
                  ; Object-like macro or no args
                  output + ExpandMacros( m\body )
                  Continue
               EndIf
            EndIf
      
            ; Not a macro: just copy the identifier
            output + ident
            Continue
         EndIf
      
         ; Otherwise copy single character
         output + Mid( line, p, 1 )
         p + 1
      Wend

         ;Debug output + "<--"
      ProcedureReturn output
   EndProcedure
   
   ; Finds and expands macros and functions
   Procedure            Preprocessor()
      Protected         i
      Protected         bFlag
      Protected.s       line
      Protected.s       szNewBody = ""
      
      ; First we find and store our macros
      Repeat
         i + 1 : bFlag = #True
         line = StringField( gszFileText, i, gszSep )
         If line = "" : Break : EndIf
         
         If FindString( line, "#define", #PB_String_NoCase ) Or FindString( line, "#pragma", #PB_String_NoCase )
            bFlag = ParseDefinitions( line )
         EndIf
         
         If bFlag = #True
            szNewBody + line + #LF$
         Else
            ;Debug mapMacros()\name + " --> " + mapMacros()\strparams + " --> " + mapMacros()\body
         EndIf
      ForEver
      
      ; Macro Expansion
      i = 0 : gszFileText = ""
      
      Repeat
         i + 1
         line = StringField( szNewBody, i, #LF$ )
         If line = "" : Break : EndIf
         
         Line = ExpandMacros( line) + #CRLF$
         gszFileText + line
      ForEver
      
      ; Find Functions
      i = 0
      
      Repeat
         i + 1
         line = StringField( gszFileText, i, #LF$ )
         If line = "" : Break : EndIf
         gszFileText + line
         
         If FindString( line, "func", #PB_String_NoCase )
            ParseFunctions( line, i )
         EndIf
      ForEver
      
      ;Debug gszFileText
      gMemSize = Len( gszFileText )
   EndProcedure
   ;- =====================================
   ;- Parser 
   ;- =====================================
   ; PureBasic procedure to detect if a string represents an Integer, Float, or neither (String)
   Macro                par_DebugParser()
      Debug "---[ Parser ]--------------"
   
      ForEach llTokenList()
         temp = RSet( Str(llTokenList()\row), 6 ) + "   " + RSet( Str(llTokenList()\col), 6 ) + "   "
         
         If llTokenList()\TokenExtra <> llTokenList()\TokenType
            temp + LSet( gszATR( llTokenList()\TokenType )\s + "_" + llTokenList()\name, 34 ) + llTokenList()\value
         Else
            temp + LSet( llTokenList()\name, 34 ) + llTokenList()\value
         EndIf
         
         If llTokenList()\function >= #C2FUNCSTART
            temp +  RSet( Str( llTokenList()\function ), 6 )
         EndIf
         
         Debug temp
      Next
   EndMacro             
   ;-
   Procedure            DetectType( Input.s )
      Protected.s       s = Trim(Input)
      Protected.s       c
      Protected.b       isInteger = #True
      Protected.i       i
      
      If s = ""
         ; Empty string considered as String type
         ProcedureReturn #ljSTRING
      EndIf

      ; Check integer: optional leading + or -, followed by digits only
   
      For i = 1 To Len(s)
         c = Mid( s, i, 1 )
         If i = 1 And ( c = "+" Or c = "-" )
            Continue ; sign is allowed at first position
         ElseIf c >= "0" And c <= "9"
            Continue ; digit is allowed
         Else
            isInteger = #False
            Break
         EndIf
      Next i
      
      If isInteger = #True
         ProcedureReturn #ljINT
      EndIf
   
      ; Check float: optional leading + or -, one decimal point, digits around it
      Protected         dotCount.i = 0
      Protected         digitCount.i = 0
      Protected         hasDigitBeforeDot.b = #False
      Protected         hasDigitAfterDot.b = #False
   
      For i = 1 To Len(s)
         c = Mid( s, i, 1 )
         If c = "."
            dotCount + 1
            If dotCount > 1
               ; more than one decimal point -> not a valid float
               dotCount = -1
               Break
            EndIf
         ElseIf i = 1 And ( c = "+" Or c = "-" )
            Continue ; sign allowed at first position
         ElseIf c >= "0" And c <= "9"
            digitCount + 1
            If dotCount = 0
               hasDigitBeforeDot = #True
            Else
               hasDigitAfterDot = #True
            EndIf
         Else
            ; invalid character for float
            dotCount = -1
            Break
         EndIf
      Next i
   
      If dotCount = 1 And hasDigitBeforeDot And hasDigitAfterDot
         ProcedureReturn #ljFLOAT
      EndIf
   
   ; If not integer or float, treat as string
   ProcedureReturn #ljSTRING
EndProcedure
   
   Procedure            IsNumber( init.i = 0 )
      Static            flag
      
      If init
         flag = 0
      Else
         If gNextChar >= "0" And gNextChar <= "9"
            ProcedureReturn 1
         ElseIf Not flag And gNextChar = ".'"
            flag + 1
            ProcedureReturn 1
         EndIf
      EndIf
   
      ProcedureReturn 0
   EndProcedure
   
   Procedure            IsAlpha()
      If ( gNextChar >= "a" And gNextChar <= "z" ) Or (gNextChar >= "A" And gNextChar <= "Z"  ) Or IsNumber()
         ProcedureReturn 1
      EndIf
      
      ProcedureReturn 0
   EndProcedure
   
   Procedure            Follow( expect.s, ifyes.i, ifno.i, *err.Integer )
      par_NextCharacter()
      
      If gNextChar = expect
         par_AddToken( #ljOP, ifyes, "", "" )
      Else
         If ifno = -1
            *err\i = 5
            SetError( "follow unrecognized character", 5 )
         Else
            par_AddToken( #ljOP, ifno, "", ""  )
            gPos - 1
         EndIf
      EndIf
      
      ProcedureReturn 0
   EndProcedure
   ; Reads character by character creating tokens used by the syntax checker and code generator
   Procedure            Scanner()
      Protected         err, first, i
      Protected.i       dots, bFloat, e
      Protected.i       braces
      Protected.s       text, temp
      
      gpos           = 1
      gCurrFunction  = 1
      
      While gPos <= gMemSize
         par_NextCharacter()
         
         Select gNextChar
            Case gszEOF
               par_AddTokenSimple( #ljEOF )
               Break

            Case " ", #CR$, #TAB$, ""
               Continue
            
            Case "{"
               braces + 1
               par_AddTokenSimple( #ljLeftBrace )
               
            Case "}"
               braces - 1
               par_AddTokenSimple( #ljRightBrace )
               If braces = 0 : gCurrFunction = 1 : EndIf
               
            Case "("
               par_AddTokenSimple( #ljLeftParent )
            Case ")"
               par_AddTokenSimple( #ljRightParent )
            Case "+"
               par_AddToken( #ljOP, #ljADD, "", "" )
            Case "-"
               par_AddToken( #ljOP, #ljSUBTRACT, "", "" )
            Case "*"
               par_AddToken( #ljOP, #ljMULTIPLY, "", "" )
            Case "%"    
               par_AddToken( #ljOP, #ljMOD, "", "" )
            Case ";"
               par_AddTokenSimple( #ljSemi )
            Case ","
               par_AddTokenSimple( #ljComma )
            Case "/"
               par_NextCharacter()
               
               If gNextChar = "/"    ;Single line comment  
                  Repeat
                     par_NextCharacter()
                     
                     If gNextChar = #CR$
                        Break
                     EndIf
                  Until gpos > gMemSize
                  
                  If gpos > gMemSize
                     SetError( "EOF in comment", 1 )
                  EndIf
               ElseIf gNextChar <> "*"
                  par_AddToken( #ljOP, #ljDIVIDE, "", "" )
                  gPos - 1
               Else        ;Must be a comment
                  
                  par_NextCharacter()
                  text = ""
                  
                  Repeat
                     par_NextCharacter()
                     text + gNextChar
                     
                     If gNextChar = "*"
                        par_NextCharacter()
                        
                        If gNextChar = "/"
                           Break
                        EndIf
                     EndIf

                  Until gPos >= gMemSize
                  
                  If gPos >= gMemSize
                     SetError( "EOF in comment", 1 )
                  EndIf

               EndIf
            Case "'"
               par_NextCharacter()
               
               If gNextChar = "'"
                  SetError( "Empty character literal", 2 )
               ElseIf gNextChar = "\"
                  par_NextCharacter()
                  
                  Select gNextChar
                     Case "'"
                        SetError( "Empty escape character literal", 2 )
                     Case "n"
                        first = 10
                     Case "r"
                        first = 13
                     Case "\"
                        first = 92
                     Default
                        SetError( "Invalid escape character", 3 )
                  EndSelect
               Else
                  first = Asc( gNextChar )
               EndIf
               
               par_NextCharacter()
               
               If gNextChar <> "'"
                  SetError( "Multi-Character literal", 4 )
               Else
                  par_AddToken( #ljINT, #ljINT, "", Str(first) )
               EndIf
               
            Case "<"
               If Follow( "=", #ljLESSEQUAL, #ljLESS, @err ) : ProcedureReturn err : EndIf
            Case ">"
               If Follow( "=", #ljGreaterEqual, #ljGREATER, @err ) : ProcedureReturn err : EndIf
            Case "!"
               If Follow( "=", #ljNotEqual, #ljNOT, @err ) : ProcedureReturn err : EndIf
            Case "="
               If Follow( "=", #ljEQUAL, #ljASSIGN, @err ) : ProcedureReturn err : EndIf
            Case "&"
               If Follow( "&", #ljAND, -1, @err ) : ProcedureReturn err : EndIf
            Case "|"
               If Follow( "|", #ljOr, -1, @err ) : ProcedureReturn err : EndIf
            Case "%"
               If Follow( "%%", #ljxOr, -1, @err ) : ProcedureReturn err : EndIf
            
            Case #INV$
               par_NextCharacter()
               text = gNextChar
               
               Repeat
                  par_NextCharacter()
                  
                  If gNextChar = #INV$
                     ;text + gNextChar
                     e = DetectType( text )
                     par_AddToken( e, e, "", text )
                     Break
                  ElseIf gNextChar = #CR$
                     SetError( "EOL in string", 7 )
                  Else
                     text + gNextChar
                  EndIf
               
               Until gPos >= gMemSize
               
               If gPos >= gMemSize
                  SetError( "EOF in string", 6 )
               EndIf
            Default
               IsNumber( 1 )        ; reset digit flag
               
               first    = IsNumber()
               text     = ""
               dots     = 0
               bFloat   = 0
               e        = 0
               
               While gPos < gMemSize And ( IsAlpha() Or gNextChar = "_" Or gNextChar = "." )
                  If gNextChar = "." : dots + 1 : EndIf
                  If gNextChar = "e" Or gNextChar = "E" : e + 1 : EndIf
                  If Not IsNumber() : first = 0 : EndIf
                  text + gNextChar
                  par_NextCharacter()
               Wend
 
               If gPos >= gMemSize
                  SetError( "EOL in number or variable '" + text + "'", 8 )
               EndIf
               
               If Len( text ) < 1
                  SetError( "Unknown sequence number or variable '" + text + "'", 9 )
               EndIf
               
               gPos - 1
               i = 0
               
               If (dots Or e) And MatchRegularExpression( #C2REG_FLOATS , text )
                  bFloat = 1
                  ;Debug text + " is a float."
               Else
                  ;Debug text + " Not float."
               EndIf
               
               If bFloat
                  par_AddToken( #ljFLOAT, #ljFLOAT, "", text )
               Else
                  temp = LCase( text )
                  ; Is it a function?
                  
                  If FindMapElement( mapModules(), "_" + temp )
                     If mapModules()\row = gLineNumber And TOKEN()\TokenType = #ljFunction
                        gCurrFunction     = mapModules()\function
                        TOKEN()\function  = gCurrFunction
                        TOKEN()\value     = Str( gCurrFunction )
                     EndIf
                     
                     par_AddToken( #ljCall, #ljCall, "", Str( mapModules()\function ) )
                  Else
                     ForEach llSymbols()
                        i + 1
                        
                        If llSymbols()\name = temp
                           ;Debug "SYMBOL: " + temp
                           par_AddToken( llSymbols()\TokenType, llSymbols()\TokenType, "", text )
                           i = -1
                           Break
                        EndIf
                     Next
                     
                     If i > 0
                        If first
                           par_AddToken( #ljINT, #ljINT, "", text )
                        Else
                           par_AddToken( #ljIDENT, #ljIDENT, "", text )
                        EndIf
                     EndIf
                  EndIf
               EndIf
         EndSelect
      Wend
      
      ProcedureReturn 0
   
   EndProcedure
   Procedure            ReorderTokens()
      Protected NewList llTemp.stToken()
      
      CopyList( llTokenList(), lltemp() )
      ClearList( llTokenList() )
      ; We need to put non function tokens at the top so all functions start after code end
      
      ForEach llTemp()
         If llTemp()\TokenType = #ljEOF
            ;Skip
         ElseIf llTemp()\function < #C2FUNCSTART
            AddElement( llTokenList() )
            llTokenList() = llTemp()
         EndIf
      Next
   
      par_AddTokenSimple( #ljHalt )
   
      ForEach llTemp()
         If llTemp()\function >= #C2FUNCSTART
            AddElement( llTokenList() )
            llTokenList() = llTemp()
         EndIf
      Next
      
      par_AddTokenSimple( #ljEOF )
      par_AddToken( #ljINT,    #ljINT, "10",  "10" )
      par_AddToken( #ljSTRING, #ljSTRING, "NULL", "" )
      par_AddToken( #ljINT,    #ljINT, "-1", "-1" )
      par_AddToken( #ljINT,   #ljINT,   "0", "0" )
   EndProcedure
   ;- =====================================
   ;- Syntax Analyzer
   ;- =====================================  
   Procedure            Expect( function.s, TokenType )
      
      ;Debug "--Expect--"
      ;Debug TOKEN()\name + " --> " + gszATR( TokenType )
      
      If TOKEN()\TokenExtra = TokenType
         NextToken()
         ProcedureReturn 0
      EndIf
      
      SetError( "Expecting " + gszATR( TokenType )\s + " but found " + gszATR( TOKEN()\TokenExtra )\s + " for " + function, 11 )
   
   EndProcedure
   
   Procedure            MakeNode( NodeType, *left.stTree, *right.stTree )
      Protected         *p.stTree = AllocateStructure( stTree )
      
      *p\NodeType = NodeType
      *p\left     = *left
      *p\right    = *right
      
      ProcedureReturn *p
   EndProcedure
   
   Procedure            Makeleaf( NodeType, value.s )
      Protected         *p.stTree = AllocateStructure( stTree )
      
      *p\NodeType    = NodeType
      *p\value       = value
      
      ProcedureReturn *p
   EndProcedure

   Procedure            expr( var )
      Protected.stTree  *p, *node, *r, *e
      Protected         op, q
      
      ;Debug "expr>" + RSet(Str(TOKEN()\row),4," ") + RSet(Str(TOKEN()\col),4," ") + "   " + TOKEN()\name + " --> " + gszATR( llTokenList()\TokenType )\s
      
      Select TOKEN()\TokenExtra
         Case #ljLeftParent
            *p = paren_expr()
            
         Case #ljSUBTRACT, #ljADD
            op = TOKEN()\TokenExtra
            NextToken()
            *node = expr( gPreTable( #ljNEGATE )\Precedence )
            
            If op = #ljSUBTRACT
               *p = MakeNode( #ljNEGATE, *node, 0 )
            Else
               *p = *Node
            EndIf
            
         Case  #ljNOT
            NextToken()
            *p = MakeNode( #ljNOT, expr( gPreTable( #ljNOT )\Precedence ), 0 )
            
         Case #ljIDENT
            *p = Makeleaf( #ljIDENT, TOKEN()\value )
            NextToken()

         Case #ljINT
            *p = Makeleaf( #ljINT, TOKEN()\value )
            NextToken()
            
         Case #ljFLOAT
            *p = Makeleaf( #ljFLOAT, TOKEN()\value )
            NextToken()
         
         Case #ljSTRING
            *p = Makeleaf( #ljSTRING, TOKEN()\value )
            NextToken()
            
         Default
            SetError( "Expecting a primary, found " + TOKEN()\name, 12 )
      
      EndSelect
      
      While gPreTable( TOKEN()\TokenExtra )\bBinary And gPreTable( TOKEN()\TokenExtra )\Precedence >= var
         op = TOKEN()\TokenExtra
         NextToken()
      
         q = gPreTable( op )\Precedence
         
         If Not gPreTable( op )\bRightAssociation
            q + 1
         EndIf
      
         *node = expr( q )
         *p = MakeNode( gPreTable( op )\NodeType, *p, *node )
      Wend
      
      ProcedureReturn *p

   EndProcedure
   
   Procedure            paren_expr()
      Protected         *p.stTree
      
      Expect( "paren_expr", #ljLeftParent )
      *p = expr( 0 )
      Expect( "paren_expr", #ljRightParent )
      ProcedureReturn *p
   EndProcedure
   
   Procedure            expand_params( op = #ljpop, nModule = -1 )
      ; OLD CODE - had wrong parameter order and unclear logic:
      ; Protected.stTree  *p, *e, *v
      ; Protected         nParams
      ; Expect( "expand_params", #ljLeftParent )
      ; Repeat
      ;    *e = expr( 0 )
      ;    If TOKEN()\TokenExtra = #ljComma
      ;       NextToken()
      ;       nParams + 1
      ;       If op = #ljPOP
      ;          If *e\value > ""
      ;             *v = Makeleaf( #ljIDENT, *e\value )
      ;          Else
      ;             *v = MakeNode( #ljASSIGN, *e, 0 )
      ;          EndIf
      ;          *p = MakeNode( #ljASSIGN, *v, *p )
      ;       Else
      ;          *p = MakeNode( #ljASSIGN, *p, *e )
      ;       EndIf
      ;    ElseIf *e
      ;       If op = #ljPOP
      ;          If *e\value > ""
      ;             *v = Makeleaf( #ljIDENT, *e\value )
      ;          Else
      ;             *v = MakeNode( #ljASSIGN, *e, 0 )
      ;          EndIf
      ;          *p = MakeNode( #ljASSIGN, *v, *p )
      ;       Else
      ;          *p = MakeNode( #ljSEQ, *p, *e )
      ;       EndIf
      ;       nParams + 1
      ;       Break
      ;    EndIf
      ; ForEver
      ; If nModule > -1
      ;    ForEach mapModules()
      ;       If mapModules()\function = nModule
      ;          mapModules()\nParams = nParams
      ;          Break
      ;       EndIf
      ;    Next
      ; EndIf
      ; Expect( "expand_params", #ljRightParent )
      ; ProcedureReturn *p

      ; NEW CODE - correct parameter order and clearer logic:
      Protected.stTree  *p, *e, *v, *first, *last, *param
      Protected         nParams
      NewList           llParams.i()  ; FIXED: Store pointers (integers), not structures

      Expect( "expand_params", #ljLeftParent )

      ; Build parameter list in correct order
      If TOKEN()\TokenExtra <> #ljRightParent
         Repeat
            *e = expr( 0 )

            If *e
               AddElement( llParams() )
               llParams() = *e  ; Store pointer value as integer
               nParams + 1
            EndIf

            If TOKEN()\TokenExtra = #ljComma
               NextToken()
            Else
               Break
            EndIf
         ForEver
      EndIf

      Expect( "expand_params", #ljRightParent )

      ; Generate code based on operation mode
      If op = #ljPOP
         ; For function declarations: POP params from stack (reverse order)
         ; Stack has: param1, param2, param3 (param3 on top)
         ; We need: POP param3, POP param2, POP param1

         ; FIXED: Can't use ForEach with PreviousElement - causes infinite loop!
         ; Use manual backwards iteration instead

         If LastElement( llParams() )
            Repeat
               *param.stTree = llParams()  ; Get pointer from list

               ; FIXED: POP needs to be a LEAF with the variable name as value
               ; NOT a binary node with IDENT as child!
               If *param\value > ""
                  ; Parameter is an identifier - create POP with that name
                  *e = Makeleaf( #ljPOP, *param\value )
               Else
                  ; This shouldn't happen for function parameters
                  *e = Makeleaf( #ljPOP, "?unknown?" )
               EndIf

               If *p
                  *p = MakeNode( #ljSEQ, *p, *e )
               Else
                  *p = *e
               EndIf

            Until Not PreviousElement( llParams() )
         EndIf
      Else
         ; For function calls: PUSH params onto stack (forward order)
         ForEach llParams()
            *param.stTree = llParams()  ; Get pointer from list

            If *p
               *p = MakeNode( #ljSEQ, *p, *param )
            Else
               *p = *param
            EndIf
         Next
      EndIf

      ; Store parameter count in module info
      If nModule > -1
         ForEach mapModules()
            If mapModules()\function = nModule
               mapModules()\nParams = nParams
               Break
            EndIf
         Next
      EndIf

      ProcedureReturn *p
   EndProcedure
   
   Procedure            stmt()
      Protected.i       i, n
      Protected.stTree  *p, *v, *e, *r, *s, *s2
      Protected.s       text, param

      gStack + 1
      
      If gStack > 120
         NextToken()
         SetError( "Stack overflow", 15 )
      EndIf
      
      ;Debug "stmt>" + RSet(Str(TOKEN()\row),4," ") + RSet(Str(TOKEN()\col),4," ") + "   " + TOKEN()\name + " --> " + gszATR( llTokenList()\TokenType )\s
      
      Select TOKEN()\TokenType
         Case #ljIF
            NextToken()
            *e    = paren_expr()
            *s    = stmt()
            *s2   = 0 
            
            If TOKEN()\TokenType = #ljElse
               NextToken()
               *s2 = stmt()
            EndIf
            
            *p = MakeNode( #ljIF, *e, MakeNode( #ljIF, *s, *s2 ) )
         
         Case #ljPRTC
            NextToken()
            *e    = paren_expr()
            *p    = MakeNode( #ljPRTC, *e, 0 )
            expect( "putc", #ljSemi )
         
         Case #ljPrint
            NextToken()
            expect( "print", #ljLeftParent )
            
            Repeat
               If TOKEN()\TokenExtra = #ljSTRING
                  *r = Makeleaf( #ljSTRING, TOKEN()\value )
                  *e = MakeNode( #ljPRTS, *r, 0 )
                  NextToken()
               ElseIf TOKEN()\TokenExtra = #ljFLOAT
                  *r = expr( 0 )
                  *e = MakeNode( #ljPRTF, *r, 0 )
               Else
                  *r = expr( 0 )
                  *e = MakeNode( #ljPRTI, *r, 0 )
               EndIf
               
               *p = MakeNode( #ljSEQ, *p, *e )
               
               If TOKEN()\TokenType <> #ljComma
                  Break
               EndIf
               
               expect( "print", #ljComma )
               
            Until TOKEN()\TokenType = #ljEOF            
            
            ; Add LineFeed as end of string
            *r = Makeleaf( #ljINT, "10" )
            *e = MakeNode( #ljPRTC, *r, 0 )
            *p = MakeNode( #ljSEQ, *p, *e )
            
            Expect( "Print", #ljRightParent )
            Expect( "Print", #ljSemi )
            
         Case #ljSemi
            NextToken()
            
         Case #ljIDENT
            *v = Makeleaf( #ljIDENT, TOKEN()\value )
            NextToken()
            Expect( "Assign", #ljASSIGN )

            ; OLD CODE - didn't handle function calls properly:
            ; If TOKEN()\TokenExtra = #ljCall
            ;    *e = MakeNode( #ljSEQ, stmt(), 0 )
            ;    *p = MakeNode( #ljASSIGN, *v, *e )
            ; Else
            ;    *e = expr( 0 )
            ;    *p = MakeNode( #ljASSIGN, *v, *e )
            ; EndIf

            ; NEW CODE - function calls leave return value on stack:
            If TOKEN()\TokenExtra = #ljCall
               ; Generate call code (pushes params, calls function)
               ; The function will leave return value on stack
               *r = Makeleaf( #ljCALL, TOKEN()\value )
               NextToken()
               *e = expand_params( #ljPush )
               ; Sequence: push params, call, store result
               *s = MakeNode( #ljSEQ, *e, *r )
               *p = MakeNode( #ljASSIGN, *v, *s )
            Else
               *e = expr( 0 )
               *p = MakeNode( #ljASSIGN, *v, *e )
            EndIf

            Expect( "Assign", #ljSemi )
         Case #ljWHILE
            NextToken()
            *e = paren_expr()
            *s = stmt()
            *p = MakeNode( #ljWHILE, *e, *s )
            
         Case #ljLeftBrace
            Expect( "Left Bracket", #ljLeftBrace )
            
            While TOKEN()\TokenExtra <> #ljRightBrace And TOKEN()\TokenExtra <> #ljEOF
               *p = MakeNode( #ljSEQ, *p, stmt() )
            Wend
            
            Expect( "Left Bracket", #ljRightBrace )
            
         Case #ljEOF
            gExit = 1
            
         Case #ljHalt
            NextToken()
            *p = MakeNode( #ljHalt, *p, 0 )
            
         Case #ljFunction
            ; OLD CODE - didn't track function body properly:
            ; *v = Makeleaf( #ljFunction, TOKEN()\value )
            ; n = Val ( TOKEN()\value )
            ; NextToken() : NextToken()
            ; *e = expand_params( #ljPOP, n )
            ; *p = MakeNode( #ljSEQ, *v, *e )

            ; NEW CODE - function marker + parameter pops:
            *v = Makeleaf( #ljFunction, TOKEN()\value )
            n = Val( TOKEN()\value )

            NextToken() : NextToken()

            ; Get parameter names and generate POP operations for them
            *e = expand_params( #ljPOP, n )

            ; Sequence: function marker, then parameter pops
            *p = MakeNode( #ljSEQ, *v, *e )
         
         Case #ljCALL
            *v = Makeleaf( #ljCALL, TOKEN()\value )
            NextToken()
            *e = expand_params( #ljPush )
            *p = MakeNode( #ljSEQ, *e, *v )
            
         Case #ljReturn
            NextToken()
            
            ; NEW CODE - generate expr, then return:
            If TOKEN()\TokenType = #ljSemi
               ; return with no value - push 0
               *e = Makeleaf( #ljINT, "0" )
               *v = MakeNode( #ljSEQ, *e, Makeleaf( #ljReturn, "0" ) )
               NextToken()
            Else
               ; return with value - evaluate expr, then return
               *e = expr(0)
               *v = MakeNode( #ljSEQ, *e, Makeleaf( #ljReturn, "0" ) )
               Expect( "Return", #ljSemi )
            EndIf

            *p = MakeNode( #ljSEQ, *p, *v )

         Default
            SetError( "Expecting beginning of a statament, found " + TOKEN()\name, 14 )
         
      EndSelect
      
      ProcedureReturn *p
   EndProcedure
   
   Procedure            DisplayNode( *p.stTree )
      If *p
         If *p\NodeType = #ljIDENT Or *p\NodeType = #ljINT Or *p\NodeType = #ljSTRING
            Debug LSet( gszATR( *p\NodeType )\s, 30 ) + *p\value
         Else
            Debug LSet( gszATR( *p\NodeType )\s, 30 )
            DisplayNode( *p\left )
            DisplayNode( *p\right )
         EndIf
      Else
         Debug ";"
      EndIf
   EndProcedure
   ;- =====================================
   ;- Code Generator
   ;- =====================================
   Procedure            hole()
      gHoles + 1

      AddElement( llHoles() )
      llHoles()\location   = llObjects()
      llHoles()\mode       = 0
      llHoles()\id         = gHoles
      
      ProcedureReturn gHoles
   EndProcedure
   
   Procedure            fix( id, dst = -1 )
      
      AddElement( llHoles() )
      
      If dst = -1
         llHoles()\mode = 1
         llHoles()\id = id
         llHoles()\location = llObjects()
      Else                                   ; Used by blind JMP
         llHoles()\mode = 3
         llHoles()\location = LastElement( llObjects() )
         llHoles()\src = dst
      EndIf

   EndProcedure
    
   Procedure            EmitInt( op.i, nVar.i = -1 )
      Static            cmd.i    = #LJUnknown
      Static            llLastOp
      
      If cmd = #ljpush And op = #ljStore
         llObjects()\code     = #ljMOV
         llObjects()\j        = llObjects()\i
         gVar( nVar )\flags   = ( gVar( llObjects()\i )\flags & #C2FLAG_TYPE ) | #C2FLAG_IDENT
      ElseIf cmd = #ljfetch And op = #ljstore
         llObjects()\code = #ljMOV
         llObjects()\j = llObjects()\i
      Else
         llLastOp = AddElement( llObjects() )
         llObjects()\code = op
      EndIf
      
      If nVar > -1
         llObjects()\i     = nVar
      EndIf
      
      cmd = llObjects()\code
   EndProcedure
   
   Procedure            FetchVarOffset(text.s)
      Protected         i, j
      Protected         temp.s
   
      j = -1
   
      For i = 0 To gnLastVariable - 1
         If gVar(i)\name = text
            ProcedureReturn i
         EndIf
      Next
      
      i = -1      
      
      ForEach TOKEN()
         If TOKEN()\value = text
            i = ListIndex( TOKEN() )
            Break
         EndIf
      Next
      
      gVar(gnLastVariable)\name  = text
      
      If TOKEN()\TokenType = #ljINT
         gVar(gnLastVariable)\i = Val(text)
         gVar(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_INT
      ElseIf TOKEN()\TokenType = #ljSTRING
         gVar(gnLastVariable)\ss = text
         gVar(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_STR
      ElseIf TOKEN()\TokenType = #ljFLOAT
         gVar(gnLastVariable)\f = ValF(text)
         gVar(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_FLOAT
      ElseIf TOKEN()\TokenType = #ljIDENT
         gVar(gnLastVariable)\i        = gnLastVariable
         gVar(gnLastVariable)\flags    = #C2FLAG_IDENT
      Else
         Debug ": " + text + " Not found"
      EndIf
      
      ;_VarExpand(gnLastVariable)
      ;Debug "FetchVar: " + text + " --> " + temp 
      
      gnLastVariable + 1
      ProcedureReturn gnLastVariable - 1
   EndProcedure
  
   Procedure            CodeGenerator( *x.stTree, *link.stTree = 0 )
      Protected         p1, p2, n
      Protected         temp.s
      Static            gFunction = 0
   
      ; If no node, return immediately
      If Not *x
         ProcedureReturn
      EndIf
   
      ;Debug gszATR( *x\NodeType )\s + " --> " + *x\value
      
      Select *x\NodeType
         Case #ljEOF : ProcedureReturn
         Case #ljPOP
            n = FetchVarOffset(*x\value)
            EmitInt( #ljPOP, n )
            gVar( n )\flags = gVar( n )\flags | #C2FLAG_IDENT
         
         Case #ljIDENT
            n = FetchVarOffset(*x\value)
            EmitInt( #ljFetch, n ) 
            gVar( n )\flags = gVar( n )\flags | #C2FLAG_IDENT
            ;_varExpand( n )
            ;Debug "CG IDENT: " + *x\value + " " + temp + " --> " + gszATR( llObjects()\code )\s
            
         Case #ljINT, #ljFLOAT, #ljSTRING
            n = FetchVarOffset(*x\value)
            EmitInt( #ljPush, n )
            ;_varExpand( n )
            ;Debug "CG TYPE: " + *x\value + " " + temp + " --> " + gszATR( llObjects()\code )\s
         Case #ljASSIGN
            n = FetchVarOffset( *x\left\value )
            CodeGenerator( *x\right )
            EmitInt( #ljSTORE, n )
            
            If llObjects()\code <> #ljMOV
               gVar( llObjects()\i )\flags = gVar( n )\flags & #C2FLAG_TYPE
            EndIf

         Case #ljReturn
            EmitInt( #ljReturn )

         Case #ljIF
            CodeGenerator( *x\left )
            EmitInt( #ljJZ)
            p1 = hole()
            CodeGenerator( *x\right\left )
            
            If *x\right\right 
               EmitInt( #ljJMP)
               p2 = hole()
            EndIf
            
            fix( p1 )
            
            If *x\right\right 
               CodeGenerator( *x\right\right )
               fix( p2 )
            EndIf
            
         Case #ljWHILE
            p1 = llObjects()
            CodeGenerator( *x\left )
            EmitInt( #ljJZ)
            p2 = Hole()
            CodeGenerator( *x\right )
            EmitInt( #ljJMP)
            fix( gHoles, p1 )
            fix( p2 )
            
         Case #ljSEQ
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            
         Case #ljFunction
            ForEach mapModules()
               If mapModules()\function = Val( *x\value )
                  mapModules()\Index = ListIndex( llObjects() ) + 1
                  ;Debug "FUNC: " + *x\value + "   " + Str( mapModules()\Index  )
                  Break
               EndIf
            Next
            
         Case #ljPRTC, #ljPRTI, #ljPRTS, #ljPRTF, #ljprint
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType )
         
         Case #ljLESS, #ljGREATER, #ljLESSEQUAL, #ljGreaterEqual, #ljEQUAL, #ljNotEqual, 
              #ljAdd, #ljSUBTRACT, #ljDIVIDE, #ljMULTIPLY
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            EmitInt( *x\NodeType )
         
         Case #ljOr, #ljAND, #ljMOD, #ljXOR
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            EmitInt( *x\NodeType)
            
         Case #ljNOT
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType)
         
         Case #ljNEGATE
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType )
            
         Case #ljCall
            ;Debug "CALL: " + *x\value
            EmitInt( *x\NodeType, Val( *x\value ) )
            
         Case #ljHalt
            EmitInt( *x\NodeType, 0 )
            
         Default
            SetError("Error in CodeGenerator at node " + Str(*x\NodeType) + " " + *x\value + " ---> " + gszATR(*x\NodeType)\s, 21)
   
      EndSelect
   EndProcedure
    
   Procedure            FixJMP()
      Protected         i, pos, pair
   
      ForEach llHoles()
         If llHoles()\mode = 1
            PushListPosition( llHoles() )
               llHoles()\mode = 2
               pair  = llHoles()\id
               ChangeCurrentElement( llObjects(), llHoles()\location )
               pos   = ListIndex( llObjects() )
               i     = 0
               
               ForEach llHoles()
                  If llHoles()\mode = 0 And llHoles()\id = pair
                     llHoles()\mode = 2
                        ChangeCurrentElement( llObjects(), llHoles()\location )
                        llObjects()\i = (pos - ListIndex( llObjects() ) ) + 1
                     Break
                  EndIf
               Next
            PopListPosition( llHoles() )
         ElseIf llHoles()\mode = 3
            llHoles()\mode = 2
            ChangeCurrentElement( llObjects(), llHoles()\src )
            pos = ListIndex( llObjects() )
            ChangeCurrentElement( llObjects(), llHoles()\location )
            ; Perhaps, keep an eye on this
            llObjects()\i = (pos - ListIndex( llObjects() ) ) + 1
         EndIf
      Next
      
      ForEach llObjects()
         If llObjects()\code = #ljCall
            ForEach mapModules()
               If mapModules()\function = llObjects()\i
                  llObjects()\i = mapModules()\Index
                  ;Debug "Adjusted to " + Str( mapModules()\Index )
                  Break
               EndIf
            Next   
         EndIf
      Next
   EndProcedure
   
   Procedure            PostProcessor()
      Protected         PushFlags.w    = #C2FLAG_INT
      Protected         jmpFlags.w     = #C2FLAG_INT
      Protected         op, n
      Protected         flag
      Protected         jmppos = -1
      Protected.s       Line, temp
      Protected Dim     arFlags.i( 32 )
      
      ForEach llObjects()
         op = llObjects()\code

         If op = #ljMOV 
            n = llObjects()\j
         Else
            n = llObjects()\i
         EndIf
         
         If op = #ljPush Or op = #ljMOV Or op = #ljFetch Or op = #ljPOP
            If ListIndex( llObjects() ) = jmppos
               PushFlags = jmpFlags
               ;Debug gszATR( op )\s + " ---> we are here"
            Else
               PushFlags   = gVar( n )\flags
            EndIf
         ElseIf op = #ljStore
            If gVar( n )\flags = #C2FLAG_IDENT
               gVar( n )\flags = ( PushFlags & #C2FLAG_TYPE ) | #C2FLAG_IDENT
            EndIf
         ElseIf op = #ljJZ Or op = #ljJMP
            jmppos   = ListIndex( llObjects() ) + n
            jmpFlags = PushFlags
            ;Debug gszATR( op )\s + " we want to jump to " + Str( jmppos ) + " " + Str(n)
         Else
            If PushFlags & #C2FLAG_FLOAT
               If gszATR( op )\flttoken > 0
                  llObjects()\code = gszATR( op )\flttoken
               EndIf
            ElseIf PushFlags & #C2FLAG_STR
               If gszATR( op )\strtoken > 0
                  llObjects()\code = gszATR( op )\strtoken
               EndIf
            EndIf
         EndIf
      Next
      
      ForEach llObjects()
         If llObjects()\code = #ljCall
            ; Found a function call - look backwards to find the parameters being pushed
            Protected callIndex = ListIndex( llObjects() )
            Protected funcIndex = llObjects()\i  ; This is the function entry point
            Protected paramCount = 0
            Protected bAlreadyLocked = #False

            ; Search for the function to check if types already locked
            ForEach mapModules()
               If mapModules()\Index = funcIndex
                  paramCount = mapModules()\nParams
                  bAlreadyLocked = mapModules()\bTypesLocked
                  Break
               EndIf
            Next

            ; Only process if types NOT already locked (first call)
            If paramCount > 0 And Not bAlreadyLocked
               ; Go backwards from call to find the parameters
               Protected currentIndex = callIndex - 1
               Protected foundParams = 0
               ReDim arFlags( paramCount )

               ; Scan backwards to collect parameter types
               While currentIndex >= 0 And foundParams < paramCount
                  SelectElement( llObjects(), currentIndex )

                  If llObjects()\code = #ljPush Or llObjects()\code = #ljFetch
                     arFlags( foundParams ) = gVar( llObjects()\i )\flags
                     foundParams + 1
                  EndIf

                  currentIndex - 1
               Wend

               ; Now apply these types to the POP operations at function start
               ; POPs are in reverse order (last param first), so we need to reverse the array
               If foundParams = paramCount
                  SelectElement( llObjects(), funcIndex )
                  n = 0

                  ; FIXED: Don't skip if we're already at a POP!
                  ; funcIndex should point to the first POP, not a function marker
                  If llObjects()\code <> #ljPOP
                     NextElement( llObjects() )
                  EndIf

                  ; Apply types to each POP operation (FIRST CALL LOCKS THE TYPES)
                  While n < paramCount
                     If llObjects()\code = #ljPOP
                        ; Apply type from corresponding push
                        ; arFlags is already in correct order (we scanned backwards, POPs execute backwards)
                        ; IMPORTANT: Keep type bits but set as IDENT (variable), not CONST
                        gVar( llObjects()\i )\flags = (arFlags(n) & #C2FLAG_TYPE) | #C2FLAG_IDENT
                        n + 1
                     EndIf

                     If Not NextElement( llObjects() )
                        Break
                     EndIf
                  Wend

                  ; LOCK the types - this function's parameter types are now fixed
                  ForEach mapModules()
                     If mapModules()\Index = funcIndex
                        mapModules()\bTypesLocked = #True
                        Break
                     EndIf
                  Next

                  ; NOW re-run type propagation on the function body with correct parameter types
                  ; This is critical - the first pass ran before parameter types were set,
                  ; so operations like PRTI weren't converted to PRTF/PRTS

                  ; Find the end of this function (next function start or end of list)
                  Protected funcEndIndex = -1
                  Protected tempIndex = funcIndex + paramCount

                  While tempIndex < ListSize(llObjects())
                     SelectElement(llObjects(), tempIndex)
                     ; Check if this is the start of another function (first POP after params)
                     ; Actually, just look for the next function marker in mapModules
                     Protected bFoundNextFunc = #False
                     ForEach mapModules()
                        If mapModules()\Index > funcIndex And mapModules()\Index <= tempIndex
                           funcEndIndex = mapModules()\Index - 1
                           bFoundNextFunc = #True
                           Break
                        EndIf
                     Next
                     If bFoundNextFunc
                        Break
                     EndIf
                     tempIndex + 1
                  Wend

                  If funcEndIndex = -1
                     funcEndIndex = ListSize(llObjects()) - 1
                  EndIf

                  ; Apply type propagation to function body
                  Protected bodyPushFlags.w = #C2FLAG_INT
                  Protected bodyOp, bodyN

                  For tempIndex = funcIndex To funcEndIndex
                     SelectElement(llObjects(), tempIndex)
                     bodyOp = llObjects()\code

                     If bodyOp = #ljMOV
                        bodyN = llObjects()\j
                     Else
                        bodyN = llObjects()\i
                     EndIf

                     If bodyOp = #ljPush Or bodyOp = #ljMOV Or bodyOp = #ljFetch Or bodyOp = #ljPOP
                        bodyPushFlags = gVar(bodyN)\flags
                     ElseIf bodyOp = #ljStore
                        If gVar(bodyN)\flags = #C2FLAG_IDENT
                           gVar(bodyN)\flags = (bodyPushFlags & #C2FLAG_TYPE) | #C2FLAG_IDENT
                        EndIf
                     Else
                        ; This is where we convert operations based on type!
                        If bodyPushFlags & #C2FLAG_FLOAT
                           If gszATR(bodyOp)\flttoken > 0
                              llObjects()\code = gszATR(bodyOp)\flttoken
                           EndIf
                        ElseIf bodyPushFlags & #C2FLAG_STR
                           If gszATR(bodyOp)\strtoken > 0
                              llObjects()\code = gszATR(bodyOp)\strtoken
                           EndIf
                        EndIf
                     EndIf
                  Next

                  ; Determine return type by looking for RET operation
                  Protected returnTypeFlags.w = #C2FLAG_INT  ; Default
                  For tempIndex = funcIndex To funcEndIndex
                     SelectElement(llObjects(), tempIndex)
                     If llObjects()\code = #ljReturn
                        ; Found RET - look at previous operation (should be FETCH or PUSH)
                        If tempIndex > 0
                           SelectElement(llObjects(), tempIndex - 1)
                           If llObjects()\code = #ljFetch Or llObjects()\code = #ljPush
                              If llObjects()\code = #ljMOV
                                 returnTypeFlags = gVar(llObjects()\j)\flags & #C2FLAG_TYPE
                              Else
                                 returnTypeFlags = gVar(llObjects()\i)\flags & #C2FLAG_TYPE
                              EndIf
                           EndIf
                        EndIf
                        Break
                     EndIf
                  Next

                  ; Store return type in function info
                  ForEach mapModules()
                     If mapModules()\Index = funcIndex
                        mapModules()\returnType = returnTypeFlags
                        Break
                     EndIf
                  Next

               EndIf
            EndIf

            ; Restore position
            SelectElement( llObjects(), callIndex )
         EndIf
      Next

      ; THIRD PASS - Apply function return types to STORE operations after CALL
      ; Now that we've determined all function return types, propagate them to stores

      Protected callReturnFlags.w = #C2FLAG_INT
      ForEach llObjects()
         op = llObjects()\code

         If op = #ljCall
            ; Look up the function's return type
            callReturnFlags = #C2FLAG_INT  ; Default
            ForEach mapModules()
               If mapModules()\Index = llObjects()\i
                  If mapModules()\returnType > 0
                     callReturnFlags = mapModules()\returnType
                  EndIf
                  Break
               EndIf
            Next
         ElseIf op = #ljStore
            ; Apply the return type from the previous CALL
            If callReturnFlags > 0
               Protected storeVarIdx = llObjects()\i
               If gVar(storeVarIdx)\flags & #C2FLAG_IDENT
                  gVar(storeVarIdx)\flags = (callReturnFlags & #C2FLAG_TYPE) | #C2FLAG_IDENT
               EndIf
            EndIf
            ; Reset for next sequence
            callReturnFlags = #C2FLAG_INT
         EndIf
      Next

      ; FOURTH PASS - Re-run operation conversion with updated variable types
      ; The third pass updated STORE variable types, so now we need to convert operations
      ; that use those variables (like PRTS -> PRTF for printing float return values)

      PushFlags = #C2FLAG_INT
      ForEach llObjects()
         op = llObjects()\code

         If op = #ljMOV
            n = llObjects()\j
         Else
            n = llObjects()\i
         EndIf

         If op = #ljPush Or op = #ljMOV Or op = #ljFetch Or op = #ljPOP
            PushFlags = gVar(n)\flags
         ElseIf op = #ljJZ Or op = #ljJMP
            jmppos = ListIndex(llObjects()) + n
            jmpFlags = PushFlags
         Else
            ; Convert operations based on current type flags
            If PushFlags & #C2FLAG_FLOAT
               If gszATR(op)\flttoken > 0
                  llObjects()\code = gszATR(op)\flttoken
               EndIf
            ElseIf PushFlags & #C2FLAG_STR
               If gszATR(op)\strtoken > 0
                  llObjects()\code = gszATR(op)\strtoken
               EndIf
            EndIf
         EndIf
      Next



   EndProcedure
   
   Procedure            ListCode( gadget = 0 )
      Protected         i
      Protected         flag
      Protected.s       temp, line, FullCode

      Debug ";--"
      Debug ";-- Variables & Constants --"
      Debug ";--"

      For i = 0 To gnLastVariable - 1
         If gVar(i)\flags & #C2FLAG_IDENT
            temp = "Variable"
         ElseIf gVar(i)\flags & #C2FLAG_INT
            temp = "Integer"
         ElseIf gVar(i)\flags & #C2FLAG_FLOAT
            temp = "Float"
         ElseIf gVar(i)\flags & #C2FLAG_STR
            temp = "String"
         EndIf
         
         If gVar(i)\flags & #C2FLAG_CONST
            temp + " constant"
         EndIf

         Debug RSet(Str(i),6, " ") + "   " + LSet(gVar(i)\name,20," ") + "  (" + temp + ")"
      Next

      Debug ";--"
      Debug ";--     Code Section      --"
      Debug ";--"
  
      ForEach llObjects()
         ASMLine( llObjects(),0 )
         Debug Line
         FullCode + Line +  #CRLF$
      Next

      Debug ";--"
      Debug ";--     End Program       --"
      Debug ";--"
      SetClipboardText( FullCode )
   EndProcedure
   ;- =====================================
   ;- Compiler
   ;- =====================================
   Procedure         Compile()
   Protected         i
      Protected      err
      Protected      *p.stTree
      Protected      total
      Protected.s    temp
      
      Init()
      Preprocessor()
   
      If Scanner()
         ProcedureReturn gLastError
      Else   
         ;- par_DebugParser()
         ;par_DebugParser()
         ReorderTokens()

         FirstElement( TOKEN() )
         total = ListSize( TOKEN() ) - 1
      
         Repeat
            gStack = 0
            *p = MakeNode( #ljSEQ, *p, stmt() )
            
            If gLastError
               Debug gszlastError
               gExit = -1
               Break 
            EndIf
            
         Until ListIndex( TOKEN() ) >= total Or gExit
         
         If gExit >= 0
            ;- DisplayNode( *p )
            ;DisplayNode( *p )
            
            CodeGenerator( *p )
            
            FixJMP()
            PostProcessor()
            vm_ListToArray( llObjects, arCode )
         Else
            Debug "gExit=" + Str(gExit)
         EndIf
      EndIf
      
      ProcedureReturn 0
   EndProcedure
EndModule

CompilerIf #PB_Compiler_IsMainFile
   ; -- Module demo
   
   Define         err
   Define.s       filename
 
   
   ;filename = ".\Examples\02 Simple while.lj"
   ;filename = ".\Examples\04 If Else.lj"
   ;filename = ".\Examples\06 Mandelbrot.lj"
   filename = ".\Examples\07 Floats and macros.lj"
   filename = ".\Examples\09 Functions2.lj"
   ;filename = OpenFileRequester( "Please choose source", ".\Examples\", "LJ Files|*.lj", 0 )
 
   If filename > ""
      If C2Lang::LoadLJ( filename )
         Debug "Error: " + C2Lang::Error( @err )
      Else
         C2Lang::Compile()
         C2Lang::ListCode()
         C2VM::RunVM()
      EndIf
   EndIf
   
CompilerEndIf


; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 1691
; FirstLine = 1683
; Folding = ---------
; Markers = 1391,1554,1598
; Optimizer
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; DisablePurifier = 1,1,4,1
; EnableCompileCount = 515
; EnableBuildCount = 0
; EnableExeConstant