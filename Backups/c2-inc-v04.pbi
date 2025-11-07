
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
; Common constants and structures

; ======================================================================================================
;- Constants
; ======================================================================================================

#C2PROFILER       = 0
#INV$             = ~"\""

#C2MAXTOKENS      = 500   
#C2MAXCONSTANTS   = 8192

#C2FLAG_TYPE      = 30
#C2FLAG_CONST     = 1
#C2FLAG_IDENT     = 2
#C2FLAG_INT       = 4
#C2FLAG_FLOAT     = 8
#C2FLAG_STR       = 16


Enumeration
   #ljUNUSED
   #ljIDENT
   #ljINT
   #ljFLOAT
   #ljSTRING
   #ljIF
   #ljElse   
   #ljWHILE
   #ljJZ
   #ljJMP
   #ljNEGATE
   #ljFLOATNEG
   #ljNOT
   #ljASSIGN  
   
   #ljADD
   #ljSUBTRACT
   #ljMULTIPLY
   #ljDIVIDE
   #ljFLOATADD
   #ljFLOATSUB
   #ljFLOATMUL
   #ljFLOATDIV   
   #ljSTRADD
   
   #ljOr
   #ljAND
   #ljXOR
   #ljMOD
  
   #ljEQUAL 
   #ljNotEqual
   #ljLESSEQUAL
   #ljGreaterEqual
   #ljGREATER
   #ljLESS
   #ljFLOATEQ
   #ljFLOATNE
   #ljFLOATLE
   #ljFLOATGE
   #ljFLOATGR
   #ljFLOATLESS
   
   #ljMOV
   #ljFetch
   #ljPush
   #ljStore
   #ljHALT
   
   #ljPrint
   #ljPRTC
   #ljPRTI
   #ljPRTF
   #ljPRTS
         
   #ljLeftBrace
   #ljRightBrace
   #ljLeftParent
   #ljRightParent
   #ljSemi
   #ljComma   
   #ljpragma
   
   #ljUNKNOWN
   #ljNOOP
   #ljOP
   #ljSEQ
   #ljKeyword
   #ljEOF
EndEnumeration

#ljSTR = #ljSTRING


;- Structures
Structure stType
   code.w
   i.l
   j.l
   flags.w     ; for versions 5 and older
EndStructure

Structure stVT  ; Variable Type
   name.s
   flags.w
   ss.s 
   *p
   i.i
   f.d
EndStructure

Structure stCall
   function.i
   id.l
   *src
   *location
EndStructure

Structure stATR
   s.s
   strtoken.w
   flttoken.w
EndStructure

;- Globals

Global Dim           gszATR.stATR(#C2MAXTOKENS)
Global Dim           gVar.stVT(#C2MAXCONSTANTS)
Global NewList       llObjects.stType()
Global NewList       llHoles.stCall()
Global NewMap        mapPragmas.s()
;Global NewMap        mapVariables(2053)

Global               gnLastVariable.i
Global               gnTotalTokens.i

;- Macros
Macro          _ASMLineHelper1(view, uvar)
   CompilerIf view
      If gVar( uvar )\flags & #C2FLAG_INT
         temp = " (" + Str( gVar( uvar )\i ) + ")" 
      ElseIf gVar( uvar )\flags & #C2FLAG_FLOAT
         temp = " (" + StrF( gVar( uvar )\f, 3 ) + ")" 
      ElseIf gVar( uvar )\flags & #C2FLAG_STR
         temp = " (" + gVar( uvar )\ss + ")" 
      EndIf
   CompilerEndIf
EndMacro

Macro          _ASMLineHelper2(uvar)
   If gVar( uvar )\flags & #C2FLAG_IDENT
      temp = gVar( uvar )\name
   ElseIf gVAR( uvar )\flags & #C2FLAG_STR
      temp = gVar( uvar )\ss
   ElseIf gVar( uvar )\flags & #C2FLAG_FLOAT
      temp = StrD( gVar( uvar )\f )
   Else
      temp = Str(gVar( uvar )\i )
   EndIf
EndMacro

Macro                   _VarExpand(vr)
   temp = ""
   If gVar( vr )\flags & #C2FLAG_INT :   temp + " INT "   : EndIf
   If gVar( vr )\flags & #C2FLAG_FLOAT : temp + " FLT "   : EndIf
   If gVar( vr )\flags & #C2FLAG_STR   : temp + " STR "   : EndIf
   If gVar( vr )\flags & #C2FLAG_CONST : temp + " CONST " : EndIf
   If gVar( vr )\flags & #C2FLAG_IDENT : temp + " VAR"    : EndIf
EndMacro

Macro          ASMLine(obj,show)
   CompilerIf show
      line = RSet( Str( pc ), 7 ) + "  "
   CompilerElse
      line = RSet( Str( ListIndex(obj) ), 7 ) + "  "
   CompilerEndIf
   
   line + LSet( gszATR( obj\code )\s, 10 ) + "  "
   temp = "" : flag = 0
   CompilerIf show
      line + "[" + RSet(Str(sp),5," ") + "] " 
   CompilerEndIf
   If obj\code = #ljJMP Or obj\code = #ljJZ
      CompilerIf show
         line + "  (" +Str(obj\i) + ") " + Str(pc+obj\i)
      CompilerElse
         line + "  (" +Str(obj\i) + ") " + Str(ListIndex(obj)+obj\i)
      CompilerEndIf
   ElseIf obj\code = #ljMOV
      _ASMLineHelper1( show, obj\j )
      line + "[" + gVar( obj\j )\name + temp + "] --> [" + gVar( obj\i )\name + "]"
      flag + 1
   ElseIf obj\code = #ljSTORE
      _ASMLineHelper1( show, sp - 1 )
      line + "[sp" + temp + "] --> [" + gVar( obj\i )\name + "]"
      flag + 1
   ElseIf obj\code = #ljPUSH Or obj\code = #ljFetch
      flag + 1
      _ASMLineHelper1( show, obj\i )
      If gVAR( obj\i )\flags & #C2FLAG_IDENT
         line + "[" + gVar( obj\i )\name + "] --> [sp]"
      ElseIf gVAR( obj\i )\flags & #C2FLAG_STR
         line + "[" + gVar( obj\i )\ss + "] --> [sp]"
      ElseIf gVar( obj\i )\flags & #C2FLAG_INT
         line + "[" + Str(gVar( obj\i )\i) +  "] --> [sp]"
      ElseIf gVar( obj\i )\flags & #C2FLAG_FLOAT
         line + "[" + StrD(gVar( obj\i )\f,3) +  "] --> [sp]"
      Else
         line + "[" + gVar( obj\i )\name + "] --> [sp]"
      EndIf
   ElseIf obj\code = #ljNEGATE Or obj\code = #ljNOT
      flag + 1
      CompilerIf show
         _ASMLineHelper2(sp - 1)
         line  + "  op (" + temp + ")"
      CompilerElse
         line  + "  op (sp - 1)"
      CompilerEndIf
      
   ElseIf obj\code <> #ljHALT
      CompilerIf show
         _ASMLineHelper2(sp - 2)
         line  + "   (" + temp + ") -- ("
         _ASMLineHelper2(sp - 1)
         line  + temp + ")"
      CompilerElse
         line  + "   (sp - 2) -- (sp - 1)"
      CompilerEndIf
   EndIf
   CompilerIf Not show
      If flag
         _VarExpand( obj\i )
         line + " FLAGS ["+ temp +"]"
      EndIf
   CompilerEndIf
EndMacro
;- End of file

DataSection
c2tokens:
   Data.s   "UNUSED"
   Data.i   0, 0
   Data.s   "VAR"
   Data.i   0, 0
   Data.s   "INT"
   Data.i   0, 0
   Data.s   "FLT"
   Data.i   0, 0
   Data.s   "STR"
   Data.i   0, 0
   
   Data.s   "IF"
   Data.i   0, 0
   Data.s   "ELSE"   
   Data.i   0, 0
   Data.s   "WHILE"
   Data.i   0, 0
   Data.s   "JZ"
   Data.i   0, 0
   Data.s   "JMP"
   Data.i   0, 0
   Data.s   "NEG"
   Data.i   #ljFLOATNEG, 0
   Data.s   "FLNEG"
   Data.i   #ljFLOATNEG, 0
   Data.s   "NOT"
   Data.i   0, 0
   Data.s   "ASSIGN"
   Data.i   0, 0
   
   Data.s   "ADD"
   Data.i   #ljFLOATADD, #ljSTRADD
   Data.s   "SUB"
   Data.i   #ljFLOATSUB, 0
   Data.s   "MUL"
   Data.i   #ljFLOATMUL, 0
   Data.s   "DIV"
   Data.i   #ljFLOATDIV, 0
   Data.s   "FLADD"
   Data.i   #ljFLOATADD, 0
   Data.s   "FLSUB"
   Data.i   #ljFLOATSUB, 0
   Data.s   "FLMUL"
   Data.i   #ljFLOATMUL, 0
   Data.s   "FLDIV"
   Data.i   #ljFLOATDIV, 0
   Data.s   "STRADD"
   Data.i   #ljFLOATADD, #ljSTRADD
   
   Data.s   "OR"
   Data.i   0, 0
   Data.s   "AND"
   Data.i   0, 0
   Data.s   "XOR"
   Data.i   0, 0
   Data.s   "MOD"
   Data.i   0, 0
   
   Data.s   "EQ"
   Data.i   #ljFLOATEQ, 0
   Data.s   "NE"
   Data.i   #ljFLOATNE, 0
   Data.s   "LTE"
   Data.i   #ljFLOATLE, 0
   Data.s   "GTE"
   Data.i   #ljFLOATGE, 0
   Data.s   "GT"
   Data.i   #ljFLOATGR, 0
   Data.s   "LT"
   Data.i   #ljFLOATLE, 0
   Data.s   "FLEQ"
   Data.i   0, 0
   Data.s   "FLNE"
   Data.i   0, 0
   Data.s   "FLLTE"
   Data.i   0, 0
   Data.s   "FLGTE"
   Data.i   0, 0
   Data.s   "FLGT"
   Data.i   0, 0
   Data.s   "FLLT"
   Data.i   0, 0
   
   Data.s   "MOV"
   Data.i   0, 0
   Data.s   "FETCH"
   Data.i   0, 0
   Data.s   "PUSH"
   Data.i   0, 0
   Data.s   "STORE"
   Data.i   0, 0
   Data.s   "HALT"
   Data.i   0, 0
   
   Data.s   "PRINT"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTC"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTI"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTF"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTS"
   Data.i   #ljPRTF, #ljPRTS
   
   Data.s   "LeftBrace"
   Data.i   0, 0
   Data.s   "RightBrace"
   Data.i   0, 0
   Data.s   "LeftParent"
   Data.i   0, 0
   Data.s   "RightParent"
   Data.i   0, 0
   Data.s   "SemiColon"
   Data.i   0, 0
   Data.s   "Comma"
   Data.i   0, 0
   Data.s   "pragma"
   Data.i   0, 0
   
   Data.s   "Unknown"
   Data.i   0, 0
   Data.s   "NOOP"
   Data.i   0, 0
   Data.s   "OP"
   Data.i   0, 0
   Data.s   "SEQ"
   Data.i   0, 0
   Data.s   "Keyword"
   Data.i   0, 0
   Data.s   "End of file"
   Data.i   0, 0
   Data.s   "-"
EndDataSection

; IDE Options = PureBasic 6.20 (Windows - x64)
; CursorPosition = 113
; FirstLine = 99
; Folding = -
; Optimizer
; EnableAsm
; EnableThread
; EnableXP
; SharedUCRT
; CPU = 1
; EnablePurifier
; EnableCompileCount = 21
; EnableBuildCount = 0
; EnableExeConstant