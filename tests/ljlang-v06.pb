; -- lexical parser for a simplified C Language 
; Tested in UTF8
; PBx64 v5.71
;
; Based on  https://rosettacode.org/wiki/Compiler/lexical_analyzer
; And
; https://rosettacode.org/wiki/Compiler/syntax_analyzer
; Distribute and use freely
; 
; Kingwolf71 Feb/2020
;

; ======================================================================================================
;- Includes
; -=====================================================================================================


EnableDebugger
XIncludeFile         "../../common/tz_memfile-v08.pbi"

DeclareModule LJLang
   
   #WithEOL = 1
   
   Enumeration
      #ljWinDelay
      #ljShowRunCode
      #ljListParse
      #ljListSyntax
      #ljFastConsole
      #ljListASM
   EndEnumeration
 
   Declare           DebugFlags( const, var = -1 )
   Declare.s         Error( *error.Integer )
   Declare           GenerateCode( gadget = -1 )
   Declare           LoadLJ( file.s )
   Declare           RunVm( gadget = -1 )
EndDeclareModule

Module LJLang
   EnableExplicit
   
   ; ======================================================================================================
   ;- Private Constants
   ; -=====================================================================================================

   UseModule MF

   #LJTOKENS            = 150
   #MAX_COMPDEPTH       = 100
   #MAX_STACK           = 8192
   #MAX_VARS            = 8192
   #MAX_CALLSTACK       = 4096
   #MAX_FUNCS           = 8192
   #My_Event_Logging    = 100

   ; Debug and console modifiers
   #WINDELAY            = 8
  
   ; Instruction Modifiers  
   #LJMOD_INT        = 1
   #LJMOD_FLOAT      = 2
   #LJMOD_STRING     = 4
   #LJMOD_ARRAY      = 8
   #LJMOD_STACK      = 16
   #LJMOD_CONST      = 32
   
   #LJMOD_NOSTR      = 3
   #LJMOD_NOINT      = 6
   #LJMOD_NOFLO      = 5
   #LJMOD_NOTYP      = 7
   
    Enumeration 
      #ljNOOP = 0
      #ljHOLE
      
      #ljIDENT
      #ljIF
      #ljWHILE
      #ljElse
      #ljFunction
      #ljReturn
      #ljDim
      #ljDeclare
      
      #ljASSIGN
      #ljINTEGER
      #ljFLOAT
      #ljSTRING
      #ljINTERNAL
      
      #ljSEQ
      #ljLeftCurly
      #ljRightCurly
      #ljLeftParent     ;30
      #ljRightParent
      #ljSemi
      #ljComma
      #ljEOF
      #ljBOF
      
      ; ASM Code
      
      #ljJZ
      #ljJMP
      #ljMULTIPLY
      #ljDIVIDE
      #ljMOD
      #ljADD
      #ljSUBTRACT
      #ljNEGATE
      #ljNOT            ;10
      #ljBNOT
      #ljLESS
      #ljLESSEQUAL
      #ljGREATER
      #ljGreaterEqual
      #ljEQUAL
      #ljNotEqual
      #ljAND
      #ljOr             ;20
      #ljXOR
      #ljSWAP
      #ljCALL
      #ljHALT
      #ljPush
      #ljFetch
      #ljStore          ;50
      #ljMOV
      #ljPUSHSP
      
      ; Functions
      
      #ljREDIM
      #ljCONCAT
      #ljINTFUNC
      #ljRND
      #ljPRTS
      #ljPRTC
      #ljPRINT
      #ljSizeOf
      #ljLEN
      #ljMID
      #ljFORMAT
      #ljTRIM
      #ljLSET
      #ljRSET
      #ljTypeOf
      #ljCapilize
      #ljFindStr
      #ljStrRep
      #ljSplitString
      #ljReverseString
      #ljCountString
      #ljRepString
      #ljDate
      #ljFormatDate
      #ljParseDate
      
      #ljNoMore
      
   EndEnumeration
   
; ======================================================================================================
;- Structures
; ======================================================================================================

   Structure stTree
      NodeType.w
      IdentType.w
      VarType.u
      ModID.u
      value.s
      VarName.s
      *left.stTree
      *right.stTree
      *middle.stTree
   EndStructure

   Structure stSymbols
      name.s
      TokenType.u
   EndStructure

   Structure   stFunc
      retType.w
      TokenId.u
      params.u
   EndStructure

   Structure stToken
      Value.s
      TokenType.u
      VarType.u
      row.u
      col.u
      ModID.u
      *p.stToken
   EndStructure

   Structure stS
      *p.stToken
      VarType.u
      Index.u
      ModID.u
      ArSize.l
      Value.s
      shortname.s
      bConstant.u
   EndStructure
   
   Structure stPrec
      bRightAssociation.w
      bBinary.w
      bUnary.w
      Precedence.w
      NodeType.w
      pcode.s
   EndStructure
   
   Structure stType
      code.a
      flag.a
      i.w
      ndx.u
   EndStructure
   
   Structure stGlobals
      flag.u
      arSize.u
      Array i.i(1)
      Array d.d(1)
      Array ss.s(1)
   EndStructure

   Structure stModInfo
      *code.stTree
      function.l
      params.l
      count.w
      *NewPos
   EndStructure
   
   Structure stCall
      function.i
      id.l
      *src
      *location
   EndStructure
   
   Structure stCallStack
      sp.l
      pc.l
   EndStructure
   
; ======================================================================================================
;- Module Macros
; ======================================================================================================
   ;-------------------------
   ;- Debugging only macros
   ;-------------------------
   Macro                DebugParser( gadget )
   
      Logging( gadget, "---[ Parser ]--------------" )
      Logging( gadget, ";Variables" )
      
      ForEach mapVariables()
         *g = mapVariables()\p
         If *g\p
            Logging( gadget, MapKey( mapVariables() ) + " (" + gszATR( *g\p\TokenType ) + ")" )
         EndIf
      Next
      
      Logging( gadget, ";Syntax" )
   
      ForEach llTokenList()
         temp = RSet( Str(llTokenList()\row), 4 ) + "   " + RSet( Str(llTokenList()\col), 4 ) + " "
         temp + LSet( gszATR( llTokenList()\TokenType ), 20 ) + llTokenList()\Value
         
         If llTokenList()\p
            *g = llTokenList()\p
            
            If *g\Value > ""
               temp + " (" + *g\Value + ")"
            EndIf
         EndIf
         
         Logging( gadget, temp )
      Next
      
      Logging( gadget, "---[ End ]--------------" )
      
   EndMacro
   
   Macro                DebugStack( pos )
      If pos >= 0 
         Select myStack( pos )\type
            Case #ljString
               Debug "Stack(" + Str(pos) + ")-String = " + myStack( pos )\ss(0)
               
            Case #ljFloat
               Debug "Stack(" + Str(pos) + ")-Float = " + StrD( myStack( pos )\d(0) )
               
            Default
               Debug "Stack(" + Str(pos) + ")-Int = " + Str(myStack( pos )\i(0) )
         
         EndSelect
      EndIf
   EndMacro
   
   Macro             DebugListCode()
      
      Debug ";-- Code Section --"
   
      ForEach llObjects()
         line = LSet( Str( ListIndex(llObjects()) ), 6 )
         Line + MakeASMLine( llObjects() )
         If llObjects()\i <> 0
            Line + " " + Str( llObjects()\i )
            If llObjects()\ndx <> 0
               Line + " [" + Str( llObjects()\ndx ) + "]"
            EndIf
         EndIf
         Debug line
      Next

      Debug ";-- End --"
   EndMacro
   
   Macro                DebugListVars()
      Debug "#################[ CODE GENERATOR ]#################"
      Debug "--- Const ---"
      
      ForEach mapVariables()
         If mapVariables()\bConstant
            temp = Str( mapVariables()\Index) + " --> " + gszInv + mapVariables()\Value + gszInv
            Debug temp + " (" + gszAtr(mapVariables()\VarType)+")"
         EndIf
      Next

      Debug "--- Vars ---"
   
      ForEach mapVariables()
         If Not mapVariables()\bConstant
            temp = Str( mapVariables()\Index) + " -- " + MapKey( mapVariables() )
            If mapVariables()\ArSize
               temp + " (Array)"
            EndIf
            
            Debug temp + " (" + gszAtr(mapVariables()\VarType)+")"
         EndIf
      Next
      
      Debug "--- End ---"
   EndMacro
   ;-------------------------
   ;- Parser Macros
   ;-------------------------
   Macro                AddModule( modname )
      
      If FindMapElement( mapModules(), modname )
         SetError( "Function already declared", 16 )
      Else
         AddMapElement( mapModules(), modname )
         mapModules()\function      = gNextFunc
         mapModules()\NewPos        = 0
         gszFuncNames( gNextFunc )  = modname
         mapModules()\params        = mapFunctions()\params
         gNextFunc + 1
         
         Debug "New function: " + modname
      EndIf
   
   EndMacro
   
   Macro                NextToken()
      NextElement( llTokenList() )
   EndMacro
   
   Macro                Install( symbolname, id  )
      AddElement( llSymbols() )
         llSymbols()\name        = LCase( symbolname )
         llSymbols()\TokenType   = id

   EndMacro
   
   Macro                AddFunction( funcname, type, token )
      temp = LCase( funcname )
      AddMapElement( mapFunctions(), temp )
      mapFunctions()\retType     = type
      mapFunctions()\TokenId     = token
   
   EndMacro
   
   Macro                AddToken( tkentype, text )
      AddElement( llTokenList() )
         llTokenList()\TokenType    = tkentype
         llTokenList()\row          = gLineNumber
         llTokenList()\col          = gCol
         llTokenList()\value        = text
         llTokenList()\ModID        = mapModules()\function 
         
      If tkentype = #ljSTRING
         llTokenList()\VarType = #ljSTRING
      EndIf
      
      ;Debug "New Token: " + gszAtr( tkentype ) + " = " + text
      
   EndMacro
   
   Macro                NextCharacter()
      gNextChar = memReadStrBySize(Handle,1,gFileFormat)
      
      ;Debug gNextChar
      
      gPos + 1 : gCol + 1
      
      If gNextChar = #LF$
         gCol = 1
         gLineNumber + 1
         
         gNextChar = memReadStrBySize(Handle,1,gFileFormat)
         gPos + 1
      EndIf
   EndMacro
   
   Macro             ChangePos( offpos )
      gPos + offpos
      memFileSeek( handle, memLoc(handle) + offpos )
   EndMacro
   
   Macro                SetPre( id, right, bi, un, prec, pc )
   
      gPreTable( id )\NodeType            = id
      gPreTable( id )\bRightAssociation   = right
      gPreTable( id )\bBinary             = bi
      gPreTable( id )\bUnary              = un
      gPreTable( id )\Precedence          = prec
      gPreTable( id )\PCode               = pc
      
   EndMacro
   
   Macro                SetPre2( id, op )
   
      gPreTable( id )\bRightAssociation   = 0
      gPreTable( id )\bBinary             = 0
      gPreTable( id )\bUnary              = 0
      gPreTable( id )\Precedence          = -1
      gPreTable( id )\NodeType            = op
      
   EndMacro
   
   Macro                AddNode()
      AddElement( llNodes() )
      llNodes() = *p
   EndMacro
   
   Macro                SetAssign()
      ;Debug "---------------------------"
      ;Debug "SetAssign: " + LastIdent
      ;Debug "From token: " + gszAtr( llTokenList()\VarType )
      
      If LastIdent  > "" And llTokenList()\VarType
         *p = llTokenList()

         FindVariable( LastIdent, mapModules()\function )
         
         If gFound > -1
            If mapVariables()\p\VarType <> #ljString
               llTokenList()\TokenType = mapVariables()\p\VarType
            EndIf
            
            ;Debug "SetAssign: " + gszATR( mapVariables()\p\VarType )
         
            llTokenList()\p = mapVariables()\p
            ChangeCurrentElement( llTokenList(), mapVariables()\p )
            llTokenList()\p = *p
            
            Debug "[LINK] " + gszATR( llTokenList()\TokenType ) + " --> " + llTokenList()\Value
            
            LastElement( llTokenList() )
         Else
            SetError( "variable undefined '" + LastIdent + "'", 11 )
         EndIf
      EndIf
   EndMacro
   
   Macro                CommaStream( cmd )
   
      ;Debug gszAtr( llTokenList()\TokenType ) + " -- " + gszAtr( llTokenList()\VarType )
   
      If llTokenList()\TokenType = #ljSTRING
         *r = Makeleaf( #ljSTRING, llTokenList()\value, llTokenList()\ModID )
         *e = MakeNode( cmd, *r, 0, llTokenList()\ModID )
         NextToken()
      ElseIf llTokenList()\VarType = #ljFLOAT
         *r = expr( 0, bExpression )
         *e = MakeNode( cmd, *r, 0, llTokenList()\ModID )
      Else
         *r = expr( 0, bExpression )
         *e = MakeNode( cmd, *r, 0, llTokenList()\ModID )
      EndIf

   EndMacro
   
   Macro                AddNewVariable( varname, token, isconstant )
      
      If mapModules()\function = 0 Or isconstant
         AddMapElement( mapVariables(), varname )
      Else
         AddMapElement( mapVariables(), gszFuncNames( mapModules()\function ) + "-" + varname )
      EndIf            
            
      mapVariables()\p        = token
      mapVariables()\Index    = gVarPos
      mapVariables()\ModID    = mapModules()\function
      mapVariables()\shortname= varname
      mapVariables()\VarType  = llTokenList()\VarType
      
      CompilerIf isconstant
         mapVariables()\value       = varname
         mapVariables()\bConstant   = 1
      CompilerEndIf

      *pVars(gVarPos) = mapVariables()

      Debug "- Adding variable: " + varname
      gVarPos + 1
   EndMacro

   Macro                FindVariable( varname, modnameid )
      gFound = -1
      ;Debug "1. FindVariable> " + varname 
   
      If FindMapElement( mapVariables(), varname )
         gFound = mapVariables()\Index
      Else
         ;Debug "2. FindVariable> " + gszFuncNames( modnameid ) + "-" + varname
      
         If FindMapElement( mapVariables(), gszFuncNames( modnameid ) + "-" + varname )
            gFound = mapVariables()\Index
         EndIf
      EndIf
   EndMacro
   ;-------------------------
   ;- Compiler Macros
   ;-------------------------
   Macro                cg_expr_X_2( text, cmd )
      Case cmd
         NextToken()
         expect( text, #ljLeftParent )
         *r = expr( 0, bExpression )
         expect( text, #ljComma )
         *e = expr( 0, bExpression )
         expect( text, #ljRightParent )
         *x = MakeNode( cmd, *r, *e, llTokenList()\ModID )
   EndMacro

   Macro                cg_expr_X_1( cmd )
      Case cmd
            NextToken()
            *r = paren_expr( bExpression )
            *x = MakeNode( cmd, 0, *r, llTokenList()\ModID )

   EndMacro
   
   Macro                SetError( text, err )
   
      If err > 0 And err < 10
         gszlastError   = text + " on line " + Str( gLineNumber ) + ", col = " + Str( gCol ) + " , memsize = " + Str( gMemSize )
      ElseIf err > 10
         gszlastError   = text + " on line " + Str( llTokenList()\row ) + ", col = " + Str( llTokenList()\col )
      Else
         gszlastError   = text
      EndIf
      
      gLastError     = err
      
      ProcedureReturn err
   EndMacro
   
   Macro                cg_SetFlagOnFoundVar()
       n = gFound
            
      If n < 0
         SetError( "Variable not defined " + *x\value, 22 )
      EndIf
      
      If mapVariables()\VarType = #ljFLOAT
         flag = #LJMOD_FLOAT
      ElseIf mapVariables()\VarType = #ljSTRING
         flag = #LJMOD_STRING
      Else
         flag = #LJMOD_INT
      EndIf
   EndMacro
   
   Macro                cg_DebugVariableName(mcode)
      If bShowDebug
         symb = Trim(Symbols(mcode))
         If symb = ""
            symb = Str( mcode )
         EndIf
      Else
         symb = Str( mcode )
      EndIf
   EndMacro
   ;-------------------------
   ;- Virtual Machine macros
   ;-------------------------
   Macro                vm_CheckStack()
      sp + 1
      
      If sp > #MAX_VARS
         Debug "Out of stack space"
         ProcedureReturn -53
      EndIf
   EndMacro
   
   Macro                vm_ListToArray( ll, ar )
      i = ListSize( ll() )
      ReDim ar( i )
      i = 0
      
      ForEach ll()
         ar( i ) = ll()
         i + 1
      Next
   EndMacro

   Macro                vm_IfElseLogic()
      myStack( sp - 1 )\i( 0 ) = #True
      Else
         myStack( sp - 1 )\i( 0 ) = #False
      EndIf

   EndMacro
   
   Macro                vm_DoMaths( marker )
   
      If myStack( sp - 1 )\flag & #LJMOD_FLOAT And myStack( sp)\flag & #LJMOD_FLOAT
         myStack( sp - 1 )\d( 0 ) marker myStack( sp )\d( 0 )
      ElseIf myStack( sp - 1 )\flag & #LJMOD_INT And myStack( sp)\flag & #LJMOD_INT
         myStack( sp - 1 )\i( 0 ) marker myStack( sp )\i( 0 )
      Else
         If myStack( sp - 1 )\flag & #LJMOD_FLOAT
            If myStack( sp )\flag & #LJMOD_INT
               myStack( sp - 1 )\d( 0 ) marker  myStack( sp )\i( 0 )
            ElseIf myStack( sp )\flag & #LJMOD_STRING
               myStack( sp - 1 )\d( 0 ) marker ValD( myStack( sp )\ss( 0 ) )
            EndIf
         ElseIf myStack( sp - 1 )\flag & #LJMOD_INT
            If myStack( sp )\flag & #LJMOD_FLOAT
               myStack( sp - 1 )\i( 0 ) marker myStack( sp )\d( 0 )
            ElseIf myStack( sp )\flag & #LJMOD_STRING
               myStack( sp - 1 )\i( 0 ) marker Val( myStack( sp )\ss( 0 ) )
            EndIf
         ElseIf myStack( sp - 1 )\flag & #ljMOD_STRING
            myStack( sp - 1 )\flag = #LJMOD_INT | #ljMOD_FLOAT
            myStack( sp - 1 )\d(0) = ValD(myStack( sp - 1 )\ss(0))
            myStack( sp - 1 )\i(0) = myStack( sp - 1 )\d(0)
            
            If myStack( sp )\flag & #LJMOD_FLOAT
               myStack( sp - 1 )\d( 0 ) marker myStack( sp )\d( 0 )
               myStack( sp - 1 )\i( 0 ) = myStack( sp - 1 )\d( 0 )
            ElseIf myStack( sp )\flag & #LJMOD_INT
               myStack( sp - 1 )\i( 0 ) marker myStack( sp )\i( 0 )
               myStack( sp - 1 )\d( 0 ) = myStack( sp - 1 )\i( 0 )
            ElseIf myStack( sp )\flag & #LJMOD_STRING
               myStack( sp - 1 )\d( 0 ) marker ValD( myStack( sp )\ss( 0 ) )
               myStack( sp - 1 )\i( 0 ) = myStack( sp - 1 )\d( 0 )
            EndIf
         EndIf
      EndIf
      
      sp - 1
      ;Debug "3. " + StrD( mystack( sp  )\d( 0 ) ) + " , " + Str( mystack( sp  )\i( 0 ) )
      
   EndMacro

   Macro                vm_Operand( marker )
      If myStack( sp - 1 )\flag & #ljMOD_STRING
         If myStack( sp )\flag & #ljMOD_FLOAT
            If myStack( sp - 1 )\ss( 0 ) marker StrD( myStack( sp )\d( 0 ) )
            vm_ifElseLogic()
         ElseIf myStack( sp )\flag & #ljMOD_STRING
            If myStack( sp - 1 )\ss( 0 ) marker myStack( sp )\ss( 0 )
            vm_ifElseLogic()
         ElseIf myStack( sp )\flag & #ljMOD_INT
            If myStack( sp - 1 )\ss( 0 ) marker Str( myStack( sp )\i( 0 ) )
            vm_ifElseLogic()
         EndIf
      ElseIf myStack( sp - 1 )\flag & #ljMOD_FLOAT
         If myStack( sp )\flag & #ljMOD_FLOAT
            If myStack( sp - 1 )\d( 0 ) marker myStack( sp )\d( 0 )
               vm_ifElseLogic()
         ElseIf myStack( sp )\flag & #ljMOD_STRING
               If myStack( sp - 1 )\d( 0 ) marker ValD ( myStack( sp )\ss( 0 ) )
               vm_ifElseLogic()
         ElseIf myStack( sp )\flag & #ljMOD_INT
            If myStack( sp - 1 )\d( 0 ) marker myStack( sp )\i( 0 )
            vm_ifElseLogic()
         EndIf
      ElseIf myStack( sp - 1 )\flag & #ljMOD_INT
         If myStack( sp )\flag & #ljMOD_FLOAT
            If myStack( sp - 1 )\i( 0 ) marker myStack( sp )\d( 0 )
            vm_ifElseLogic()
         ElseIf myStack( sp )\flag & #ljMOD_STRING
            If myStack( sp - 1 )\i( 0 ) marker Val ( myStack( sp )\ss( 0 ) )
            vm_ifElseLogic()
         ElseIf myStack( sp )\flag & #ljMOD_INT
            If myStack( sp - 1 )\i( 0 ) marker myStack( sp )\i( 0 )
            vm_ifElseLogic()
         EndIf
      EndIf
      
      sp - 1
   EndMacro
   
   Macro                vm_BitOperand( marker )
      myStack( sp - 1 )\flag ! #ljMOD_NOTYP | #ljMOD_INT
      
      If myStack( sp )\flag & #ljMOD_FLOAT
         i = myStack( sp )\d( 0 )
         If myStack( sp - 1 )\i( 0 ) marker i
         vm_ifElseLogic()
      ElseIf myStack( sp )\flag & #ljMOD_STRING
         If myStack( sp - 1 )\i( 0 ) marker Val ( myStack( sp )\ss( 0 ) )
         vm_ifElseLogic()
      ElseIf myStack( sp )\flag & #ljMOD_INT
         If myStack( sp - 1 )\i( 0 ) marker myStack( sp )\i( 0 )
         vm_ifElseLogic()
      EndIf
      
      sp - 1
   EndMacro
   
   Macro                vm_XINT_1( cmd, rtype, runcmd )
      Case cmd
         CompilerIf rtype = #ljINTEGER
            myStack( sp )\flag = #ljMOD_INT
            myStack( sp )\i( 0 ) = runcmd
         CompilerElse
            If myStack( sp )\flag & #ljMOD_INT
               myStack( sp )\ss(0) = Str( myStack( sp )\i(0) )
            ElseIf myStack( sp )\flag & #ljMOD_FLOAT
               myStack( sp )\ss(0) = StrD( myStack( sp )\d(0) )
            EndIf
            
            myStack( sp )\flag = #ljMOD_STRING
            myStack( sp )\ss(0)  = runcmd( myStack( sp )\ss(0) )
         CompilerEndIf
   EndMacro
   
   Macro                vm_XINT_2( cmd, runcmd )
      Case cmd
         If myStack( sp - 1)\flag & #LJMOD_FLOAT
            myStack( sp - 1)\ss(0)  = StrD( myStack( sp - 1)\d(0) )
            myStack( sp - 1)\flag = #ljMOD_STRING
         ElseIf myStack( sp - 1)\flag & #LJMOD_INT
            myStack( sp - 1)\ss(0)  = Str( myStack( sp - 1)\d(0) )
            myStack( sp - 1)\flag = #ljMOD_STRING
         ElseIf myStack( sp - 1)\flag & #ljMOD_STRING
            myStack( sp - 1)\ss(0)  = myStack( sp - 1)\ss(0)
         EndIf
         
         If myStack( sp )\flag & #ljMOD_STRING
            i = Val( myStack( sp  )\ss(0) )
         ElseIf myStack( sp )\flag & #ljMOD_FLOAT
            i = myStack( sp  )\d(0)
         ElseIf myStack( sp )\flag & #ljMOD_INT
            i = myStack( sp  )\i(0)
         EndIf
         
         myStack( sp - 1)\ss(0) = runcmd( myStack( sp - 1)\ss(0), i )
         sp - 1
   EndMacro
   
   Macro                vm_XSTR_2( cmd, runcmd )
      Case cmd
         Select myStack( sp - 1)\type
            Case #ljFLOAT
               myStack( sp - 1)\ss(0)  = StrD( myStack( sp - 1)\d(0) )
               myStack( sp - 1)\type   = #ljSTRING
               
            Case #ljINTEGER
               myStack( sp - 1)\ss(0)  = Str( myStack( sp - 1)\i(0) )
               myStack( sp - 1)\type   = #ljSTRING
         EndSelect
         
         If myStack( sp )\flag = #ljINTEGER
            myStack( sp  )\ss(0) ) = Str( myStack( sp  )\i( 0 ) )
         ElseIf myStack( sp )\flag = #ljFLOAT
            myStack( sp  )\ss(0) ) = StrD( myStack( sp  )\d( 0 ) )
         EndIf
         
         myStack( sp - 1)\ss(0) = runcmd( myStack( sp - 1)\ss(0), myStack( sp )\ss(0) )
         sp - 1
   EndMacro

   Macro                vm_CONV2STR( stp, withreset )
      If myStack( stp )\flag & #ljMOD_FLOAT
         myStack(stp)\ss(0)  = StrD( myStack(stp)\d(0) )
         CompilerIf withreset
            myStack(stp)\flag ! #ljMOD_NOTYP | #ljMOD_STRING
         CompilerEndIf
      ElseIf myStack( stp )\flag & #ljMOD_INT
         myStack(stp)\ss(0)  = Str( myStack(stp)\i(0) )
         CompilerIf withreset
            myStack(stp)\flag ! #ljMOD_NOTYP | #ljMOD_STRING
         CompilerEndIf
      EndIf
   EndMacro
   
   Macro                vm_CONV2INT( stp, withreset )
      If myStack( stp )\flag & #ljMOD_FLOAT
         myStack(stp)\i(0)  = myStack(stp)\d(0)
         CompilerIf withreset
            myStack(stp)\flag ! #ljMOD_NOTYP | #ljMOD_INT
         CompilerEndIf
      ElseIf myStack( stp )\flag & #ljMOD_STRING
         myStack(stp)\i(0)  = Val( myStack(stp)\ss(0) )
         CompilerIf withreset
            myStack(stp)\flag ! #ljMOD_NOTYP | #ljMOD_INT
         CompilerEndIf
      EndIf
   EndMacro
   
   Macro                vm_POP()
      sp - 1
      ;If sp < 0 
      ;   sp = 0
      ;EndIf
   EndMacro
   
   Macro                vm_Store()
      If Not CPC()\flag & #LJMOD_ARRAY
         If CPC()\flag & #LJMOD_FLOAT
            *arData(CPC()\i)\d(0) = myStack(sp)\d(0)
         ElseIf CPC()\flag & #LJMOD_INT
            *arData(CPC()\i)\i(0) = myStack(sp)\i(0)
         ElseIf CPC()\flag & #LJMOD_STRING
            *arData(CPC()\i)\ss(0) = myStack(sp)\ss(0)
         EndIf
      Else
         If CPC()\flag & #LJMOD_STACK
            ndx = myStack(sp)\i(0)
            sp - 1
         Else
            ndx = *arData(CPC()\ndx)\i(0)
         EndIf
         
         If CPC()\flag & #LJMOD_INT
            *arData(CPC()\i)\i( ndx ) = myStack(sp)\i(0)
         ElseIf CPC()\flag & #LJMOD_FLOAT
            *arData(CPC()\i)\d( ndx ) = myStack(sp)\d(0)
         ElseIf CPC()\flag & #LJMOD_STRING
            *arData(CPC()\i)\ss(ndx) = myStack(sp)\ss(0)
         EndIf
      EndIf

      sp - 1

   EndMacro
   ;-------------------------
   ;- Save Typing
   ;-------------------------
   Macro                CPC()
                        code(pc)
   EndMacro
   ;-------------------------
   ;- To Delete
   ;-------------------------
   Macro                CheckDest( spv )
      If *arData( ndx )\arSize > 0
         rc + 1
         arx = myStack( spv )\i(0)
       
         If arx < 0 Or arx >= *arData( ndx )\arSize
            Debug "Array out of bounds - " + Str( arx )
            ProcedureReturn -52
         EndIf
      Else
         arx = 0
      EndIf
   EndMacro
      
   Macro                stCopy( src, dst )
   
      Select src\type
         Case #ljString      
            dst\type    = #ljString
            dst\ss(0)   = src\ss(0)
            
         Case #ljFloat
            dst\type = #ljFloat
            dst\d(0) = src\d(0)
            
         Case #ljINTEGER
         ;Default
            dst\type = #ljINTEGER
            dst\i(0) = src\i(0)
            
         Default
            Debug "stCopy unknown type: " + Str( src\type )
      
      EndSelect
      
   EndMacro
   ;-------------------------
   ;- End Macros
   ;-------------------------

; ======================================================================================================
;- Module Globals
; ======================================================================================================
  
   Global Dim           gFlags( 10 )
   Global Dim           gszATR.s( #LJTOKENS )
   Global Dim           gPreTable.stPrec( #LJTOKENS )
   
   Global NewMap        mapVariables.stS()
   Global NewMap        mapFunctions.stFunc()
   Global NewMap        mapModules.stModInfo()

   Global NewList       llSymbols.stSymbols()
   Global NewList       llTokenList.stToken()
   Global NewList       llObjects.stType()
   Global NewList       llNodes()
   Global NewList       llFuncPos.stCall()
   Global NewList       llHoles.stCall()
   
   Global Dim           gszFuncNames.s( #MAX_FUNCS + 1 )
   Global Dim           *pVars.sts(#MAX_VARS+1)
   
   Global               gFileFormat
   Global               Handle
   Global               *gMem
   
   Global               gLineNumber
   Global               gStack
   Global               gCol
   Global               gMemSize.q
   Global               gPos
   Global               gExit
   Global               gLastOp
   Global               gVarPos
   Global               gNextFunc
   Global               gLastError
   Global               gFound
   Global               gHoles
   Global               *gLastSemi
   
   Global               gszFileText.s
   Global               gszlastError.s
   Global               gNextChar.s
   Global               gszTemp.s
   Global               gszInv.s          = Chr( 34 )
   Global               gszEOF.s          = Chr( 255 )
   Global               *g.stToken
   
   Global               gszNL.s           = "\n"
   Global               gszNULL.s         = ""
   Global               gszGlobal.s       = "#glob"
   
; ======================================================================================================
;- Procedure Definitions
; ======================================================================================================
      
   Declare              paren_expr( bExpression = 0 )
   Declare              GetConstantIndex( text.s )
   Declare              expr( var, bExpression = 0, Type = 0 )
   
; ======================================================================================================
;- Module Function
; ======================================================================================================
   
   gFlags( #ljWinDelay )      = 8
   gFlags( #ljShowRunCode )   = 0
   gFlags( #ljListParse )     = 0
   gFlags( #ljListSyntax )    = 0
   gFlags( #ljFastConsole )   = 1
   gFlags( #ljListASM )       = 0
   
   Procedure            DebugFlags( const, var = -1 )
      If const > #ljListASM Or const < 0 
         ProcedureReturn -1
      EndIf
      
      If var < 0
         ProcedureReturn gFlags( const )
      Else
         gFlags( const ) = var
      EndIf
   
   EndProcedure


   Procedure            Logging( id, Text.s, pos = -1 )
      Protected         rows
      
      If id >= 0
         AddGadgetItem( id, pos, Text )
         
         If pos < 0
            rows = CountGadgetItems( id )
            SetGadgetState( id, rows - 1)
         EndIf
         
         SetGadgetState( id, -1 )
         
         If gFlags( #ljWinDelay )
            WaitWindowEvent( gFlags( #ljWinDelay ) )
         EndIf
      EndIf
   EndProcedure
   
   Procedure            Display( id, text.s )
      Static            flag = 0
      Static            line.s
      Protected         rows, l, bLF
      Protected         temp.s
      
      If id >= 0
         id + 1
         
         If gFlags( #ljFastConsole )
            If Not flag
               AddGadgetItem( id, -1, "" )
               flag + 1
            EndIf
         EndIf
         
         l = Len( text )
       
         If l > 1  
            If Right( Text, 2 ) = gszNL
               bLF = 1
               text = Left( Text, l - 2 )
            EndIf
         ElseIf l = 1
            If text = #CR$ Or text = #LF$
               bLF = 1
            EndIf
         EndIf
         
         If gFlags( #ljFastConsole )
            line + text
            
            If bLF
               AddGadgetItem( id, -1, line )
               line = ""
            EndIf
         Else
            rows = CountGadgetItems( id )
            temp = GetGadgetItemText( id, rows - 1 )
            temp + text
            SetGadgetItemText( id, rows - 1, temp )
            SetGadgetState( id, -1 )
            
            If bLF
               AddGadgetItem( id, -1, "" )
            EndIf
         EndIf
         
         If gFlags( #ljWinDelay )
            WaitWindowEvent( gFlags( #ljWinDelay ) )
         EndIf
      EndIf
   EndProcedure
   ;----------------------------------------------------------------------------------------
   ;- 1. Init
   ;- Compiler initializes
   ;----------------------------------------------------------------------------------------
   Procedure            Init()
      Protected         temp.s
      Protected         i
   
      Restore  TOKENS

      Repeat
         Read.s     temp
         
         If temp = "-" : Break : EndIf
         gszATR( i ) = temp
         i + 1
      ForEver

      SetPre2( #ljEOF, -1 )
      SetPre2( #ljNOOP, -1 )
      SetPre( #ljMULTIPLY,     0, 1, 0, 13, "MUL" )
      SetPre( #ljDIVIDE,       0, 1, 0, 13, "DIV"  )
      SetPre( #ljMOD,          0, 1, 0, 13, "MOD" )
      SetPre( #ljADD,          0, 1, 0, 12, "ADD" )
      SetPre( #ljSUBTRACT,     0, 1, 0, 12, "SUB" )
      SetPre( #ljNEGATE,       0, 0, 1, 14, "NEG" )
      SetPre( #ljNOT,          0, 0, 1, 14, "NOT" )
      SetPre( #ljBNOT,         0, 0, 1, 14, "BNOT" )
      SetPre( #ljLESS,         0, 1, 0, 10, "LT" )
      SetPre( #ljLESSEQUAL,    0, 1, 0, 10, "LE" )
      SetPre( #ljGREATER,      0, 1, 0, 10, "GT" )
      SetPre( #ljGreaterEqual, 0, 1, 0, 10, "GE" )
      SetPre( #ljEQUAL,        0, 1, 0, 9,  "LE" )
      SetPre( #ljNotEqual,     0, 1, 0, 9,  "NE" )
      SetPre2( #ljASSIGN,      #ljASSIGN )
      SetPre( #ljAND,          0, 1, 0, 5, "AND" )
      SetPre( #ljOr,           0, 1, 0, 4, "OR" )
      SetPre2( #ljIF,          #ljIF )
      SetPre2( #ljElse,        -1 )
      SetPre2( #ljWHILE,       #ljWHILE )
      SetPre2( #ljPRTS,        -1 )
      SetPre2( #ljPRTC,        -1 )
      SetPre2( #ljPRINT,        -1 )
      SetPre2( #ljCONCAT,       -1 )
      SetPre2( #ljLeftParent,  -1 )
      SetPre2( #ljLeftCurly,   -1 )
      SetPre2( #ljRightParent,  -1 )
      SetPre2( #ljRightCurly, -1 )
      SetPre2( #ljComma,  -1 )
      SetPre2( #ljSemi,  -1 )
      SetPre2( #ljReturn,  -1 )
     
      SetPre2( #ljIDENT,    #ljIDENT )
      SetPre2( #ljINTEGER,  #ljINTEGER )
      SetPre2( #ljFLOAT,    #ljFLOAT )
      SetPre2( #ljSTRING,   #ljSTRING )
      
      SetPre2( #ljDim,       -1 )
      SetPre2( #ljINTFUNC,    -1 )
      SetPre2( #ljREDIM,    -1 )
      SetPre2( #ljRND,    -1 )
      SetPre2( #ljMID,    -1 )
      SetPre2( #ljSizeOf,    -1 )
      SetPre2( #ljLEN,    -1 )
      SetPre2( #ljFORMAT,    -1 )
      SetPre2( #ljLSET,    -1 )
      SetPre2( #ljRSET,    -1 )
      SetPre2( #ljTRIM,    -1 )
      
      ClearList( llFuncPos() )
      ClearList( llTokenList() )
      ClearList( llSymbols() )
      ClearList( llObjects() )
      ClearList( llHoles() )
      ClearMap( mapVariables() )
      ClearMap( mapModules() )
      ClearMap( mapFunctions() )
      
      If ListSize( llNodes() )
         ForEach llNodes()
            FreeStructure( llNodes() )
         Next
         
         ClearList( llNodes() )
      EndIf

      gLineNumber = 0
      gCol        = 1
      gStack      = 0
      gExit       = 0
      gPos        = 1
      gLastOp     = 0
      gNextFunc   = 0
      gVarPos     = 0
      gHoles      = 1

      ; Language constructs
      Install( "else",  #ljElse )
      install( "if",    #ljIF )
      install( "print", #ljPRINT )
      install( "prts", #ljPRTS )
      install( "putc",  #ljPRTC )
      install( "dim",  #ljDIM )
      install( "while", #ljWHILE )
      install( "redim", #ljREDIM )
      install( "declare", #ljDeclare )
      install( "declare.i", #ljDeclare )
      install( "declare$", #ljDeclare )
      install( "declare.s", #ljDeclare )
      install( "declare.d", #ljDeclare )
      install( "declare.f", #ljDeclare )
      install( "func", #ljFunction )
      install( "return", #ljReturn )

      ; Inbuilt library functions
      AddFunction( "concat",  #ljSTRING,  #ljCONCAT )
      AddFunction( "intf",  #ljINTEGER,  #ljINTFUNC )
      AddFunction( "random",  #ljINTEGER,  #ljRND )
      AddFunction( "sizeof", #ljINTEGER, #ljSizeOf )
      AddFunction( "len", #ljINTEGER, #ljLEN )
      AddFunction( "mid", #ljSTRING, #ljMID )
      AddFunction( "trim", #ljSTRING, #ljTRIM )
      AddFunction( "format", #ljSTRING, #ljFORMAT )
      AddFunction( "lset", #ljSTRING, #ljLSET )
      AddFunction( "rset", #ljSTRING, #ljRSET )
      AddFunction( "typeof", #ljSTRING, #ljTypeOf )
      AddFunction( "capitalize", #ljSTRING, #ljCapilize )
      AddFunction( "findstr", #ljINTEGER, #ljFindStr )
      AddFunction( "strrep", #ljSTRING, #ljStrRep )
      AddFunction( "splitstring", #ljSTRING, #ljSplitString )
      AddFunction( "revstring", #ljSTRING, #ljReverseString )
      AddFunction( "countstring", #ljINTEGER, #ljCountString )
      AddFunction( "repstring", #ljSTRING, #ljRepString )
      AddFunction( "date", #ljINTEGER, #ljDate )
      AddFunction( "formatdate", #ljSTRING, #ljFormatDate )
      AddFunction( "parsedate", #ljINTEGER, #ljParseDate )
      
      ; These constants are always pre-defined
      AddModule( gszGlobal )
      
      AddToken( #ljBOF, "" )
      llTokenList()\VarType = #ljSTRING
      AddNewVariable( "", llTokenList(), 1 )
      mapVariables()\shortname   = "$Null"
      AddNewVariable( "0", llTokenList(), 1 )
      mapVariables()\shortname   = "$Zero"
      AddNewVariable( "\n", llTokenList(), 1 )
      mapVariables()\shortname   = "$NL"
      AddNewVariable( "0", llTokenList(), 1 )
      mapVariables()\shortname   = "$Zero"
      mapVariables()\VarType     = #ljINTEGER
      
      *gLastSemi = llTokenList()
      gLineNumber + 1

   EndProcedure
   
   Procedure            LoadLJ( filename.s )
      Protected         f, *mem, FileType
   
      gMemSize = FileSize( filename )
      
      If gMemSize > 0
         f = ReadFile( #PB_Any, filename, #PB_File_NoBuffering )
         gFileFormat = ReadStringFormat( f )                        ;  Thanks to Infratec and Mesa

         If Not f
            SetError( "Could not open file", -3 )
         EndIf
         
         If gFileFormat <> #PB_Ascii And gFileFormat <> #PB_UTF8 And gFileFormat <> #PB_Unicode
            gFileFormat = #PB_Unicode
         EndIf
         
         If Handle
            memDestroy( Handle )
         EndIf
         
         If *gMem
            FreeMemory( *gMem )
         EndIf

         *gMem = AllocateMemory( gMemSize + 16, #PB_Memory_NoClear )
         ReadData( f, *gMem, gMemSize )
         CloseFile( f )
         
         Handle = memCreateFromPTR( *gMem, gMemSize + 16 )
         memFileSeek(Handle, memLof( Handle ) - 16 )
         memWriteString( Handle, gszEOF, gFileFormat )
         memFileSeek( Handle, 0 )

         Init()
         ProcedureReturn 0
      EndIf
      
      SetError( "Invalid file", -2 )
   EndProcedure
   
   Procedure.s             Error( *error.Integer )
      Protected            szerror.s
   
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
   
   Procedure               IsNumber( init.i = 0 )
      Static               flag
      
      If init
         flag = 0
      Else
         If gNextChar >= "0" And gNextChar <= "9"
            ProcedureReturn 1
         ElseIf Not flag And gNextChar = "."
            flag + 1
            ProcedureReturn 1
         EndIf
      EndIf
   
      ProcedureReturn 0
   
   EndProcedure
   
   Procedure               IsAlpha()
      If ( gNextChar >= "a" And gNextChar <= "z" ) Or (gNextChar >= "A" And gNextChar <= "Z"  ) Or IsNumber()
         ProcedureReturn 1
      EndIf
      
      ProcedureReturn 0
   EndProcedure
   
   Procedure               Follow( expect.s, ifyes.i, ifno.i, *err.Integer )
      NextCharacter()
      
      If gNextChar = expect
         AddToken( ifyes, "" )
         gLastOp = ifyes
      Else
         If ifno = -1
            *err\i = 5
            SetError( "follow unrecognized character", 5 )
         Else
            AddToken( ifno, "" )
            ChangePos( -1 )
            gLastOp = ifno
         EndIf
      EndIf
      
      ProcedureReturn 0
   EndProcedure
   
   Procedure            PeekNumber()
      Protected         text.s
   
      While gPos < gMemSize
         NextCharacter()
      
         Select gNextChar
            Case gszEOF, #CR$, ";"
               Break
   
            Case " ", #TAB$
               Continue
               
            Case "("
               NextCharacter()
               
               While gPos < gMemSize And ( IsNumber() Or gNextChar = ")" Or gNextChar = " " )
                  If gNextChar = ")"
                     ProcedureReturn Val( text )
                  EndIf
                  
                  text + gNextChar
                  NextCharacter()
               Wend
               Break
               
         EndSelect
      Wend

      ProcedureReturn -1

   EndProcedure

   Procedure.s          CreateVar( VarType = #ljINTEGER )
      Protected         var.s
      
      Debug "<--- Create variable --->"
      
      ChangeCurrentElement( llTokenList(), *gLastSemi )
      
      var = gszFuncNames( mapModules()\function ) + Str( mapModules()\count )
      mapModules()\count + 1
      
      AddToken( #ljIDENT, var )
      llTokenList()\VarType = VarType   
      AddNewVariable( var, llTokenList(), 0 )
      AddToken( #ljASSIGN, "" )
   
      If VarType = #ljINTEGER
         AddToken( #ljINTEGER, "0" )
      ElseIf VarType = #ljFLOAT
         AddToken( #ljFLOAT, "0.0" )
      Else
         AddToken( #ljIDENT, "" )
      EndIf
   
      AddToken( #ljSemi, "" )
      LastElement( llTokenList() )
      
      ProcedureReturn var

   EndProcedure
   ;----------------------------------------------------------------------------------------
   ;- 2 Parser
   ;- The parser/tokenizer and variable builder
   ;----------------------------------------------------------------------------------------
   Procedure            Scanner()
      Protected         err, first, i, dots
      Protected         BraceLevel, type
      Protected.s       text, temp, LastIdent, r
      Protected         *p
      Protected         PrevToken
      
      Repeat
         NextCharacter() 
         
         If memLoc(handle) > gMemSize
            AddToken( #ljEOF, "" )
            ProcedureReturn 0
         EndIf
         
         Select gNextChar
            Case gszEOF
               AddToken( #ljEOF, "" )
               ProcedureReturn 0

            Case " ", #CR$, #TAB$, ""
               Continue
            
            Case "{"
               AddToken( #ljLeftCurly, "" )
               BraceLevel + 1
            Case "}"
               AddToken( #ljRightCurly, "" )
               BraceLevel - 1
               
               If BraceLevel < 0
                  SetError( "Mismatched braces", 16 )
               ElseIf BraceLevel = 0
                  FindMapElement( mapModules(), "#glob" )
               EndIf
               
            Case "("
               AddToken( #ljLeftParent, "" )
            Case ")"
               AddToken( #ljRightParent, "" )
            Case "+"
               AddToken( #ljADD, "" )
            Case "-"
               AddToken( #ljSUBTRACT, "" )
            Case "*"
               AddToken( #ljMULTIPLY, "" )
            Case "%"    
               AddToken( #ljMOD, "" )
            Case ";"
               AddToken( #ljSemi, "" )
               *gLastSemi  = llTokenList()
               gLastOp     = 0
               LastIdent   = ""
               
            Case ","
               AddToken( #ljComma, "" )
            Case "/"
               NextCharacter()
               
               If gNextChar = "/"    ;Single line comment  
                  Repeat
                     NextCharacter()
                     
                     If gNextChar = #CR$
                        Break
                     EndIf
                  Until memLoc(handle) > gMemSize
                  
                  If memLoc(handle) > gMemSize
                     SetError( "EOF in comment", 1 )
                  EndIf
               ElseIf gNextChar <> "*"
                  AddToken( #ljDIVIDE, "" )
                  ChangePos( -1 )
               Else                  ;Must be a multi-line comment
                  NextCharacter()
                  
                  Repeat
                     NextCharacter()
                     
                     If gNextChar = "*"
                        NextCharacter()
                        
                        If gNextChar = "/"
                           Break
                        EndIf
                     ElseIf gNextChar = #CR$
                        ;Do nothing
                     EndIf

                  Until memLoc(handle) > gMemSize
                  
                  If memLoc(handle) > gMemSize
                     SetError( "EOF in comment", 1 )
                  EndIf
               EndIf
            Case "'"
               NextCharacter()
               
               If gNextChar = "'"
                  SetError( "Empty character literal", 2 )
               ElseIf gNextChar = "\"
                  NextCharacter()
                  
                  Select gNextChar
                     Case "'"
                        SetError( "Empty escape character literal", 2 )
                     Case "n"
                        first = 10
                     Case "r"
                        first = 13
                     Case gszInv
                        first = Asc( gszInv )
                     Case "\"
                        first = 92
                     Default
                        SetError( "Invalid escape character", 3 )
                  EndSelect
               Else
                  first = Asc( gNextChar )
               EndIf
               
               NextCharacter()
               
               If gNextChar <> "'"
                  SetError( "Multi-Character literal", 4 )
               Else
                  AddToken( #ljINTEGER, Str(first) )
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
            
            Case gszInv
               text = ""
               
               Repeat
                  NextCharacter()
                  
                  If gNextChar = gszInv
                     AddToken( #ljSTRING, text )
                     GetConstantIndex( text )
                     Break
                  ElseIf gNextChar = #CR$
                     SetError( "EOL in string", 7 )
                  Else
                     text + gNextChar
                  EndIf
               
               Until memLoc(handle) >= gMemSize
               
               If memLoc(handle) >= gMemSize
                  SetError( "EOF in string", 6 )
               EndIf
               
            Default
               IsNumber( 1 )        ; reset digit flag
               
               ;ShowMemoryViewer( *gMem, 100 )
               
               first    = IsNumber()
               text     = ""
               dots     = 0
               
               While memLoc(handle) < gMemSize And ( IsAlpha() Or gNextChar = "_"  Or gNextChar = "." Or gNextChar = "$" )
                  If gNextChar = "." : dots + 1 : EndIf
                  text + gNextChar
                  
                  If gNextChar = "$"
                     ChangePos( 1 )
                     Break
                  EndIf
                  
                  NextCharacter()
               Wend
            
               If memLoc(handle) >= gMemSize
                  SetError( "EOL in number or variable '" + text + "'", 8 )
               EndIf
               
               If Len( text ) < 1
                  SetError( "Unknown sequence number or variable '" + text + "'", 9 )
               EndIf
               
                If text = "$"
                  SetError( "Invalid string variable name '" + text + "'", 11 )
               EndIf
               
               ChangePos( -1 )
               i = 0
               temp = LCase( text )
               
               ForEach llSymbols()
                  i + 1
                  
                  If llSymbols()\name = temp
                     ;Debug "Found symbol: " + temp
                     AddToken( llSymbols()\TokenType, "" )
                     i = -1
                     gLastOp = 0
                     
                     If llTokenList()\TokenType = #ljDeclare
                        r = Right( temp, 2 )
                        If Right( temp, 1 ) = "$" Or r = ".s"
                           type = #ljSTRING
                        ElseIf r = ".d" Or r = ".f"
                           type = #ljFLOAT
                        Else
                           type = #ljINTEGER
                        EndIf
                     EndIf
                     
                     Break
                  EndIf
               Next

               If i > 0
                  If PrevToken = #ljDeclare
                     AddFunction( temp, type, #ljCALL )
                     AddToken( #ljFunction, temp )
                     i = PeekNumber()
                     
                     If i < 0
                        SetError( "Invalid parameter for declared function", 17 )
                     EndIf

                     mapFunctions()\params = i
                  ElseIf FindMapElement( mapFunctions(), temp )
                     ;If LastIdent = ""
                     ;   LastIdent = CreateVar( mapFunctions()\retType )
                     ;EndIf
                     
                     FindVariable( LastIdent , mapModules()\function )
                     
                     If gFound > -1
                        AddToken( mapFunctions()\TokenId, "#" + temp )
                        SetAssign()
                        
                        gLastOp = 0
                        LastIdent = ""
                        
                        If BraceLevel = 0 And PrevToken = #ljFunction  ; Function start
                           AddModule( temp )
                        EndIf
                     Else
                        SetError( "variable undefined '" + LastIdent + "'", 12 )
                     EndIf
                  Else
                     If first
                        If dots > 1
                           SetError( "Invalid float found '" + text + "'", 10 )
                        ElseIf dots
                           AddToken( #ljFLOAT, text )
                           llTokenList()\VarType = #ljFLOAT
                           AddNewVariable( text, llTokenList(), 1 )
                           SetAssign()
                        Else
                           AddToken( #ljINTEGER, text )
                           llTokenList()\VarType = #ljINTEGER
                           AddNewVariable( text, llTokenList(), 1 )
                           SetAssign()
                           
                           If PrevToken = #ljLeftParent And mapVariables()\ArSize
                              ;Debug "<---- Int dimension for Array ---->"
                              llTokenList()\TokenType = #ljINTEGER
                           EndIf
                        EndIf
                     Else
                        ;Debug "temp=" + temp
                        
                        If Len( temp ) > 2
                           r = Right( temp, 2 )
                           
                           If Left( r, 1 ) = "."
                              temp = Left( temp, Len( text ) - 2 )
                           EndIf
                        Else
                           r = ""
                        EndIf
                        
                        AddToken( #ljIDENT, temp )
                        LastIdent = temp 
                        
                        If Right( text, 1 ) = "$" Or r = ".s"
                           llTokenList()\VarType = #ljSTRING
                        ElseIf r = ".d" Or r = ".f"
                           llTokenList()\VarType = #ljFLOAT
                        Else
                           llTokenList()\VarType = #ljINTEGER
                        EndIf

                        FindVariable( temp, mapModules()\function )
                        
                        If gFound = -1
                           AddNewVariable( temp, llTokenList(), 0 )
                        EndIf
                        
                        If PrevToken = #ljDim
                           i = PeekNumber()
                           
                           If i <= 0
                              SetError( "Invalid Dim parameter", 13 )
                           EndIf
                           
                           mapVariables()\ArSize = i
                        EndIf
                        
                        gLastOp = 0
                     EndIf
                  EndIf
               EndIf
         EndSelect
         
         PrevToken = llTokenList()\TokenType
      Until memEOF(handle) = #True
      
      ProcedureReturn 0
   
   EndProcedure
   
   Procedure            Expect( function.s, TokenType )

      ;Debug "Expect> " + llTokenList()\Value + " --> " + gszATR( TokenType )
      
      If llTokenList()\TokenType = TokenType
         NextToken()
         ProcedureReturn 0
      EndIf
      
      SetError( "Expecting " + gszATR( TokenType ) + " but found " + gszATR( llTokenList()\TokenType ) + " for " + function, 11 )
   
   EndProcedure
   
   Procedure            MakeNode( NodeType, *left.stTree, *right.stTree, ModID, IdentType.i = 0 )
      Protected         *p.stTree = AllocateStructure( stTree )
      
      ;Debug #PB_Compiler_Procedure + " --> " + gszATR( NodeType )
      
      *p\NodeType    = NodeType
      *p\left        = *left
      *p\right       = *right
      *p\IdentType   = IdentType
      *p\ModID       = ModID
      AddNode()
      
      ProcedureReturn *p
   EndProcedure
   
   Procedure            MakeNodeMore( NodeType, *left.stTree, *right.stTree, *middle.stTree, modid,IdentType.i = 0 )
      Protected         *p.stTree = AllocateStructure( stTree )
      
      ;Debug #PB_Compiler_Procedure  + " --> " + gszATR( NodeType )
       
      *p\NodeType    = NodeType
      *p\left        = *left
      *p\right       = *right
      *p\middle      = *middle
      *p\IdentType   = IdentType
      *p\ModID       = modid
      AddNode()
      
      ProcedureReturn *p
   EndProcedure
      
   Procedure            PeekAhead( qty.i = 1 )
      Protected         type, i, P
      
      PushListPosition( llTokenList() )
      
      For i = 1 To qty
         P = NextElement( llTokenList() )
         If Not p
            ProcedureReturn -1
         EndIf
      Next
      
      type = llTokenList()\TokenType
      PopListPosition( llTokenList() )
   
      ProcedureReturn type
   EndProcedure
   
   Procedure            Makeleaf( NodeType, value.s, ModID, IdentType.i = 0, VarName.s = "" )
      Protected         *p.stTree = AllocateStructure( stTree )

      ;Debug #PB_Compiler_Procedure  + " --> " + gszATR( NodeType )

      *p\NodeType    = NodeType
      *p\value       = value
      *p\IdentType   = IdentType
      *p\VarName     = VarName
      *p\ModID       = ModID
      AddNode()
      
      ProcedureReturn *p
   EndProcedure
   
   Procedure            MakeleafMore( NodeType, value.s, *middle.stType, IdentType.i = 0, VarName.s = "" )
      Protected         *p.stTree = AllocateStructure( stTree )

      ;Debug #PB_Compiler_Procedure  + " --> " + gszATR( NodeType )

      *p\NodeType    = NodeType
      *p\value       = value
      *p\IdentType   = IdentType
      *p\VarName     = VarName
      *p\middle      = *middle
      AddNode()
      
      ProcedureReturn *p
   EndProcedure

   Procedure            expr( var, bExpression = 0, Type = 0 )
      Protected.stTree  *x, *e, *r
      Protected         op, q, flag
      Protected         i, j
      Protected.s       value, temp
      Protected Dim     *p(64)
      
      ;Logging( 0, "expr> " + llTokenList()\name )
      ;Debug "expr> " + gszATR( llTokenList()\TokenType) + " " + llTokenList()\Value
      
      Select llTokenList()\TokenType
      
         Case #ljLeftParent
            *x = paren_expr()

         Case #ljSUBTRACT, #ljADD
            op = llTokenList()\TokenType
            NextToken()
            *e = expr( gPreTable( #ljNEGATE )\Precedence, bExpression )
            
            If op = #ljSUBTRACT
               *x = MakeNode( #ljNEGATE, *e, 0, llTokenList()\ModID )
            Else
               *x = *e
            EndIf
         
         cg_expr_X_2( "StrRep", #ljStrRep )
         cg_expr_X_2( "SplitString", #ljSplitString )
         cg_expr_X_2( "LSet", #ljLSET )
         cg_expr_X_2( "RSet", #ljRSET )
         cg_expr_X_2( "CountString", #ljCountString )
         cg_expr_X_1( #ljTypeOf )
         cg_expr_X_1( #ljINTFUNC )
         cg_expr_X_1( #ljLEN )
         cg_expr_X_1( #ljSizeOf )
         cg_expr_X_1( #ljReverseString )
         Case #ljCALL
            temp = Mid( llTokenList()\Value, 2 )
            NextToken()
            Expect( "Call", #ljLeftParent )
            FindMapElement( mapFunctions(), temp )
            i = 0
            
            While i < mapFunctions()\params
               *p(i) = expr( 0, bExpression )
               
               If llTokenList()\TokenType = #ljComma
                  NextToken()
               EndIf
               
               i + 1
            Wend
            
            ;we reverse order of parameters
            For j = i - 1 To 0 Step -1
               *x = MakeNode( #ljSEQ, *x, *p(j), llTokenList()\ModID )
            Next
            
            *x = MakeNode( #ljCALL, Makeleaf( #ljINTERNAL, temp, llTokenList()\ModID ), *x, llTokenList()\ModID )
            Expect( "Call", #ljRightParent )   
   
         Case #ljCapilize
            NextToken()
            expect( "Capitlize", #ljLeftParent )
            *r = expr( 0, bExpression )
            
            If llTokenList()\TokenType = #ljComma
               NextToken()
               *e = expr( 0, bExpression )
            Else
               *e = Makeleaf( #ljINTEGER, "0", llTokenList()\ModID )
            EndIf
            
            expect( "Capitlize", #ljRightParent )
            *x = MakeNode( #ljCapilize, *r, *e, llTokenList()\ModID )
            
         Case #ljFindStr
            NextToken()
            expect( "FindStr", #ljLeftParent )
            *r = expr( 0, bExpression )
            expect( "FindStr", #ljComma )
            *e = expr( 0, bExpression )
            *x = MakeNode( #ljSEQ, *r, *e, llTokenList()\ModID )

            If llTokenList()\TokenType = #ljComma
               NextToken()
               *e = expr( 0, bExpression )
               
               If llTokenList()\TokenType = #ljComma
                  NextToken()
                  *r = expr( 0, bExpression )
               Else
                  *r = Makeleaf( #ljINTEGER, Str( #PB_String_CaseSensitive ), llTokenList()\ModID )
               EndIf
            Else
               *e = Makeleaf( #ljINTEGER, "1", llTokenList()\ModID )
               *r = Makeleaf( #ljINTEGER, Str( #PB_String_CaseSensitive ), llTokenList()\ModID )
            EndIf
            
            expect( "FindStr", #ljRightParent )
            *x = MakeNode( #ljSEQ, *x, *e, llTokenList()\ModID )
            *x = MakeNode( #ljFindStr, *x, *r, llTokenList()\ModID )
         
         Case #ljRepString
            NextToken()
            expect( "RepString", #ljLeftParent )
            *r = expr( 0, bExpression )
            expect( "RepString", #ljComma )
            *e = expr( 0, bExpression )
            *x = MakeNode( #ljSEQ, *r, *e, llTokenList()\ModID )
            expect( "RepString", #ljComma )
            *e = expr( 0, bExpression )
            *x = MakeNode( #ljSEQ, *x, *e, llTokenList()\ModID )

            If llTokenList()\TokenType = #ljComma
               NextToken()
               *e = expr( 0, bExpression )
               
               If llTokenList()\TokenType = #ljComma
                  NextToken()
                  *r = expr( 0, bExpression )
               Else
                  *r = Makeleaf( #ljINTEGER, "1", llTokenList()\ModID )
               EndIf
            Else
               *e = Makeleaf( #ljINTEGER, Str( #PB_String_CaseSensitive ), llTokenList()\ModID )
               *r = Makeleaf( #ljINTEGER, "1", llTokenList()\ModID )
            EndIf
            
            expect( "RepString", #ljRightParent )
            *x = MakeNode( #ljSEQ, *x, *e, llTokenList()\ModID )
            *x = MakeNode( #ljRepString, *x, *r, llTokenList()\ModID )

         Case #ljDate
            NextToken()
            expect( "Date", #ljLeftParent )
            *r = Makeleaf( #ljNOOP, "", llTokenList()\ModID )
            
            If llTokenList()\TokenType <> #ljRightParent
               Repeat
                  *e = MakeNode( #ljSEQ, *e, expr(0), llTokenList()\ModID )
                  
                  If llTokenList()\TokenType <> #ljComma
                     Break
                  Else
                     NextToken()
                  EndIf
               ForEver
            EndIf

            expect( "Date", #ljRightParent )
            *x = MakeNode( #ljDate, *r, *e, llTokenList()\ModID )
            
         Case #ljTRIM
            NextToken()
            expect( "Trim", #ljLeftParent )
            *r = expr( 0, bExpression )
            
            If llTokenList()\TokenType = #ljComma
               NextToken()
               *e = expr( 0, bExpression )
            Else
               *e = Makeleaf( #ljSTRING, " ", llTokenList()\ModID )
            EndIf
            
            expect( "Trim", #ljRightParent )
            *x = MakeNode( #ljTRIM, *r, *e, llTokenList()\ModID )

         Case #ljMID
            NextToken()
            expect( "Mid", #ljLeftParent )
            *r = expr( 0, bExpression )
            expect( "Mid", #ljComma )
            *e = expr( 0, bExpression )
            *x = MakeNode( #ljSEQ, *r, *e, llTokenList()\ModID )
            
            If llTokenList()\TokenType = #ljComma
               NextToken()
               *r = expr( 0, bExpression )
            Else
               *r = Makeleaf( #ljINTEGER, "-1", llTokenList()\ModID )
            EndIf
            
            *x = MakeNode( #ljMID, *x, *r, llTokenList()\ModID )
            expect( "Mid", #ljRightParent )
            
         Case #ljRND
            NextToken()
            expect( "RND", #ljLeftParent )
            
            *r = expr( 0, bExpression, llTokenList()\ModID )
            
            If llTokenList()\TokenType = #ljComma
               NextToken()
               *e = expr( 0, bExpression )
            Else
               *e = Makeleaf( #ljINTEGER, "0", llTokenList()\ModID )
            EndIf
            
            expect( "RND", #ljRightParent )
            *x = MakeNode( #ljRND, *r, *e, llTokenList()\ModID )

         Case #ljFORMAT
            NextToken()
            expect( "Format", #ljLeftParent )
            *r = expr( 0, bExpression )
            expect( "Format", #ljComma )
            *e = expr( 0, bExpression )
            *x = MakeNode( #ljSEQ, *r, *e, llTokenList()\ModID )

            If llTokenList()\TokenType = #ljComma
               NextToken()
               *e = expr( 0, bExpression )
               
               If llTokenList()\TokenType = #ljComma
                  NextToken()
                  *r = expr( 0, bExpression )
               Else
                  *r = Makeleaf( #ljSTRING, ",", llTokenList()\ModID )
               EndIf
            Else
               *e = Makeleaf( #ljSTRING, ".", llTokenList()\ModID )
               *r = Makeleaf( #ljSTRING, ",", llTokenList()\ModID )
            EndIf
            
            expect( "Format", #ljRightParent )
            *x = MakeNode( #ljSEQ, *x, *e, llTokenList()\ModID )
            *x = MakeNode( #ljFORMAT, *x, *r, llTokenList()\ModID )

         Case #ljCONCAT

            NextToken()
            expect( "Concat", #ljLeftParent )

            *e = Makeleaf( #ljSTRING, gszNULL, llTokenList()\ModID )
            *x = MakeNode( #ljSEQ, *x, *e, llTokenList()\ModID )
            
            Repeat
               CommaStream( #ljADD )
               
               If llTokenList()\TokenType = #ljLeftParent
                  *e\middle = paren_expr( bExpression )
               EndIf
              
              *x = MakeNode( #ljSEQ, *x, *e, llTokenList()\ModID )
               
               If llTokenList()\TokenType <> #ljComma
                  Break
               EndIf
               
               expect( "Concat", #ljComma )
               
            Until llTokenList()\TokenType = #ljEOF

            Expect( "Concat", #ljRightParent )
            
         Case  #ljNOT
            NextToken()
            
            If bExpression
               *x = MakeNode( #ljBNOT, expr( gPreTable( #ljNOT )\Precedence ), 0, llTokenList()\ModID )
            Else
               *x = MakeNode( #ljNOT, expr( gPreTable( #ljNOT )\Precedence ), 0, llTokenList()\ModID )
            EndIf

         Case #ljIDENT
            ; In case element is an array, type must be implicitly defined
            *x = Makeleaf( #ljIDENT, llTokenList()\value, llTokenList()\ModID, llTokenList()\VarType )
            NextToken()

            If llTokenList()\TokenType = #ljLeftParent
              *x\middle = paren_expr( bExpression )
            EndIf

         Case #ljINTEGER
            *x = Makeleaf( #ljINTEGER, llTokenList()\value, llTokenList()\ModID, type )
            NextToken()

         Case #ljFLOAT
            *x = Makeleaf( #ljFLOAT, llTokenList()\value, llTokenList()\ModID, type )
            NextToken()

         Case #ljSTRING
            
            If llTokenList()\p
               *x = Makeleaf( #ljSTRING, llTokenList()\value, llTokenList()\ModID, type, llTokenList()\p\Value )
            Else
               *x = Makeleaf( #ljSTRING, llTokenList()\value, llTokenList()\ModID, type )
            EndIf
            NextToken()

         Default
            SetError( "Expecting a primary, found " + gszATR( llTokenList()\TokenType ), 12 )
      
      EndSelect
      
      While gPreTable( llTokenList()\TokenType )\bBinary And gPreTable( llTokenList()\TokenType )\Precedence >= var
         op = llTokenList()\TokenType
         NextToken()
      
         q = gPreTable( op )\Precedence
         
         If Not gPreTable( op )\bRightAssociation
            q + 1
         EndIf
      
         *e = expr( q )
         *x = MakeNode( gPreTable( op )\NodeType, *x, *e, llTokenList()\ModID )
      Wend
      
      ProcedureReturn *x

   EndProcedure
   
   Procedure            paren_expr( bExpression = 0 )
      Protected         *p.stTree

      Expect( "paren_expr", #ljLeftParent )
      *p = expr( 0, bExpression )
      Expect( "paren_expr", #ljRightParent )
      ProcedureReturn *p
      
   EndProcedure

   Procedure            stmt( bExpression = 0 )
      Protected.stTree  *p, *v, *s, *s2
      Protected.stTree  *e, *r
      Protected.stToken *t
      Protected         type, bDim, op, i
      Protected.s       temp

      Debug "stmt> " + gszATR( llTokenList()\TokenType )

      gStack + 1
      
      If gStack > #MAX_COMPDEPTH
         NextToken()
         SetError( "Stack overflow", 15 )
      EndIf

      Select llTokenList()\TokenType

         Case #ljDeclare
            NextToken()
            Expect( "Declare", #ljFunction )
            Expect( "Declare", #ljSemi )
         
         Case #ljReturn
            If mapModules()\function
               NextToken()
               
               If llTokenList()\TokenType = #ljSemi              
                  NextToken()
                  *p = Makeleaf( #ljReturn, Str( 0 ), mapModules()\function )
               Else
                  *p = MakeNode( #ljReturn, 0, expr(0), mapModules()\function )
               EndIf
            Else
               SetError( "Not permitted in global section", 18 )
            EndIf
         
         Case #ljFunction
            NextToken()

            If Not FindMapElement( mapModules(), Mid( llTokenList()\Value, 2 ) )
               SetError( "Function has not been defined", 24 )
            EndIf
            
            Expect( "Function", #ljCALL )
            Expect( "Function", #ljLeftParent )
            ;*p = MakeNode( #ljNOOP, 0, 0, llTokenList()\ModID )
            
            If mapModules()\params
               i = 0
            
               While i < mapModules()\params
                  *r = expr( 0, bExpression )
                  *e = MakeNode( #ljASSIGN, *r, 0, llTokenList()\ModID )
                  *p = MakeNode( #ljSEQ, *p, *e, llTokenList()\ModID )
                  
                  If llTokenList()\TokenType <> #ljComma
                     Break
                  Else
                     NextToken()
                  EndIf
                  
                  i + 1
               Wend
            Else
               *p = MakeNode( #ljNOOP, 0, 0, llTokenList()\ModID )
            EndIf
            Expect( "Function", #ljRightParent )
            
            ; What to do next?
            *p = MakeNode( #ljSEQ, *p, stmt(), llTokenList()\ModID )
            *p = MakeNode( #ljSEQ, *p, Makeleaf( #ljReturn, Str( 0 ), llTokenList()\ModID ), llTokenList()\ModID )
            mapModules()\code = *p
            FindMapElement( mapModules(), gszGlobal )
            *p = 0

         Case #ljIF
            NextToken()
            *e    = paren_expr( #ljIF )
            *s    = stmt( #ljIF )
            *s2   = 0 
            
            If llTokenList()\TokenType = #ljElse
               NextToken()
               *s2 = stmt( #ljIF )
            EndIf
            
            *p = MakeNode( #ljIF, *e, MakeNode( #ljIF, *s, *s2, llTokenList()\ModID ), llTokenList()\ModID )
         
         Case #ljPRTC
            NextToken()
            *e    = paren_expr( bExpression )
            *p    = MakeNode( #ljPRTC, *e, 0, llTokenList()\ModID )
            expect( "putc", #ljSemi )

         Case #ljPRINT, #ljPRTS
            op = llTokenList()\TokenType
            NextToken()
            expect( "print", #ljLeftParent )
            
            Repeat
               CommaStream( #ljPRTS )
               
               ;Is it an array?
               If llTokenList()\TokenType = #ljLeftParent
                  Debug "Array --> " + gszATR( *r\NodeType )
                  *r\middle = paren_expr( bExpression )
               EndIf
               
               *p = MakeNode( #ljSEQ, *p, *e, llTokenList()\ModID )
               
               If llTokenList()\TokenType <> #ljComma
                  Break
               EndIf
               
               expect( "print", #ljComma )
               
            Until llTokenList()\TokenType = #ljEOF
            
            ; Automatically add NewLine when using print
            If op = #ljPRINT
               *r = Makeleaf( #ljSTRING, gszNL, llTokenList()\ModID )
               *e = MakeNode( #ljPRTS, *r, 0, llTokenList()\ModID )
               *p = MakeNode( #ljSEQ, *p, *e, llTokenList()\ModID )
            EndIf
            
            Expect( "Print", #ljRightParent )
            Expect( "Print", #ljSemi )
            
         Case #ljSemi
            NextToken()
         
         Case #ljREDIM
            NextToken()
            Expect( "ReDim", #ljLeftParent )
            *r = expr( 0 )
            Expect( "Redim", #ljComma )
            *e = expr( 0 )
            Expect( "ReDim", #ljRightParent )
            Expect( "ReDim", #ljSemi )
            *p = MakeNode( #ljREDIM, *e, *r, llTokenList()\ModID )
            
         Case #ljIDENT, #ljDim

            If llTokenList()\TokenType = #ljDim
               NextToken()
               If llTokenList()\TokenType <> #ljIDENT
                  SetError( "Expecting variable, found " + gszATR( llTokenList()\TokenType ), 16 )
               EndIf
               
               bDim = 1
            EndIf

            Debug "Var = " + llTokenList()\value + " - " + gszATR( llTokenList()\VarType )

            FindVariable( llTokenList()\value, llTokenList()\ModID )

            If gFound > -1
               *t     = mapVariables()\p
               type   = *t\VarType

               If *t\p
                  mapVariables()\Value    = *t\p\Value
               EndIf
            Else   
               Debug "!!Not found >" + llTokenList()\Value
            EndIf

            If bDim 
               NextToken()
            ElseIf type And mapVariables()\ArSize
               *v = Makeleaf( #ljIDENT, llTokenList()\value, llTokenList()\ModID, type )
               NextToken()
               
               *s2 = paren_expr( bExpression )
               Expect( "Assign", #ljASSIGN )
               *e = expr( 0, bExpression, type )
               *p = MakeNodeMore( #ljASSIGN, *v, *e, *s2, llTokenList()\ModID, type )
            Else
               *v = Makeleaf( #ljIDENT, llTokenList()\value, llTokenList()\ModID, type )
               NextToken()
               Expect( "Assign", #ljASSIGN )
               
               *e = expr( 0, bExpression )
               *p = MakeNode( #ljASSIGN, *v, *e, llTokenList()\ModID )
            EndIf
            
            Expect( "Assign", #ljSemi )
            
         Case #ljWHILE
            NextToken()
            *e = paren_expr( #ljIF )
            *s = stmt( #ljIF )
            *p = MakeNode( #ljWHILE, *e, *s, llTokenList()\ModID )
            
         Case #ljLeftCurly

            Expect( "Left Curly", #ljLeftCurly )
            
            While llTokenList()\TokenType <> #ljRightCurly And llTokenList()\TokenType <> #ljEOF
               *p = MakeNode( #ljSEQ, *p, stmt( bExpression ), llTokenList()\ModID )
            Wend
            
            Expect( "Left Curly", #ljRightCurly )

         Case #ljEOF
            gExit + 1
            
         Default
            SetError( "Expecting beginning of a statament, found " + gszATR( llTokenList()\TokenType ), 14 )
         
      EndSelect

      ProcedureReturn *p
      
   EndProcedure
   
   Procedure            DisplayNode( *p.stTree, gadget = -1 )
      If *p
         If *p\NodeType = #ljIDENT Or *p\NodeType = #ljINTEGER Or *p\NodeType = #ljFLOAT
            Logging( gadget, LSet( gszATR( *p\NodeType ), 30 ) + *p\value )
         Else
            Logging( gadget, LSet( gszATR( *p\NodeType ), 30 ) )
            DisplayNode( *p\left, gadget )
            DisplayNode( *p\middle, gadget )
            DisplayNode( *p\right, gadget )
         EndIf
      Else
         Logging( gadget,  ";" )
      EndIf
   EndProcedure
   
   Procedure            EmitInt( op.i, flag, pos, ndx = 0 )
      AddElement( llObjects() )
      
      With llObjects()
         \code    = op
         \flag    = flag
         \i       = pos
         \ndx     = ndx
      EndWith

   EndProcedure
   
     Procedure            GetConstantIndex( text.s )
   
      If FindMapElement( mapVariables(), text )
         ProcedureReturn mapVariables()\Index
      EndIf
      
      AddNewVariable( text, llTokenList(), 1 )
      ProcedureReturn gVarPos - 1
   
   EndProcedure
    
   Procedure      hole()
      gHoles + 1

      AddElement( llHoles() )
      llHoles()\location   = llObjects()
      llHoles()\function   = 0
      llHoles()\id         = gHoles
      
      ProcedureReturn gHoles
   EndProcedure
   
   Procedure            fix( id, dst = -1 )
      
      AddElement( llHoles() )
      
      If dst = -1
         llHoles()\function = 1
         llHoles()\id = id
         llHoles()\location = llObjects()
      Else                                   ; Used by blind JMP
         llHoles()\function = 3
         ;llHoles()\location = AddElement( llObjects() )
         llHoles()\location = LastElement( llObjects() )
         llHoles()\src = dst
      EndIf

   EndProcedure
   
   Procedure            CodeFinish()
      LastElement( llObjects() )
      EmitInt( #ljHALT, 0, 0 )
   EndProcedure
   
   ;----------------------------------------------------------------------------------------
   ;- 3 Code generator
   ;- Finally, source is converted to assembly mnemonics
   ;----------------------------------------------------------------------------------------
   Procedure            CodeGenerator( *x.stTree, bUsePush = 0 )
      Protected         p1, p2, n
      Protected         *t.stToken
      Protected         temp.s
      Protected         d.d, i.i
      Protected         flag, cmd
      Protected         *p.stType
      Static.s          param
      
      If Not *x
         ProcedureReturn
      EndIf
      
      Select *x\NodeType
         Case #ljIDENT
            FindVariable( *x\value, *x\ModID )
            cg_SetFlagOnFoundVar()

            If mapVariables()\ArSize
               flag = flag | #LJMOD_ARRAY
               PushMapPosition( mapVariables() )
               CodeGenerator( *x\middle )
               PopMapPosition( mapVariables() )
               
               If llObjects()\code = #ljFetch
                  i = llObjects()\i
                  llObjects()\ndx   = i
                  llObjects()\i     = n
                  llObjects()\flag  = flag | #LJMOD_ARRAY
               ElseIf llObjects()\code = #ljMOV
                  i     = llObjects()\ndx
                  EmitInt( #ljFetch, flag, n, i )
               Else
                  flag = flag | #LJMOD_STACK
                  EmitInt( #ljFetch, flag, n )
               EndIf
            Else
               EmitInt( #ljFetch, flag, n )
            EndIf

         Case #ljINTEGER, #ljFLOAT, #ljSTRING
            
            If Not bUsePush
               cmd = #ljFetch
            Else
               cmd = #ljPush
            EndIf
            
            FindVariable( *x\value, *x\ModID )
            
            If gFound < 0
               AddMapElement( mapVariables(), *x\value )
                  mapVariables()\Index    = gVarPos
                  mapVariables()\ModID    = mapModules()\function                  
                  mapVariables()\VarType  = *x\NodeType
                  mapVariables()\value    = *x\value
                  mapVariables()\bConstant= 1
                  n = gVarPos
                  *pVars(n) = mapVariables()
                  gVarPos + 1
            Else
               n = gFound
            EndIf
            
            If *x\NodeType = #ljINTEGER
               EmitInt( cmd, #LJMOD_CONST + #LJMOD_INT, n )
            ElseIf *x\NodeType = #ljFLOAT
               EmitInt( cmd, #LJMOD_CONST + #LJMOD_FLOAT, n )
            Else
               EmitInt( cmd, #LJMOD_CONST + #LJMOD_STRING, n )
            EndIf
            
         Case #ljINTERNAL     ; Parameters used internally
            param = *x\value
         
         Case #ljASSIGN
            FindVariable( *x\left\value, *x\left\ModID )
            cg_SetFlagOnFoundVar()
            
            PushMapPosition( mapVariables() )
            CodeGenerator( *x\right )
            PopMapPosition( mapVariables() )

            If mapVariables()\ArSize
               flag = flag | #LJMOD_ARRAY
               
               PushMapPosition( mapVariables() )
               CodeGenerator( *x\middle, #True )
               PopMapPosition( mapVariables() )
               
               If llObjects()\code = #ljFetch
                  i = llObjects()\i
                  llObjects()\ndx   = i
                  llObjects()\i     = n
                  llObjects()\flag  = flag
                  llObjects()\code  = #ljStore
               ElseIf llObjects()\code = #ljMOV
                  i     = llObjects()\ndx
                  EmitInt( #ljStore, flag, n, i )
               Else
                  flag = flag | #LJMOD_STACK
                  EmitInt( #ljStore, flag, n )
               EndIf
            Else
               If llObjects()\code = #ljFetch And Not llObjects()\flag & #LJMOD_ARRAY
                  llObjects()\code = #ljMOV
                  llObjects()\ndx = n
               Else
                  EmitInt( #ljStore, flag, n )
               EndIf
            EndIf

         Case #ljIF
            CodeGenerator( *x\left )
            EmitInt( #ljJZ, 0, 0 )
            p1 = hole()
            CodeGenerator( *x\right\left )
            
            If *x\right\right 
               EmitInt( #ljJMP, 0, 0 )
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
            EmitInt( #ljJZ, 0, 0 )
            p2 = Hole()
            CodeGenerator( *x\right )
            EmitInt( #ljJMP, 0, 0 )
            fix( gHoles, p1 )
            fix( p2 )
            
         Case #ljCALL
            EmitInt( #ljPUSHSP, 0, 0 )
            CodeGenerator( *x\right )
            CodeGenerator( *x\left )
            EmitInt( #ljCALL, 0, 0 )

            PushMapPosition( mapModules() )
            FindMapElement( mapModules(), param )
            
            AddElement( llFuncPos() )
            llFuncPos()\function = mapModules()\function
            llFuncPos()\location = llObjects()
            PopMapPosition( mapModules() )
         
         Case #ljReturn
            CodeGenerator( *x\right )
            EmitInt( *x\NodeType, 0, 0 )
            EmitInt( Val( *x\value ), 0, 0 )
         
         Case #ljNOOP
            EmitInt( #ljNOOP, 0, 0 )
         
         Case #ljSEQ 
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )

         Case #ljREDIM, #ljTypeOf, #ljSizeOf       ; Special case as these functions (can) take an array as a parameter
                                                   ; we will "swallow" the first parameter and inject it in command
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            llObjects()\code = *x\NodeType
            ;EmitInt( *x\NodeType, 0, llObjects()\i )

         Case #ljNEGATE, #ljNOT, #ljBNOT, #ljLEN
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType, 0, 0 )
         
         Case #ljLESS, #ljGREATER, #ljLESSEQUAL, #ljGreaterEqual, #ljNotEqual, #ljAdd, #ljOr, #ljSUBTRACT, #ljAND, #ljDIVIDE, #ljMULTIPLY, #ljMOD, #ljXOR, #ljEQUAL
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            EmitInt( *x\NodeType, 0, 0 )

         Case #ljCONCAT To #ljNoMore
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            EmitInt( *x\NodeType, 0, 0 )

         Default
            SetError( "Error in code generator in node " + gszATR( *x\NodeType ), 21 )

      EndSelect
     
   EndProcedure

   Procedure.s          MakeASMLine( *op.stType, Array *pData.stGlobals(1), Array Symbols.s(1), bShowDebug = 0 )
      Protected.s       line
      Protected.s       symb

      Select *op\code
         Case #ljFetch, #ljStore, #ljPush, #ljTypeOf, #ljSizeOf, #ljREDIM, #ljSplitString
            cg_DebugVariableName(*op\i)
            line = LSet( gszATR( *op\code ), 12 ) + "[" + symb + "]"
            
            If *op\flag & #LJMOD_ARRAY
               If *op\flag & #LJMOD_STACK
                  line + "(sp)"
               Else
                  line + "(" + Str(*op\ndx) + ")"
               EndIf
            EndIf
            
            If bShowDebug And *op\code = #ljFetch
               If *op\flag & #LJMOD_CONST
                  If *pData(*op\i)\ss(0) > ""
                     line + "<" + *pData(*op\i)\ss(0) + ">"
                  Else
                     line + "<NULL>"
                  EndIf
               Else
                  If *op\flag & #LJMOD_FLOAT
                     line + "{" + StrD(*pData(*op\i)\d(0), 3) + "}"
                  ElseIf *op\flag & #LJMOD_INT
                     line + "{" + Str(*pData(*op\i)\i(0)) + "}"
                  ElseIf *op\flag & #LJMOD_STRING
                     line + "{" + *pData(*op\i)\d(0) + "}"
                  EndIf
               EndIf
            EndIf

         Case #ljReturn, #ljCALL, #ljHOLE
            line = LSet( gszATR( *op\code ), 12 ) + Str( *op\i )
         
         Case #ljJMP, #ljJZ
            line = LSet( gszATR( *op\code ), 12 ) + "(" + Str( *op\i  ) + ")"
         
         Case #ljMOV
            cg_DebugVariableName(*op\i)
            line = LSet( gszATR( *op\code ), 12 ) + "[" + symb + "]"
            
            If *op\flag & #LJMOD_CONST And bShowDebug
               If *pData(*op\i)\ss(0) > ""
                  line + "<" + *pData(*op\i)\ss(0) + ">"
               Else
                  line + "<NULL>"
               EndIf
            EndIf
            
            cg_DebugVariableName(*op\ndx)
            line + "->[" + symb + "]" 
            
         Case #ljPUSHSP, #ljNOOP To #ljNoMore
            line = LSet( gszATR( *op\code ), 12 )
         
      EndSelect

      ProcedureReturn line
   EndProcedure
 
   Procedure            ListCode( gadget )
      Protected.s       line, FullCode
      Protected Dim     *ar.stGlobals(1)
      Protected Dim     sz.s(1)
      
      Logging( gadget, ";-- Data Section --" )
      Logging( gadget, ";   Constants" )
      
      ForEach mapVariables()
         If mapVariables()\bConstant
            line = Str( mapVariables()\Index) + " --> " + gszInv + mapVariables()\Value + gszInv
            Logging( gadget, line )
         EndIf
      Next

      Logging( gadget, ";   Variables" )
      
      ForEach mapVariables()
         If Not mapVariables()\bConstant
            If mapVariables()\ArSize
               line = MapKey( mapVariables() ) + " (Array)"
            Else
               line = MapKey( mapVariables() )
            EndIf
            
            Logging( gadget, line )
         EndIf
      Next
      
      Logging( gadget, ";-- Code Section --" )
  
      ForEach llObjects()
         line = LSet( Str( ListIndex(llObjects()) ), 6 )
      
         If llObjects()\code = #ljJMP Or llObjects()\code = #ljJZ 
            line + LSet( gszATR( llObjects()\code ), 12 ) + "(" + Str( llObjects()\i ) + ") " + Str( llObjects()\i + ListIndex( llObjects() ) )
         Else
            line + MakeASMLine( llObjects(), *ar(), sz() )
         EndIf
      
         Logging( gadget,  Line )
         FullCode + Line +  #CRLF$
      Next

      SetClipboardText( FullCode )

   EndProcedure
   
   Procedure            FindFuncById( funcid )
      Protected         i
   
      ForEach mapModules()
         If mapModules()\function = funcid
            ChangeCurrentElement( llObjects(), mapModules()\NewPos )
            ProcedureReturn ListIndex( llObjects() )
            ;If llObjects()\i <> #ljNOOP Or llObjects()\i <> #ljStore
               ;Debug "Adjustment needed"
            ;   Repeat
            ;      NextElement( llObjects() )
            ;      i + 1
            ;   Until llObjects()\i = #ljNOOP Or i = 3
            ;EndIf
         EndIf
      Next
   
      ProcedureReturn -1
   EndProcedure
 
   Procedure            SyntaxCheck( gadget = -1 )
      Protected         temp.s, total
   
      Debug "#################[ Parser ]#################"

      If Scanner()
         ProcedureReturn gLastError
      EndIf
   
      If gadget >= 0 And gFlags( #ljListParse )
         DebugParser( gadget )
      EndIf
      
      Debug "#################[ Functions ]#################"
      ForEach mapModules()
         Debug  MapKey( mapModules() ) + " -- " + gszFuncNames( mapModules()\function )
      Next
      Debug "#################[ Syntax Analysis ]#################"

      FirstElement( llTokenList() )
      DeleteElement( llTokenList(), 1 )
      total = ListSize( llTokenList() )
      FindMapElement( mapModules(), gszGlobal )
   
      Repeat
         gStack = 0
         mapModules()\code = MakeNode( #ljSEQ, mapModules()\code, stmt(), mapModules()\function )
         
         If gLastError
            Break 
         EndIf
         
      Until ListIndex( llTokenList() ) > total Or gLastError Or llTokenList()\TokenType = #ljEOF
   
      If gFlags( #ljListSyntax ) And gadget >= 0
         Logging( gadget, "---[ Syntax Analysis ]--------------" )
         ForEach mapModules()
            Logging( gadget, ";- " + gszFuncNames( mapModules()\function ) )
            DisplayNode( mapModules()\code, gadget )
         Next
         Logging( gadget, "---[ End ]--------------" )
      EndIf
   
      ProcedureReturn gLastError
   EndProcedure
   
   ; Certaon code requires fixing post generation, due to idiosyncorsies of certain functions
   Procedure            FixCode()
      Protected         n
   
      ForEach llObjects()
         If llObjects()\code = #ljSplitString
            NextElement( llObjects() )
            If llObjects()\code <> #ljStore
               DeleteElement( llObjects() )
               NextElement( llObjects() )
            EndIf
            
            n = llObjects()\i
            DeleteElement( llObjects() )
            llObjects()\i = n
         EndIf
      Next
   EndProcedure
   
   ;----------------------------------------------------
   ;- 4 Compiler
   ;- This function does the magic
   ;----------------------------------------------------
   Procedure            GenerateCode( gadget = -1 )
      Protected         err
      Protected         total, pos, pair, i, op
      Protected.s       temp, line
      Protected         *p1, *p2

      If SyntaxCheck( gadget )
         ProcedureReturn gLastError
      EndIf
      
      DebugListVars()
      
      ForEach mapModules()
         If mapModules()\function > 0
            mapModules()\NewPos = llObjects()
         EndIf
         
         CodeGenerator( mapModules()\code )
         
         If mapModules()\function = 0
            CodeFinish()
         EndIf
      Next
      
      ;DebugListCode()
      FixCode()
      Debug "Code size: " + Str( ListSize( llObjects() ) )
      
      total = ListSize( llHoles() )
      
      ; We need to resolve all WHILEs and IFs
      ForEach llHoles()
         If llHoles()\function = 1
            PushListPosition( llHoles() )
               llHoles()\function = 2
               pair  = llHoles()\id
               ChangeCurrentElement( llObjects(), llHoles()\location )
               pos   = ListIndex( llObjects() )
               i     = 0
               
               ForEach llHoles()
                  If llHoles()\function = 0 And llHoles()\id = pair
                     llHoles()\function = 2
                        ChangeCurrentElement( llObjects(), llHoles()\location )
                        llObjects()\i = (pos - ListIndex( llObjects() ) ) + 1
                     Break
                  EndIf
               Next
            PopListPosition( llHoles() )
         ElseIf llHoles()\function = 3
            llHoles()\function = 2
            ChangeCurrentElement( llObjects(), llHoles()\src )
            pos = ListIndex( llObjects() )
            ChangeCurrentElement( llObjects(), llHoles()\location )
            ;- To keep an eye on this
            llObjects()\i = (pos - ListIndex( llObjects() ) ) + 1
         EndIf
      Next

      If err
         Debug "Mismatched if/while/else"
         gszlastError = "Mismatched if/while/else"
         ProcedureReturn 60
      EndIf
      
      ; we need to resolve all function references in the code
      ForEach llFuncPos()
         pos = FindFuncById( llFuncPos()\function )
         ChangeCurrentElement( llObjects(), llFuncPos()\location )
         llObjects()\i = pos
      Next
      ;done
      
      If gadget >= 0 And gFlags( #ljListASM )
         Logging( gadget, "--Code--" )
         ListCode( gadget )
         Logging( gadget, "--END--" )
      EndIf
      
      ProcedureReturn gLastError
   
   EndProcedure

   ;- Library functions

   Procedure.s          Capitalize( sz.s, option.i = 0 )
      Protected         i, j, flag
      Protected.s       new, char
      
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

   Procedure.s          String( sz.s, size )
      Protected.s       new

      While size
         size - 1
         new + sz
      Wend

      ProcedureReturn new
   EndProcedure
   ;----------------------------------------------------
   ;- 5 Run VM
;- Virtual Machine Engine
   ;----------------------------------------------------
   Procedure            RunVM( gadget = -1 )
      Protected         t
      Protected         i, d.d
      Protected         long.l
      Protected.s       line
      Protected         sp, pc, cs     ; Stack Pointer, Code Pointer, Call Stack Pointer
      Protected         ndx
      Protected         old, noop, MaxData
      Protected.s       temp
      Protected Dim     myStack.stGlobals(#MAX_STACK + 1)
      Protected Dim     spStack.stCallStack(#MAX_CALLSTACK + 1)
      Protected Dim     *arData.stGlobals(0)
      Protected Dim     code.stType( 0 )
      Protected Dim     mydate(6)
      Protected Dim     szSymbols.s(0)
      
      ; TODO
      ;
      ; RemoveString
      ; Hex
      ;
      ; Time & Date Functions
      ; Lists (of items)
      ; Objects?
      ;
      ; qsort example
      ;
      ; Unfold ASM code 
      ;
      ;

      t = ElapsedMilliseconds()

      MaxData = MapSize( mapVariables() ) + 4
      ReDim *arData( MaxData )
      ReDim szSymbols( MaxData )
      vm_ListToArray( llObjects, code )
      
      ; Preload arData with strings and constants
      ForEach mapVariables()
         i = mapVariables()\Index
         
         If i >= MaxData
            MaxData + 32
            ReDim *arData( MaxData )
            ReDim szSymbols( MaxData )
         EndIf
         
         *arData(i)        = AllocateStructure( stGlobals )
         *arData(i)\arSize = mapVariables()\ArSize
         szSymbols(i)      = mapVariables()\shortname
         
         Select mapVariables()\VarType
            Case #ljSTRING
               *arData(i)\flag = #LJMOD_STRING
               
            Case #ljFLOAT
               *arData(i)\flag = #LJMOD_FLOAT
               
            Default
               *arData(i)\flag = #LJMOD_INT
         EndSelect
         
         If mapVariables()\ArSize
            *arData(i)\flag | #LJMOD_ARRAY
            
            If mapVariables()\VarType = #ljSTRING
               ReDim *arData( i )\ss( mapVariables()\ArSize )
            ElseIf mapVariables()\VarType = #ljFLOAT
               ReDim *arData( i )\d( mapVariables()\ArSize )
            Else
               ReDim *arData( i )\i( mapVariables()\ArSize )
            EndIf
         ElseIf mapVariables()\bConstant
            *arData(i)\flag   = *arData(i)\flag | #LJMOD_CONST
            *arData(i)\i(0)   = Val( mapVariables()\Value )   
            *arData(i)\d(0)   = ValD( mapVariables()\Value )
            *arData(i)\ss(0)  = mapVariables()\Value
         EndIf
      Next

      vm_ListToArray( llObjects, code )
      i = 0

      While CPC()\code <> #ljHALT
         If gFlags( #ljShowRunCode )
            line = LSet( Str( pc - 1 ), 6 )
            Line + MakeASMLine( @code(pc), *arData(), szSymbols(), 1 )
            Logging( gadget, line )
         EndIf
         
         ;- Only for testing and debugging
         Debug LSet( Str( pc ), 6 ) + LSet( Str( sp ), 4 ) + " " + MakeASMLine( @Code( pc ), *arData(), szSymbols(), 1 )
         
         Select code( pc )\code
            Case #ljNOOP
               noop = pc
               
            Case #ljMOV
               CopyStructure( *arData(CPC()\i), *arData(code(pc)\ndx), stGlobals )
         
            Case #ljFetch, #ljPush
               If Not *arData(CPC()\i)\arSize
                  vm_CheckStack()
                  CopyStructure( *arData(CPC()\i), @myStack(sp), stGlobals )
               Else
                  If CPC()\flag & #LJMOD_STACK
                     ndx = myStack(sp)\i(0)
                     sp - 1
                  Else
                     ndx = *arData(CPC()\ndx)\i(0)
                     vm_CheckStack()
                  EndIf

                  If CPC()\flag & #LJMOD_INT
                     myStack(sp)\i(0)  = *arData(CPC()\i)\i(ndx)
                     myStack(sp)\flag = CPC()\flag
                  ElseIf CPC()\flag & #LJMOD_FLOAT
                     myStack(sp)\d(0)  = *arData(CPC()\i)\d(ndx)
                     myStack(sp)\flag = CPC()\flag
                  ElseIf CPC()\flag & #LJMOD_STRING
                     myStack(sp)\ss(0) = *arData(CPC()\i)\ss(ndx)
                     myStack(sp)\flag = CPC()\flag
                  EndIf
               EndIf
               
            Case #ljStore : vm_Store()
            
            Case #ljPUSHSP
               cs + 1
               spStack(cs)\sp = sp
            
            Case #ljCALL
               spStack(cs)\pc = pc
               pc = CPC()\i

            Case #ljReturn
               sp  = spStack(cs)\sp
               pc = spStack(cs)\pc
               cs - 1
               
            Case #ljTypeOf
               sp + 1
               myStack( sp )\ss( 0 ) = "Unknown"
            
               If *arData(CPC()\i)\flag & #LJMOD_STRING
                  myStack( sp )\ss( 0 ) = "String"
               ElseIf *arData(CPC()\i)\flag & #LJMOD_FLOAT
                  myStack( sp )\ss( 0 ) = "Float"
               ElseIf *arData(CPC()\i)\flag & #LJMOD_INT
                  myStack( sp )\ss( 0 ) = "Integer"
               EndIf
            
               If *arData(CPC()\i)\flag & #LJMOD_ARRAY
                  myStack( sp )\ss( 0 ) + "Array"
               ElseIf *arData(CPC()\i)\flag & #LJMOD_CONST And code(pc-1)\code <> #ljMOV
                  myStack( sp )\ss( 0 ) + "Constant"
               EndIf
      
               myStack( sp )\flag = #LJMOD_STRING
               
            Case #ljSizeOf
               sp + 1
               myStack( sp )\flag = #ljMOD_INT
               myStack( sp )\i( 0 ) = *arData( CPC()\i )\arSize

            vm_XINT_1( #ljLEN, #ljINTEGER, Len( myStack( sp )\ss(0) ) )
            vm_XINT_1( #ljINTFUNC, #ljINTEGER, myStack( sp )\d( 0 ) )
            vm_XINT_1( #ljReverseString, #ljSTRING, ReverseString )
            
            vm_XINT_2( #ljLSET, LSet )
            vm_XINT_2( #ljRSET, RSet )
            vm_XINT_2( #ljCapilize, Capitalize )
            vm_XINT_2( #ljStrRep, String )
            
            Case #ljRepString
               vm_CONV2STR( sp - 3, 0 )
               vm_CONV2STR( sp - 2, 0 )
               vm_CONV2INT( sp - 1, 0 )
               vm_CONV2INT( sp,     0 )
               myStack( sp - 4 )\ss(0) = ReplaceString( myStack( sp - 4 )\ss(0), myStack( sp - 3 )\ss(0), myStack( sp - 2 )\ss(0), myStack( sp - 1 )\i(0), myStack( sp )\i(0) )
               sp - 4
            
            Case #ljCountString
               If myStack( sp - 1 )\flag & #LJMOD_INT
                  myStack( sp - 1 )\ss(0) = Str( myStack( sp - 1 )\i(0) )
               ElseIf myStack( sp - 1 )\flag & #LJMOD_FLOAT
                  myStack( sp - 1 )\ss(0) = StrD( myStack( sp - 1 )\d(0) )
                  myStack( sp - 1 )\flag = #LJMOD_INT
               Else
                  myStack( sp - 1 )\flag = #LJMOD_INT
               EndIf
               
               If myStack( sp )\flag & #LJMOD_INT
                  myStack( sp )\ss(0) = Str( myStack( sp )\i(0) )
               ElseIf myStack( sp )\flag = #ljMOD_FLOAT
                  myStack( sp )\ss(0) = StrD( myStack( sp )\d(0) )
               EndIf
               
               myStack( sp - 1 )\i( 0 )   = CountString( myStack( sp - 1 )\ss(0), myStack( sp )\ss(0) )
               sp - 1
            
            Case #ljTRIM
               myStack( sp - 1 )\ss( 0 ) = Trim( myStack( sp - 1 )\ss(0), myStack( sp )\ss(0) )
               sp - 1
            
            Case #ljSplitString
               ndx = CPC()\i
               If *arData(ndx)\flag & #LJMOD_STRING And *arData(ndx)\flag & #LJMOD_ARRAY
                  *arData(ndx)\arSize = 1
                  ReDim *arData(ndx)\ss(1)
                  i = 1
                  
                  Repeat
                     temp = StringField( myStack( sp - 1)\ss(0), i, myStack( sp )\ss(0) )
                     If temp = "" : Break : EndIf
                     
                     If i > 1
                        ReDim *arData(ndx)\ss(i)
                        *arData(ndx)\arSize = i
                     EndIf
                     
                     *arData(ndx)\ss(i-1) = Trim( temp )
                     i + 1
                  ForEver
                  
                  pc + 1
                  sp - 1
               Else
                  Debug "can only be used with a string array"
               EndIf
               
            Case #ljDate
               ndx = (pc - noop) - 1
               
               If ndx > 0 And ndx < 7
                  ndx - 1
                  mydate(1) = 1     ; Can't be 0
                  mydate(2) = 1
                  
                  Repeat 
                     mydate( i ) = myStack( (sp - ndx) + i )\i(0)
                     i + 1
                  Until i >= ndx
                  
                  sp - ndx
                  myStack( sp )\i(0) = Date( mydate(0), mydate(1), mydate(2), mydate(3), mydate(4), mydate(5))
               Else                             ; No parameters = current date/time
                  myStack( sp )\i(0) = Date()
                  myStack( sp )\flag = #ljMOD_INT
               EndIf 

            Case #ljMID
               If myStack( sp )\i(0) < 0
                  myStack( sp - 2 )\ss(0) = Mid( myStack( sp - 2 )\ss(0), myStack( sp - 1 )\i(0) )
               Else
                  myStack( sp - 2 )\ss(0) = Mid( myStack( sp - 2 )\ss(0), myStack( sp - 1 )\i(0), myStack( sp )\i(0) )
               EndIf
               
               sp - 2
               myStack(sp)\flag = #ljMOD_STRING

            Case #ljFORMAT
               myStack( sp - 3 )\ss( 0 ) = FormatNumber( myStack( sp - 3 )\d( 0 ), myStack( sp - 2 )\i(0), myStack( sp - 1 )\ss(0), myStack( sp )\ss(0) )
               sp - 3
               myStack(sp)\flag   = #ljMOD_STRING

            Case #ljFindStr
               myStack( sp - 3 )\i( 0 ) = FindString( myStack( sp - 3 )\ss( 0 ), myStack( sp - 2 )\ss(0), myStack( sp - 1 )\i(0), myStack( sp )\i(0) )
               sp - 3
               myStack(sp)\FLAG = #ljMOD_INT
    
            Case #ljRND
               myStack( sp - 1 )\i( 0 )= Random( myStack( sp - 1 )\i( 0 ), myStack( sp )\i( 0 ) )
               sp - 1
               myStack(sp)\FLAG  = #ljMOD_INT

            Case #ljREDIM
               ndx   = myStack(sp)\i( 0 )
               *arData( CPC()\i )\arSize = ndx
               
               If *arData( CPC()\i )\flag & #ljMOD_STRING
                  ReDim *arData( CPC()\i )\ss( ndx )
               ElseIf *arData( CPC()\i )\flag & #ljMOD_FLOAT
                  ReDim *arData( CPC()\i )\d( ndx )
               Else
                  ReDim *arData( CPC()\i )\i( ndx )
               EndIf
               sp - 1
               
            Case #ljADD
               If myStack( sp - 1 )\flag & #LJMOD_INT
                  If myStack( sp )\flag & #LJMOD_INT
                     myStack( sp - 1 )\i(0) + myStack( sp )\i(0)
                  ElseIf myStack( sp )\flag & #LJMOD_FLOAT
                     myStack( sp - 1 )\i(0) + myStack( sp )\d(0)
                  ElseIf myStack( sp )\flag & #LJMOD_STRING
                     myStack( sp - 1 )\i(0) + Val(myStack( sp )\ss(0))
                  EndIf
               ElseIf myStack( sp - 1 )\flag & #LJMOD_FLOAT
                  If myStack( sp )\flag & #LJMOD_INT
                     myStack( sp - 1 )\d(0) + myStack( sp )\i(0)
                  ElseIf myStack( sp )\flag & #LJMOD_FLOAT
                     myStack( sp - 1 )\d(0) + myStack( sp )\d(0)
                  ElseIf myStack( sp )\flag & #LJMOD_STRING
                     myStack( sp - 1 )\d(0) + ValD(myStack( sp )\ss(0))
                  EndIf
               ElseIf myStack( sp -1 )\flag & #LJMOD_STRING
                  If myStack( sp )\flag & #LJMOD_INT
                     myStack( sp - 1 )\ss(0) + Str(myStack( sp )\i(0))
                  ElseIf myStack( sp )\flag & #LJMOD_FLOAT
                     myStack( sp - 1 )\ss(0) + StrD(myStack( sp )\d(0))
                  ElseIf myStack( sp )\flag & #LJMOD_STRING
                     myStack( sp - 1 )\ss(0) + myStack( sp )\ss(0)
                  EndIf
               EndIf
            
               vm_POP()
            
            Case #ljSUBTRACT     : vm_DoMaths( - )
            Case #ljMULTIPLY     : vm_DoMaths( * )
            Case #ljGREATER      : vm_Operand( > )
            Case #ljLESS         : vm_Operand( < )
            Case #ljGreaterEqual : vm_Operand( >= )
            Case #ljLESSEQUAL    : vm_Operand( <= )
            Case #ljEQUAL        : vm_Operand( = )
            Case #ljNotEqual     : vm_Operand( <> )
            Case #ljAND          : vm_BitOperand( & )
            Case #ljOr           : vm_BitOperand( | )
            Case #ljXOR          : vm_BitOperand( ! )

            Case #ljDIVIDE
               If myStack( sp - 1 )\flag & #LJMOD_FLOAT And myStack( sp)\flag & #LJMOD_FLOAT
                  myStack( sp - 1 )\d( 0 ) / myStack( sp )\d( 0 )
               ElseIf myStack( sp - 1 )\flag & #LJMOD_INT And myStack( sp)\flag & #LJMOD_INT
                  myStack( sp - 1 )\i( 0 ) / myStack( sp )\i( 0 )
               Else
                  If myStack( sp - 1 )\flag & #LJMOD_FLOAT
                     If myStack( sp )\flag & #LJMOD_INT
                        myStack( sp - 1 )\d( 0 ) / myStack( sp )\i( 0 )
                     ElseIf myStack( sp )\flag & #LJMOD_STRING
                        myStack( sp - 1 )\d( 0 ) / ValD( myStack( sp )\ss( 0 ) )
                     EndIf
                  ElseIf myStack( sp - 1 )\flag & #LJMOD_INT
                     If myStack( sp )\flag & #LJMOD_FLOAT
                        myStack( sp - 1 )\i( 0 ) / myStack( sp )\d( 0 )
                     ElseIf myStack( sp )\flag & #LJMOD_STRING
                        myStack( sp - 1 )\i( 0 ) / Val( myStack( sp )\ss( 0 ) )
                     EndIf
                  ElseIf myStack( sp -1 )\flag & #LJMOD_STRING
                     myStack( sp - 1 )\flag ! #LJMOD_NOTYP | #LJMOD_INT | #LJMOD_FLOAT
                     myStack( sp - 1 )\d(0) = ValD(myStack( sp - 1 )\ss(0))
                     myStack( sp - 1 )\i(0) = myStack( sp - 1 )\d(0)
                     
                     If myStack( sp )\flag & #LJMOD_FLOAT
                        myStack( sp - 1 )\d( 0 ) / myStack( sp )\d( 0 )
                        myStack( sp - 1 )\i( 0 ) = myStack( sp - 1 )\d( 0 )
                     ElseIf myStack( sp )\flag & #LJMOD_INT
                        myStack( sp - 1 )\i( 0 ) / myStack( sp )\i( 0 )
                        myStack( sp - 1 )\d( 0 ) = myStack( sp - 1 )\i( 0 )
                     ElseIf myStack( sp )\flag & #LJMOD_STRING
                        myStack( sp - 1 )\d( 0 ) / ValD( myStack( sp )\ss( 0 ) )
                        myStack( sp - 1 )\i( 0 ) = myStack( sp - 1 )\d( 0 )
                     EndIf
                  EndIf
               EndIf

               vm_POP()

            Case #ljMOD
               If myStack( sp - 1 )\FLAG & #LJMOD_FLOAT
                  If myStack( sp )\flag & #LJMOD_FLOAT
                     myStack( sp - 1 )\d( 0 ) = Mod( myStack( sp - 1 )\d( 0 ), myStack( sp )\d( 0 ) )
                  ElseIf myStack( sp )\flag & #LJMOD_INT
                     myStack( sp - 1 )\d( 0 ) = Mod( myStack( sp - 1 )\d( 0 ), myStack( sp )\i( 0 ) )
                     myStack( sp - 1 )\FLAG = myStack( sp - 1 )\FLAG | #LJMOD_FLOAT
                  EndIf
               ElseIf myStack( sp - 1 )\FLAG & #LJMOD_INT
                  If myStack( sp )\flag & #LJMOD_FLOAT
                     i = myStack( sp )\d( 0 )
                     myStack( sp - 1 )\i( 0 ) % i
                  ElseIf myStack( sp )\flag & #LJMOD_INT
                     myStack( sp - 1 )\i( 0 ) % myStack( sp )\i( 0 )
                     myStack( sp - 1 )\FLAG = myStack( sp - 1 )\FLAG | #LJMOD_INT
                  EndIf
               EndIf
               
               vm_POP()

            Case #ljBNOT
               If myStack( sp )\i( 0 )
                  myStack( sp )\i( 0 ) = 0
               Else
                  myStack( sp )\i( 0 ) = 1
               EndIf
               
            Case #ljNOT
               myStack( sp )\i( 0 ) = ~myStack( sp )\i( 0 )
               
            Case #ljNEGATE
               myStack( sp )\i( 0 ) = -myStack( sp )\i( 0 )
               myStack( sp )\d( 0 ) = -myStack( sp )\d( 0 )

            Case #ljPRTS
               If myStack( sp )\flag & #LJMOD_FLOAT
                  Display( gadget, StrD( myStack( sp )\d( 0 ) ) )
               ElseIf myStack( sp )\flag & #LJMOD_STRING
                  Display( gadget, myStack( sp )\ss( 0 ) )
               ElseIf myStack( sp )\flag & #LJMOD_INT
                  Display( gadget, Str( myStack( sp )\i( 0 ) ) )
               EndIf
               vm_POP()
               
            Case #ljPRTC
               Display( gadget, Chr( myStack(sp)\i(0) ) )
               vm_POP()

            Case #ljJMP
               pc + code( pc )\i
               Continue
               
            Case #ljJZ
               
               If Not myStack(sp)\i( 0 )
                  pc + code( pc )\i
                  ;Debug "--[ Branch False ]----------------"
               Else
                  pc + 1
                  ;Debug "--[ Branch True ]-----------------"
               EndIf
               
               vm_POP()
               Continue
               
            Case #ljHALT
               Break
      
         EndSelect
         
         ;Delay(1000)
         pc + 1
      Wend

      Logging( gadget, "VM Ended." )
      Display( gadget, "Runtime: " + FormatNumber( (ElapsedMilliseconds() - t ) / 1000 ) + " seconds. SP = " + Str(sp) + "\n" )
      
      For i = 0 To MaxData - 1
         If *arData( i )
            FreeStructure( *arData( i ) )
         EndIf
      Next
  
   EndProcedure
   
   DataSection

   TOKENS:

   Data.s   "NOOP"
   Data.s   "Hole"
   
   Data.s   "Variable"
   Data.s   "If"
   Data.s   "While"
   Data.s   "Else"
   Data.s   "Function"
   Data.s   "RET"
   Data.s   "DIM"
   Data.s   "Declare"
   
   Data.s   "Assign"
   Data.s   "Integer"
   Data.s   "Float"
   Data.s   "String"
   Data.s   "Internal"
   
   Data.s   "Sequence"
   Data.s   "LeftBrace"
   Data.s   "RightBrace"
   Data.s   "LeftParent"
   Data.s   "RightParent"
   Data.s   "SemiColon"
   Data.s   "Comma"
   Data.s   "End of file"
   Data.s   "Code Start"
   
   Data.s   "JZ"
   Data.s   "JMP"
   Data.s   "MUL"
   Data.s   "DIV"
   Data.s   "MOD"
   Data.s   "ADD"
   Data.s   "SUB"
   Data.s   "NEG"
   Data.s   "NOT" 
   Data.s   "BNOT" 
   Data.s   "LT"
   Data.s   "LE"
   Data.s   "GT"
   Data.s   "GE"
   Data.s   "EQ"
   Data.s   "NE"
   Data.s   "AND"
   Data.s   "OR"
   Data.s   "XOR"
   Data.s   "SWAP"
   Data.s   "CALL"
   Data.s   "HALT"
   
   Data.s   "PUSH"
   Data.s   "FETCH"
   Data.s   "STORE"
   Data.s   "MOV"
   Data.s   "PUSHSP"
   
   ; Functions
   
   Data.s   "REDIM"
   Data.s   "CONCAT"
   Data.s   "INTF"
   Data.s   "RND"
   Data.s   "PRTS"
   Data.s   "PRTC"
   Data.s   "PRINT"
   Data.s   "SIZE"
   Data.s   "LEN"
   Data.s   "MID"
   Data.s   "FORMAT"
   Data.s   "TRIM"
   Data.s   "LSET"
   Data.s   "RSET"
   Data.s   "TYPE"
   Data.s   "STRCASE"
   Data.s   "FINDSTR"
   Data.s   "STRREP"
   Data.s   "STRFIELD"
   Data.s   "REVSTR"
   Data.s   "CNTSTR"
   Data.s   "REPSTR"
   Data.s   "DATE"
   Data.s   "FORMDATE"
   Data.s   "PARSEDATE"
   
   Data.s   "NoMoreMusic"
   Data.s   "-"

EndDataSection

EndModule

CompilerIf #PB_Compiler_IsMainFile
   ; -- Module demo
  
  #DebugWidth  = 190
  #DebugHeight = 140
  
  Enumeration
   #MainWindow
   #BtnExit
   #BtnCompile
   #BtnRun
   #BtnFile
   ;Its important these 2 are in sequence
   #LVList
   #LVDebug
   #MainPanel
   #PnlSource
   #PnlDisplay
   #EditSource
  
  EndEnumeration
  
   Procedure         MainWindow()
   
      If OpenWindow( #MainWindow, #PB_Ignore, #PB_Ignore, 700, 440, "Compiler", #PB_Window_SizeGadget | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget )
      
         ButtonGadget( #BtnExit, 35,   0, 110, 30, "EXIT" )
         ButtonGadget( #BtnCompile, 35,  35, 110, 30, "Compile" )
         ButtonGadget( #BtnRun, 35,  70, 110, 30, "Run" )
         ButtonGadget( #BtnFile, 35, 105, 110, 30, "File" )
         ListViewGadget( #LVList, 0, #DebugHeight, #DebugWidth, WindowHeight( #MainWindow ) - #DebugHeight )
         
         PanelGadget ( #MainPanel, #DebugWidth, 0, WindowWidth( #MainWindow ) - #DebugWidth, WindowHeight( #MainWindow ) )
         AddGadgetItem ( #MainPanel, -1, "- Display -" )
            ListViewGadget( #LVDebug, 0, 0, GadgetWidth( #MainPanel ) - 5, GadgetHeight( #MainPanel ) - 25 )

         AddGadgetItem ( #MainPanel, -1, "- Source -" )
            EditorGadget( #EditSource, 0, 0, GadgetWidth( #MainPanel ) - 5, GadgetHeight( #MainPanel ) - 25, #PB_Editor_WordWrap )
            CloseGadgetList()
         
         ProcedureReturn 1
      EndIf

      ProcedureReturn 0
   EndProcedure
     
   Procedure         ResizeMain()
      Protected      x, y
      
      x = WindowWidth( #MainWindow )
      y = WindowHeight( #MainWindow )
      If x < 300 : x = 300 : EndIf
      If y < 230 : y = 230 : EndIf
      
      ResizeWindow( #MainWindow, #PB_Ignore, #PB_Ignore, x, y )
      ResizeGadget( #LVList, #PB_Ignore, #PB_Ignore, #PB_Ignore, y - 140 )
      ResizeGadget( #MainPanel, #PB_Ignore, #PB_Ignore, x - #DebugWidth, y )
      ResizeGadget( #LVDebug, #PB_Ignore, #PB_Ignore, GadgetWidth( #MainPanel ) - 5, GadgetHeight( #MainPanel ) - 25 )
      ResizeGadget( #EditSource, #PB_Ignore, #PB_Ignore, GadgetWidth( #MainPanel ) - 5, GadgetHeight( #MainPanel ) - 25 )
      
   EndProcedure

   Define         err, Event, e, ExitApplication
   Define.s       filename

   If MainWindow()
      LJLang::DebugFlags( LJLang::#ljListASM, 1 )
      LJLang::DebugFlags( LJLang::#ljListParse, 1 )
      LJLang::DebugFlags( LJLang::#ljListSyntax, 1 )
      LJLang::DebugFlags( LJLang::#ljWinDelay, 4 )
      LJLang::DebugFlags( LJLang::#ljShowRunCode, 0 )
      LJLang::DebugFlags( LJLang::#ljFastConsole, 1 )

      filename = "LJ Examples\demo18.lj"

      Repeat
         Event = WaitWindowEvent()
         
         Select Event
            Case #PB_Event_CloseWindow
               ExitApplication = #True
            
            Case #PB_Event_SizeWindow
               ResizeMain()

            Case #PB_Event_Gadget
                     
               e = EventGadget()
               
               If e = #BtnCompile
                  If filename > ""
                     If LJlang::LoadLJ( filename )
                        Debug "Error: " + LJLang::Error( @err )
                     EndIf
                     
                     If LJLang::GenerateCode( #LVList )
                        Debug "Error: " + LJLang::Error( @err )
                     EndIf
                  EndIf
               ElseIf e = #BtnExit
                  ExitApplication + 1
               ElseIf e = #BtnRun
                  LJLang::RunVm( #LVList )
               ElseIf e = #BtnFile
                  filename = OpenFileRequester( "Please choose file to load", "E:\WIP\Sources\sqliterep.2020\LJ Examples\", "LJ Files|*.lj", 0 )
                  
                  If filename
                     SetWindowTitle( #MainWindow, "Compiler - " + filename )
                     ;SetGadgetText( display, gszFileText )
                  EndIf
               EndIf
               
         EndSelect
        
      Until ExitApplication
   EndIf
   
   
CompilerEndIf
   
   
   
; IDE Options = PureBasic 6.20 (Windows - x64)
; CursorPosition = 1607
; FirstLine = 1593
; Folding = --------------
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; Warnings = Display
; EnablePurifier
; EnableCompileCount = 242
; EnableBuildCount = 0
; EnableExeConstant