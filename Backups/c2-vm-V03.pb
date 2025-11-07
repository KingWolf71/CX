
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
;XIncludeFile         "c2-modules-V02.pb"


;- =====================================
;- Virtual Machine
;- =====================================
DeclareModule C2VM
   EnableExplicit
   UseModule C2Common
 
   Declare           RunVM()
EndDeclareModule

Module C2VM
   EnableExplicit
   UseModule C2Lang
   
   ;- Constants
   Enumeration
      #MainWindow
      #BtnExit
      #BtnLoad
      #BtnRun
      #edConsole
   EndEnumeration
   
   Structure stProfiler
      count.i
      time.i
   EndStructure
   
   ;- Globals
   Global            sp = 0           ; stack pointer
   Global            pc = 0
   Global            cy = 0
   Global            cs = 0
   Global            gExitApplication
   Global            cline.s
   Global            gDecs.i = 3
   
   Global Dim        *ptrJumpTable(1)
   Global Dim        arCode.stType(1)
   
   ;- Macros
   Macro             vm_Comparators( operator )
      sp - 1
      If gVar(sp-1)\i operator gVar(sp)\i
         gVar(sp-1)\i = 1
      Else
         gVar(sp-1)\i = 0
      EndIf
      pc + 1
   EndMacro
   Macro             vm_BitOperation( operand )
      sp - 1
      gVar(sp-1)\i = gVar(sp-1)\i operand gVar(sp)\i
      pc + 1
   EndMacro
   Macro             vm_FloatComparators( operator )
      sp - 1
      If gVar(sp-1)\f operator gVar(sp)\f
         gVar(sp-1)\i = 1
      Else
         gVar(sp-1)\i = 0
      EndIf
      pc + 1
   EndMacro
   Macro             vm_FloatOperation( operand )
      sp - 1
      gVar(sp-1)\f = gVar(sp-1)\f operand gVar(sp)\f
      pc + 1
   EndMacro
   Macro             vm_ListToArray( ll, ar )
      i = ListSize( ll() )
      ReDim ar( i )
      i = 0
      
      ForEach ll()
         ar( i ) = ll()
         i + 1
      Next
   EndMacro
   Macro             CPC()
      arCode(pc)\code
   EndMacro
   Macro             _AR()
      arCode(pc)
   EndMacro
   
   XIncludeFile      "c2-vm-commands-v02ok.pb"
   
   ;- Console GUI
   Procedure         MainWindow(name.s)
   
      If OpenWindow( #MainWindow, #PB_Ignore, #PB_Ignore, 760, 680, name, #PB_Window_SizeGadget | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget | #PB_Window_TitleBar )
         ButtonGadget( #BtnExit,    5,    3,  90,  29, "EXIT" )
         ButtonGadget( #BtnLoad,  100,    3,  90,  29, "Load/Compile" )
         ButtonGadget( #BtnRun,   200,    3,  90,  29, "Run" )
         
         EditorGadget( #edConsole, 0,  35, 760, 650, #PB_Editor_ReadOnly )
         AddGadgetItem( #edConsole, -1, "" )
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
      ResizeGadget( #edConsole, #PB_Ignore, #PB_Ignore, x, y - 30 )
      
   EndProcedure
   ;- VM components
   Procedure            InitVM()
      
      ReDim *ptrJumpTable(gnTotalTokens)
      
      *ptrJumpTable(#ljFetch)          = @C2FetchPush()
      *ptrJumpTable(#ljPush)           = @C2FetchPush()
      *ptrJumpTable(#ljStore)          = @C2Store()
      *ptrJumpTable(#ljMov)            = @C2Mov()
      *ptrJumpTable(#ljJMP)            = @C2JMP()
      *ptrJumpTable(#ljJZ)             = @C2JZ()
      *ptrJumpTable(#ljADD)            = @C2ADD()
      *ptrJumpTable(#ljSUBTRACT)       = @C2SUBTRACT()
      *ptrJumpTable(#ljGREATER)        = @C2GREATER()
      *ptrJumpTable(#ljLESS)           = @C2LESS()
      *ptrJumpTable(#ljLESSEQUAL)      = @C2LESSEQUAL()
      *ptrJumpTable(#ljGreaterEqual)   = @C2GREATEREQUAL()
      *ptrJumpTable(#ljNotEqual)       = @C2NOTEQUAL()
      *ptrJumpTable(#ljEQUAL)          = @C2EQUAL()
      *ptrJumpTable(#ljMULTIPLY)       = @C2MULTIPLY()
      *ptrJumpTable(#ljAND)            = @C2AND()
      *ptrJumpTable(#ljOr)             = @C2OR()
      *ptrJumpTable(#ljXOR)            = @C2XOR()
      *ptrJumpTable(#ljNOT)            = @C2NOT()
      *ptrJumpTable(#ljNEGATE)         = @C2NEGATE()
      *ptrJumpTable(#ljDIVIDE)         = @C2DIVIDE()
      *ptrJumpTable(#ljMOD)            = @C2MOD()
      
      *ptrJumpTable(#ljPRTS)           = @C2PRTS()
      *ptrJumpTable(#ljPRTC)           = @C2PRTC()
      *ptrJumpTable(#ljPRTI)           = @C2PRTI()
      *ptrJumpTable(#ljPRTF)           = @C2PRTF()
      
      *ptrJumpTable(#ljFLOATNEG)       = @C2FLOATNEGATE()
      *ptrJumpTable(#ljFLOATDIV)       = @C2FLOATDIVIDE()
      *ptrJumpTable(#ljFLOATADD)       = @C2FLOATADD()
      *ptrJumpTable(#ljFLOATSUB)       = @C2FLOATSUB()
      *ptrJumpTable(#ljFLOATMUL)       = @C2FLOATMUL()
      
      *ptrJumpTable(#ljFLOATEQ)        = @C2FLOATEQUAL()
      *ptrJumpTable(#ljFLOATNE)        = @C2FLOATNOTEQUAL()
      *ptrJumpTable(#ljFLOATLE)        = @C2FLOATLESSEQUAL()
      *ptrJumpTable(#ljFLOATGE)        = @C2FLOATGREATEREQUAL()
      *ptrJumpTable(#ljFLOATGR)        = @C2FLOATGREATER()
      *ptrJumpTable(#ljFLOATLESS)      = @C2FLOATLESS()
      
      ;*ptrJumpTable(#ljDEC)            = @ljDEC()
      ;*ptrJumpTable(#ljINC)            = @ljINC()      
      ;*ptrJumpTable(#ljACC)            = @ljACC()
      ;*ptrJumpTable(#ljGET)            = @ljGET()
      ;*ptrJumpTable(#ljADDOBJ)         = @ljADDOBJ()
      ;*ptrJumpTable(#ljPUSHSP)         = @ljPUSHSP()
      ;*ptrJumpTable(#ljCALL)           = @ljCALL()
      ;*ptrJumpTable(#ljReturn)         = @ljRETURN()
      ;*ptrJumpTable(#ljADDRESS)        = @ljADDRESS()
      ;*ptrJumpTable(#ljBYREF)          = @ljBYREF()
      ;*ptrJumpTable(#ljPoke)           = @ljPOKE()
      ;*ptrJumpTable(#ljFUNCADDR)       = @ljFUNCADDR()
      ;*ptrJumpTable(#ljEVAL)           = @ljEVAL()
      ;*ptrJumpTable(#ljNEW)            = @ljNEW()
      ;*ptrJumpTable(#ljConvert)        = @ljCONVERT()
      ;*ptrJumpTable(#ljReplace)        = @ljREPLACE()
      ;*ptrJumpTable(#ljNewRecord)      = @ljNewRecord()
      ;*ptrJumpTable(#ljDelRecord)      = @ljDelRecord()
      ;*ptrJumpTable(#ljINITOBJ)        = @ljInitObject()
      ;*ptrJumpTable(#ljForEach)        = @ljFOREACH()
      ;*ptrJumpTable(#ljLOOPOBJ)        = @ljLOOPOBJ()
      ;*ptrJumpTable(#ljDESTROYOBJ)     = @ljDESTROYOBJ()
      ;*ptrJumpTable(#ljObjPos)         = @ljOBJPOS()
      ;*ptrJumpTable(#ljDELOBJ)         = @ljDELOBJ()
      ;*ptrJumpTable(#ljInitTimer)      = @ljINITTIMER()
      ;*ptrJumpTable(#ljEndTimer)       = @ljENDTIMER()
      ;*ptrJumpTable(#ljTypeOf)         = @ljTYPEOF()
      ;*ptrJumpTable(#ljSizeOf)         = @ljSizeOf()
      ;*ptrJumpTable(#ljRndArray)       = @ljRNDARRAY()
      ;*ptrJumpTable(#ljHex)            = @ljHex()
      ;*ptrJumpTable(#ljRepString)      = @ljRepString()
      ;*ptrJumpTable(#ljRemoveString)   = @ljRemoveString()
      ;*ptrJumpTable(#ljInsertString)   = @ljInsertString()
      ;*ptrJumpTable(#ljCountString)    = @ljCountString()
      ;*ptrJumpTable(#ljTRIM)           = @ljTRIM()
      ;*ptrJumpTable(#ljParseDate)      = @ljParseDate()
      ;*ptrJumpTable(#ljSplitString)    = @ljSplitString()
      ;*ptrJumpTable(#ljDate)           = @ljDate()
      ;*ptrJumpTable(#ljMID)            = @ljMID()
      ;*ptrJumpTable(#ljFormatNumber)   = @ljFormatNumber()
      ;*ptrJumpTable(#ljFormatDate)     = @ljFormatDate()
      ;*ptrJumpTable(#ljFindStr)        = @ljFindStr()
      ;*ptrJumpTable(#ljRND)            = @ljRND()
      ;*ptrJumpTable(#ljREDIM)          = @ljREDIM()
      ;*ptrJumpTable(#ljPRTS)           = @ljPRTS()
      ;*ptrJumpTable(#ljPRTC)           = @ljPRTC()
      ;*ptrJumpTable(#ljLEN)            = @ljLEN()
      ;*ptrJumpTable(#ljINTFUNC)        = @ljINTFUNC()
      ;*ptrJumpTable(#ljReverseString)  = @ljReverseString()
      ;*ptrJumpTable(#ljLSET)           = @ljLset()
      ;*ptrJumpTable(#ljRSET)           = @ljRSet()
      ;*ptrJumpTable(#ljCapilize)       = @ljCapitalize()
      ;*ptrJumpTable(#ljSTRING)         = @ljString()
      ;*ptrJumpTable(#ljStrRep)         = @ljStrRep()
   EndProcedure
   
   Procedure         Execute(*p = 0)
      Protected      i, j
      Protected      t, t1
      Protected.s    temp, name, line
      Dim            arProfiler.stProfiler(1)
   
      t     = ElapsedMilliseconds()
      sp    = gnLastVariable
      cy    = 0
      pc    = 0
      ReDim arProfiler(gnTotalTokens)
      
      While CPC() <> #ljHALT Or gExitApplication
         ;ASMLine( arCode(pc), 1 )
         ;Debug Line
         
         ;- Only for testing and debugging
         CompilerIf #C2PROFILER > 0
            arProfiler(CPC())\count + 1
            t1 = ElapsedMilliseconds()
         CompilerEndIf
         
         CallFunctionFast(*ptrJumpTable(CPC()))
         
         CompilerIf #C2PROFILER > 0
            arProfiler(CPC())\time + (ElapsedMilliseconds() - t1)
         CompilerEndIf
      Wend
      
      AddGadgetItem( #edConsole, -1, "Runtime: " + FormatNumber( (ElapsedMilliseconds() - t ) / 1000 ) + " seconds. Stack=" + Str(sp - gnLastVariable) )
      
      CompilerIf #C2PROFILER > 0
         AddGadgetItem( #edConsole, -1, "====[Stats]=======================================" )
         For i = 0 To gnTotalTokens
            If arProfiler(i)\count > 0
               AddGadgetItem( #edConsole, -1, LSet(gszATR(i)\s,20) + RSet(FormatNumber(arProfiler(i)\count,0),16) + RSet( FormatNumber( arProfiler(i)\time/1000,3,".","," ), 12) + " total" + RSet( FormatNumber( arProfiler(i)\time / arProfiler(i)\count,3,".","," ), 16) )
               ;AddGadgetItem( #edConsole, -1, LSet(gszATR(i),20) + RSet(FormatNumber(arProfiler(i)\count,0),16) + RSet( FormatNumber( arProfiler(i)\time/1000,3,".","," ), 12) + " total" + RSet( FormatNumber( arProfiler(i)\time / arProfiler(i)\count,3,".","," ), 16) )
               arProfiler(i)\count = 0
               arProfiler(i)\time  = 0
            EndIf
         Next
         AddGadgetItem( #edConsole, -1, "==================================================" )
      CompilerEndIf

   EndProcedure
   
   ; Execute the code list
   Procedure         RunVM()
      Protected      i, j, e
      Protected      err
      Protected      x, y
      Protected.s    temp, name, filename
      Protected      win, Event
      Protected      thRun
      
      InitVM()
      
      ;Execute #pragmas first
      name = mapPragmas("appname")
      temp = mapPragmas("console")
      temp = LCase(temp)
      
      If temp = "on"
         win = MainWindow( name )
         temp = mapPragmas("consolesize")
         x = Val( StringField(temp, 1, "x") )
         y = Val( StringField(temp, 2, "x") )
         ResizeWindow( #MainWindow, #PB_Ignore, #PB_Ignore, x, y )
      EndIf
      
      cs = ListSize( llObjects() )
      vm_ListToArray( llObjects, arCode )
      
      If win
         thRun = CreateThread(@Execute(), 0 )
      
         Repeat
            If IsWindow(#MainWindow)
               Event = WaitWindowEvent(32)
            
               Select Event
                  Case #PB_Event_CloseWindow
                     gExitApplication = #True
                  
                  Case #PB_Event_SizeWindow
                     ResizeMain()
      
                  Case #PB_Event_Gadget
                     e = EventGadget()
                     
                     If e = #BtnExit
                        gExitApplication = #True
                     ElseIf e = #BtnLoad
                        filename = OpenFileRequester( "Please choose source", ".\Examples\", "LJ Files|*.lj", 0 )
                        
                        If IsThread( thRun )
                           KillThread( thRun)
                        EndIf
      
                        If filename > ""
                           If C2Lang::LoadLJ( filename )
                              Debug "Error: " + C2Lang::Error( @err )
                           Else
                              C2Lang::Compile()
                              C2Lang::ListCode()
                           EndIf
                        EndIf
                     
                     ElseIf e = #BtnRun
                        CloseWindow( #MainWindow )
                        C2VM::RunVM()
                     EndIf
               EndSelect
            Else
               Delay( 64)
            EndIf  
         Until gExitApplication
      Else
         Execute()
      EndIf
   EndProcedure
EndModule

; IDE Options = PureBasic 6.20 (Windows - x64)
; CursorPosition = 103
; FirstLine = 87
; Folding = ---
; Markers = 17
; Optimizer
; EnableAsm
; EnableThread
; EnableXP
; SharedUCRT
; CPU = 1
; EnablePurifier
; EnableCompileCount = 182
; EnableBuildCount = 0
; EnableExeConstant