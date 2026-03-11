; PureBasic C-Backend Bug #2: Reproducer (expression truncation)
; Compile with: pbcompilerc /OPTIMIZER /THREAD /CONSOLE pb_cbackend_bug2.pb /EXE test.exe
; Expected: Compiles and prints "hello"
; Actual: GCC error: expected ')' before '.' token
;
; Note: This bug may only trigger with /OPTIMIZER or in large compilation units.
; The ASM backend (pbcompiler) compiles this correctly.

EnableExplicit

Structure stSlot
   ss.s
   i.i
   f.d
   *ptr
   ptrtype.w
EndStructure

Structure stContainer
   Array var.stSlot(1)
EndStructure

Structure stCodeIns
   i.i
   j.i
   n.i
   ndx.i
   funcid.i
EndStructure

Global Dim *gData.stContainer(100)
Global Dim arCode.stCodeIns(100)
Global gFuncSlot.i = 5
Global pc.i = 0
Global sp.i = 10

Structure stEval
   ss.s
   i.i
   f.d
   *ptr
   ptrtype.w
EndStructure
Global Dim gEvalStack.stEval(100)

Macro _AR()
   arCode(pc)
EndMacro

Macro _SLOT(j, off)
   *gData(gFuncSlot * (j) + (off) * (1 - (j)))\var((off) * (j))
EndMacro

Procedure Setup()
   *gData(0) = AllocateStructure(stContainer)
   ReDim *gData(0)\var(5)
   *gData(5) = AllocateStructure(stContainer)
   ReDim *gData(5)\var(10)
   arCode(pc)\j = 1
   arCode(pc)\i = 3
EndProcedure

Procedure TestFetchString()
   ; This is the pattern that fails in the real project (C2FETCHS handler)
   gEvalStack(sp)\ss = _SLOT(_AR()\j, _AR()\i)\ss
   gEvalStack(sp)\i = Len(gEvalStack(sp)\ss)
   sp + 1
   pc + 1
EndProcedure

Setup()
_SLOT(arCode(0)\j, arCode(0)\i)\ss = "hello"
TestFetchString()
Debug gEvalStack(10)\ss
