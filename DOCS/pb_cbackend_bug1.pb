; PureBasic C-Backend Bug #1: Reproducer
; Compile with: pbcompilerc /CONSOLE pb_cbackend_bug1.pb /EXE test.exe
; Expected: Compiles and prints "42"
; Actual: GCC error: invalid type argument of '->' (have 'struct ...')
;
; The ASM backend (pbcompiler) compiles this correctly.

EnableExplicit

Structure stElement
   ss.s
   i.i
   f.d
EndStructure

Structure stArrayData
   size.l
   Array ar.stElement(0)
EndStructure

Structure stSlot
   ss.s
   i.i
   f.d
   *ptr
   ptrtype.w
   dta.stArrayData
EndStructure

Structure stContainer
   Array var.stSlot(1)
EndStructure

Structure stEvalEntry
   ss.s
   i.i
   f.d
   *ptr
   ptrtype.w
EndStructure

; Pointer array (like our VM's *gVar)
Global Dim *gData.stContainer(100)

; Structured array (like our VM's gEvalStack)
Global Dim gEval.stEvalEntry(100)

Global gFuncSlot.i = 5
Global sp.i = 10

; Setup
*gData(5) = AllocateStructure(stContainer)
ReDim *gData(5)\var(20)
ReDim *gData(5)\var(3)\dta\ar(50)

; Set up eval stack with dynamic index
gEval(sp + 1)\ptr = 3    ; var index (dynamic)
gEval(sp + 1)\i = 7      ; array element index (dynamic)
gEval(sp)\i = 42          ; value to store

; BUG: Dynamic index from another structured array into pointer array chain
; Pattern: *PointerArray(slot)\structArray(dynamicIdx)\embedded\structArray(dynamicIdx2)\field = value
*gData(gFuncSlot)\var(gEval(sp + 1)\ptr)\dta\ar(gEval(sp + 1)\i)\i = gEval(sp)\i

Debug *gData(gFuncSlot)\var(gEval(sp + 1)\ptr)\dta\ar(gEval(sp + 1)\i)\i
