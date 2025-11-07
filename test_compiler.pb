; Quick test to see generated code

XIncludeFile "c2-modules-V07.pb"
XIncludeFile "c2-vm-V04.pb"

Define err.i

If C2Lang::LoadLJ(".\Examples\09 Functions2.lj")
   Debug "Error: " + C2Lang::Error(@err)
Else
   Debug "Compiling..."
   C2Lang::Compile()

   Debug "Generated code:"
   C2Lang::ListCode(0)  ; 0 = output to Debug

   Debug ""
   Debug "Press Enter to run VM..."
   Input()

   C2VM::RunVM()
EndIf
