; Test opcode values
XIncludeFile "c2-inc-v17.pbi"
_INIT_OPCODE_NAMES

Debug "Testing opcode values:"
Debug "#ljMULTIPLY = " + Str(#ljMULTIPLY)
Debug "#ljFLOATMUL = " + Str(#ljFLOATMUL)
Debug "#ljAdd = " + Str(#ljAdd)
Debug "#ljFLOATADD = " + Str(#ljFLOATADD)
Debug "#ljDIVIDE = " + Str(#ljDIVIDE)
Debug "#ljFLOATDIV = " + Str(#ljFLOATDIV)
Debug ""
Debug "Opcode 38 name = " + gszATR(38)\s
Debug ""
Debug "flttoken values:"
Debug "gszATR(#ljMULTIPLY)\flttoken = " + Str(gszATR(#ljMULTIPLY)\flttoken)
Debug "gszATR(#ljAdd)\flttoken = " + Str(gszATR(#ljAdd)\flttoken)
Debug "gszATR(#ljDIVIDE)\flttoken = " + Str(gszATR(#ljDIVIDE)\flttoken)
