; Verify enum values
XIncludeFile "c2-inc-v17.pbi"

Debug "Direct enum values from c2-inc-v17.pbi:"
Debug "#ljMULTIPLY = " + Str(#ljMULTIPLY)
Debug "#ljDIVIDE = " + Str(#ljDIVIDE)
Debug "#ljFLOATADD = " + Str(#ljFLOATADD)
Debug "#ljFLOATMUL = " + Str(#ljFLOATMUL)
Debug ""
Debug "Check +1 off-by-one:"
Debug "MULTIPLY + 1 = " + Str(#ljMULTIPLY + 1) + " which should be DIVIDE = " + Str(#ljDIVIDE)
Debug "DIVIDE + 1 = " + Str(#ljDIVIDE + 1) + " which should be FLOATADD = " + Str(#ljFLOATADD)
