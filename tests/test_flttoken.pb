; Test flttoken initialization
XIncludeFile "c2-inc-v17.pbi"

_INIT_OPCODE_NAMES

OpenConsole()
PrintN("Checking flttoken values:")
PrintN("#ljMULTIPLY flttoken = " + Str(gszATR(#ljMULTIPLY)\flttoken) + " (expected " + Str(#ljFLOATMUL) + ")")
PrintN("#ljDIVIDE flttoken = " + Str(gszATR(#ljDIVIDE)\flttoken) + " (expected " + Str(#ljFLOATDIV) + ")")
PrintN("#ljADD flttoken = " + Str(gszATR(#ljADD)\flttoken) + " (expected " + Str(#ljFLOATADD) + ")")
PrintN("#ljSUBTRACT flttoken = " + Str(gszATR(#ljSUBTRACT)\flttoken) + " (expected " + Str(#ljFLOATSUB) + ")")
PrintN("#ljNEGATE flttoken = " + Str(gszATR(#ljNEGATE)\flttoken) + " (expected " + Str(#ljFLOATNEG) + ")")

PrintN("")
PrintN("Expected enum values:")
PrintN("#ljFLOATMUL = " + Str(#ljFLOATMUL))
PrintN("#ljFLOATDIV = " + Str(#ljFLOATDIV))
PrintN("#ljFLOATADD = " + Str(#ljFLOATADD))
PrintN("#ljFLOATSUB = " + Str(#ljFLOATSUB))
PrintN("#ljFLOATNEG = " + Str(#ljFLOATNEG))
