; Test condition evaluation
XIncludeFile "c2-inc-v17.pbi"
_INIT_OPCODE_NAMES

Define opType.i = #C2FLAG_FLOAT
Define nodeType.i = #ljMULTIPLY

Debug "opType = " + Str(opType)
Debug "#C2FLAG_FLOAT = " + Str(#C2FLAG_FLOAT)
Debug "nodeType = " + Str(nodeType)
Debug "gszATR(nodeType)\flttoken = " + Str(gszATR(nodeType)\flttoken)
Debug ""
Debug "opType & #C2FLAG_FLOAT = " + Str(opType & #C2FLAG_FLOAT)
Debug "gszATR(nodeType)\flttoken > 0 = " + Str(Bool(gszATR(nodeType)\flttoken > 0))
Debug ""
Debug "FULL CONDITION:"
Debug "opType & #C2FLAG_FLOAT And gszATR(nodeType)\flttoken > 0 = " + Str(Bool(opType & #C2FLAG_FLOAT And gszATR(nodeType)\flttoken > 0))
