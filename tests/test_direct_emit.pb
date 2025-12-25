; Test if the condition at line 5415 works correctly
XIncludeFile "c2-inc-v17.pbi"
_INIT_OPCODE_NAMES

; Simulate the condition at line 5415
Define opType.i = #C2FLAG_FLOAT  ; 8
Define NodeType.i = #ljMULTIPLY  ; 37

Debug "Testing line 5415 condition:"
Debug "opType = " + Str(opType) + " (#C2FLAG_FLOAT = " + Str(#C2FLAG_FLOAT) + ")"
Debug "NodeType = " + Str(NodeType) + " (#ljMULTIPLY = " + Str(#ljMULTIPLY) + ")"
Debug "gszATR(NodeType)\flttoken = " + Str(gszATR(NodeType)\flttoken) + " (#ljFLOATMUL = " + Str(#ljFLOATMUL) + ")"
Debug ""
Debug "Condition parts:"
Debug "  opType & #C2FLAG_FLOAT = " + Str(opType & #C2FLAG_FLOAT)
Debug "  gszATR(NodeType)\flttoken > 0 = " + Str(Bool(gszATR(NodeType)\flttoken > 0))
Debug ""

If opType & #C2FLAG_FLOAT And gszATR(NodeType)\flttoken > 0
   Debug "RESULT: Would emit FLOATMUL = " + Str(gszATR(NodeType)\flttoken)
Else
   Debug "RESULT: Would emit NodeType = " + Str(NodeType)
EndIf
