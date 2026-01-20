; ============================================================================
; D-Plus Test Runner
; ============================================================================
; Automatically runs all source test files in Examples directory
; Captures output, timing, and creates JSON results
; Compares with previous runs to detect regressions
; V1.039.37 - Refactored to use PureBasic JSON library
; ============================================================================

EnableExplicit

; Open console FIRST (before including modules that might use Print)
CompilerIf #PB_Compiler_ExecutableFormat = #PB_Compiler_Console
   OpenConsole()
CompilerEndIf

; Include the LJ2 compiler and VM
XIncludeFile "c2-modules-V20.pb"

; ============================================================================
; Structures
; ============================================================================

Structure TestResult
  filename.s        ; Test file name
  elapsed.i         ; Execution time in milliseconds
  output.s          ; Console output
  exitCode.i        ; Exit code (0 = success)
  error.s           ; Error message if any
  timestamp.s       ; When test was run
  sourceHash.s      ; MD5 hash of source file
EndStructure

Structure ChangedFile
  filename.s        ; File that changed
  changeType.s      ; "OUTPUT", "SOURCE", or "NEW"
  prevOutput.s      ; Previous output
  newOutput.s       ; New output
  prevHash.s        ; Previous source hash
  newHash.s         ; New source hash
EndStructure

; ============================================================================
; Global Variables
; ============================================================================

Global NewList gTests.TestResult()
Global NewList gPrevTests.TestResult()
Global NewList gChangedFiles.ChangedFile()
Global resultsFile.s
Global diffFile.s
Global examplesPath.s
Global testCount.i
Global passCount.i
Global failCount.i
Global changedCount.i
Global newCount.i

UseMD5Fingerprint()

; ============================================================================
; Helper Procedures
; ============================================================================

Procedure.s ComputeFileHash(filepath.s)
  ; Compute MD5 hash of file contents using FileFingerprint
  Protected hash.s

  ; Verify file exists
  If FileSize(filepath) < 0
    ProcedureReturn ""
  EndIf

  ; Compute MD5 hash directly from file
  hash = FileFingerprint(filepath, #PB_Cipher_MD5)

  ProcedureReturn hash
EndProcedure

Procedure SaveTestResultsJSON(filename.s)
  ; Save test results to JSON file using PureBasic JSON library
  Protected json.i, rootObj.i, testsArray.i, testObj.i
  Protected file.i

  json = CreateJSON(#PB_Any)
  If Not json
    PrintN("ERROR: Cannot create JSON object")
    ProcedureReturn
  EndIf

  rootObj = SetJSONObject(JSONValue(json))

  ; Add metadata
  SetJSONString(AddJSONMember(rootObj, "timestamp"), FormatDate("%yyyy-%mm-%dd %hh:%ii:%ss", Date()))
  SetJSONInteger(AddJSONMember(rootObj, "testCount"), testCount)

  ; Add tests array
  testsArray = SetJSONArray(AddJSONMember(rootObj, "tests"))

  ForEach gTests()
    testObj = SetJSONObject(AddJSONElement(testsArray))
    SetJSONString(AddJSONMember(testObj, "filename"), gTests()\filename)
    SetJSONInteger(AddJSONMember(testObj, "elapsed"), gTests()\elapsed)
    SetJSONInteger(AddJSONMember(testObj, "exitCode"), gTests()\exitCode)
    SetJSONString(AddJSONMember(testObj, "error"), gTests()\error)
    SetJSONString(AddJSONMember(testObj, "timestamp"), gTests()\timestamp)
    SetJSONString(AddJSONMember(testObj, "sourceHash"), gTests()\sourceHash)
    SetJSONString(AddJSONMember(testObj, "output"), gTests()\output)
  Next

  ; Write to file with formatting
  file = CreateFile(#PB_Any, filename)
  If file
    WriteString(file, ComposeJSON(json, #PB_JSON_PrettyPrint))
    CloseFile(file)
    PrintN("Results saved to: " + filename)
  Else
    PrintN("ERROR: Cannot create results file: " + filename)
  EndIf

  FreeJSON(json)
EndProcedure

Procedure SaveDiffFile(filename.s)
  ; Save diff file showing what changed between runs
  Protected file.i
  Protected i.i
  Protected prevLines.s, newLines.s
  Protected prevLine.s, newLine.s
  Protected maxLines.i
  Protected lineNum.i

  If ListSize(gChangedFiles()) = 0
    ProcedureReturn  ; Nothing changed
  EndIf

  file = CreateFile(#PB_Any, filename)
  If Not file
    PrintN("ERROR: Cannot create diff file: " + filename)
    ProcedureReturn
  EndIf

  WriteStringN(file, "================================================================================")
  WriteStringN(file, "D-Plus Test Diff Report - " + FormatDate("%yyyy-%mm-%dd %hh:%ii:%ss", Date()))
  WriteStringN(file, "================================================================================")
  WriteStringN(file, "")
  WriteStringN(file, "Changed files: " + Str(ListSize(gChangedFiles())))
  WriteStringN(file, "")

  ForEach gChangedFiles()
    WriteStringN(file, "--------------------------------------------------------------------------------")
    WriteStringN(file, "FILE: " + gChangedFiles()\filename)
    WriteStringN(file, "TYPE: " + gChangedFiles()\changeType)
    WriteStringN(file, "--------------------------------------------------------------------------------")

    Select gChangedFiles()\changeType
      Case "NEW"
        WriteStringN(file, ">>> NEW TEST - No previous output to compare")
        WriteStringN(file, "")
        WriteStringN(file, "Current output:")
        WriteStringN(file, gChangedFiles()\newOutput)

      Case "SOURCE"
        WriteStringN(file, ">>> SOURCE FILE MODIFIED")
        WriteStringN(file, "Previous hash: " + gChangedFiles()\prevHash)
        WriteStringN(file, "Current hash:  " + gChangedFiles()\newHash)
        WriteStringN(file, "")
        WriteStringN(file, "--- PREVIOUS OUTPUT ---")
        WriteStringN(file, gChangedFiles()\prevOutput)
        WriteStringN(file, "")
        WriteStringN(file, "+++ CURRENT OUTPUT +++")
        WriteStringN(file, gChangedFiles()\newOutput)

      Case "OUTPUT"
        WriteStringN(file, ">>> OUTPUT CHANGED (source unchanged - possible regression!)")
        WriteStringN(file, "")
        WriteStringN(file, "--- PREVIOUS OUTPUT ---")
        WriteStringN(file, gChangedFiles()\prevOutput)
        WriteStringN(file, "")
        WriteStringN(file, "+++ CURRENT OUTPUT +++")
        WriteStringN(file, gChangedFiles()\newOutput)

    EndSelect

    WriteStringN(file, "")
  Next

  WriteStringN(file, "================================================================================")
  WriteStringN(file, "END OF DIFF REPORT")
  WriteStringN(file, "================================================================================")

  CloseFile(file)
  PrintN("Diff saved to: " + filename)
EndProcedure

Procedure LoadTestResultsJSON(filename.s)
  ; Load previous test results from JSON file using PureBasic JSON library
  Protected json.i, rootValue.i, testsArray.i, testObj.i
  Protected i.i, testCount.i

  If Not FileSize(filename) > 0
    ProcedureReturn
  EndIf

  json = LoadJSON(#PB_Any, filename)
  If Not json
    ProcedureReturn
  EndIf

  rootValue = JSONValue(json)
  If JSONType(rootValue) <> #PB_JSON_Object
    FreeJSON(json)
    ProcedureReturn
  EndIf

  ; Get tests array
  testsArray = GetJSONMember(rootValue, "tests")
  If testsArray And JSONType(testsArray) = #PB_JSON_Array
    testCount = JSONArraySize(testsArray)

    For i = 0 To testCount - 1
      testObj = GetJSONElement(testsArray, i)
      If testObj And JSONType(testObj) = #PB_JSON_Object
        AddElement(gPrevTests())

        Protected memberValue.i

        memberValue = GetJSONMember(testObj, "filename")
        If memberValue And JSONType(memberValue) = #PB_JSON_String
          gPrevTests()\filename = GetJSONString(memberValue)
        EndIf

        memberValue = GetJSONMember(testObj, "elapsed")
        If memberValue And JSONType(memberValue) = #PB_JSON_Number
          gPrevTests()\elapsed = GetJSONInteger(memberValue)
        EndIf

        memberValue = GetJSONMember(testObj, "exitCode")
        If memberValue And JSONType(memberValue) = #PB_JSON_Number
          gPrevTests()\exitCode = GetJSONInteger(memberValue)
        EndIf

        memberValue = GetJSONMember(testObj, "error")
        If memberValue And JSONType(memberValue) = #PB_JSON_String
          gPrevTests()\error = GetJSONString(memberValue)
        EndIf

        memberValue = GetJSONMember(testObj, "timestamp")
        If memberValue And JSONType(memberValue) = #PB_JSON_String
          gPrevTests()\timestamp = GetJSONString(memberValue)
        EndIf

        memberValue = GetJSONMember(testObj, "sourceHash")
        If memberValue And JSONType(memberValue) = #PB_JSON_String
          gPrevTests()\sourceHash = GetJSONString(memberValue)
        EndIf

        memberValue = GetJSONMember(testObj, "output")
        If memberValue And JSONType(memberValue) = #PB_JSON_String
          gPrevTests()\output = GetJSONString(memberValue)
        EndIf
      EndIf
    Next
  EndIf

  FreeJSON(json)
  PrintN("Loaded previous results: " + Str(ListSize(gPrevTests())) + " tests")
EndProcedure

; ============================================================================
; Test Execution
; ============================================================================

Procedure.s FindPreviousTestOutput(filename.s)
  ; Find output from previous run
  Protected result.s

  result = ""
  ForEach gPrevTests()
    If gPrevTests()\filename = filename
      result = gPrevTests()\output
      Break
    EndIf
  Next

  ProcedureReturn result
EndProcedure

Procedure.s FindPreviousTestHash(filename.s)
  ; Find source hash from previous run
  Protected result.s

  result = ""
  ForEach gPrevTests()
    If gPrevTests()\filename = filename
      result = gPrevTests()\sourceHash
      Break
    EndIf
  Next

  ProcedureReturn result
EndProcedure

Procedure RunTest(filepath.s, filename.s)
  ; Run a single test file using integrated compiler
  Protected err.i
  Protected output.s
  Protected startTime.i
  Protected endTime.i
  Protected prevOutput.s
  Protected changed.i

  AddElement(gTests())
  gTests()\filename = filename
  gTests()\timestamp = FormatDate("%yyyy-%mm-%dd %hh:%ii:%ss", Date())
  gTests()\sourceHash = ComputeFileHash(filepath)

  Print("Running: " + filename + " ... ")

  ; Clear VM state before each test
  C2VM::vmClearRun()

  ; Clear batch output and errors (CRITICAL: must clear gExit too!)
  C2VM::gBatchOutput = ""
  C2Lang::gszlastError = ""
  C2Lang::gExit = 0

  ; Verify file exists
  If FileSize(filepath) < 0
    gTests()\exitCode = 1
    gTests()\error = "File not found: " + filepath
    gTests()\output = gTests()\error
    failCount + 1
    testCount + 1
    PrintN("[FILE NOT FOUND]")
    Delay(100)
    ProcedureReturn
  EndIf

  ; Start timing
  startTime = ElapsedMilliseconds()

  ; Load and compile the test file
  If C2Lang::LoadLJ(filepath)
    ; Compilation error
    gTests()\exitCode = 1
    gTests()\error = "Compilation error: " + C2Lang::Error(@err)
    gTests()\output = gTests()\error
    failCount + 1
    testCount + 1
    PrintN("[COMPILE ERROR]")
    Delay(100)  ; Small pause between tests
    ProcedureReturn
  EndIf

  ; Compile the code
  C2Lang::Compile()

  ; Check for compilation errors after Compile()
  ; Check both gszlastError AND gExit (AST errors set gExit=-1)
  If C2Lang::gszlastError <> "" Or C2Lang::gExit <> 0
    gTests()\exitCode = 1
    If C2Lang::gszlastError <> ""
      gTests()\error = "Compilation error: " + C2Lang::gszlastError
    Else
      ; gExit set but no error message - AST error
      gTests()\error = "AST Error (gExit=" + Str(C2Lang::gExit) + ")"
      If C2Lang::Error(@err) <> ""
        gTests()\error = gTests()\error + ": " + C2Lang::Error(@err)
      EndIf
    EndIf
    gTests()\output = gTests()\error
    failCount + 1
    testCount + 1
    Print("[COMPILE ERROR] gExit=" + Str(C2Lang::gExit) + " errMsg='" + C2Lang::gszlastError + "'")
    PrintN("")
    Delay(100)
    ProcedureReturn
  EndIf

  ; Run the VM
  C2VM::RunVM()

  ; End timing
  endTime = ElapsedMilliseconds()
  gTests()\elapsed = endTime - startTime

  ; Capture output from batch mode
  output = C2VM::gBatchOutput

  ; Check for runtime errors
  If C2Lang::gszlastError <> ""
    ; Runtime error occurred
    gTests()\exitCode = 1
    gTests()\error = "Runtime error: " + C2Lang::gszlastError
    gTests()\output = output + Chr(10) + "ERROR: " + gTests()\error
    failCount + 1
    testCount + 1
    PrintN("[RUNTIME ERROR]")
    Delay(100)
    ProcedureReturn
  EndIf

  ; Check if output seems valid (not empty for tests that should produce output)
  If Len(output) = 0
    ; Empty output might indicate a problem
    gTests()\exitCode = 0  ; Not necessarily an error, but worth noting
    gTests()\output = ""
  Else
    gTests()\output = output
    gTests()\exitCode = 0
  EndIf

  ; Check if output or source changed
  Protected prevHash.s
  prevOutput = FindPreviousTestOutput(filename)
  prevHash = FindPreviousTestHash(filename)

  If prevOutput <> ""
    ; Check if source file was modified
    If prevHash <> "" And prevHash <> gTests()\sourceHash
      Print("[SOURCE CHANGED] ")
      changedCount + 1
      ; Track the change
      AddElement(gChangedFiles())
      gChangedFiles()\filename = filename
      gChangedFiles()\changeType = "SOURCE"
      gChangedFiles()\prevOutput = prevOutput
      gChangedFiles()\newOutput = output
      gChangedFiles()\prevHash = prevHash
      gChangedFiles()\newHash = gTests()\sourceHash
    ElseIf prevOutput <> output
      ; Output changed but source didn't (regression or optimization)
      changed = #True
      changedCount + 1
      Print("[OUTPUT CHANGED] ")
      ; Track the change
      AddElement(gChangedFiles())
      gChangedFiles()\filename = filename
      gChangedFiles()\changeType = "OUTPUT"
      gChangedFiles()\prevOutput = prevOutput
      gChangedFiles()\newOutput = output
      gChangedFiles()\prevHash = prevHash
      gChangedFiles()\newHash = gTests()\sourceHash
    Else
      Print("[OK] ")
    EndIf
  Else
    Print("[NEW] ")
    newCount + 1
    ; Track new test
    AddElement(gChangedFiles())
    gChangedFiles()\filename = filename
    gChangedFiles()\changeType = "NEW"
    gChangedFiles()\prevOutput = ""
    gChangedFiles()\newOutput = output
    gChangedFiles()\prevHash = ""
    gChangedFiles()\newHash = gTests()\sourceHash
  EndIf

  ; Show output length for debugging
  passCount + 1
  PrintN(Str(gTests()\elapsed) + "ms (" + Str(Len(output)) + " chars)")
  testCount + 1

  ; Small pause between tests
  Delay(100)
EndProcedure

Procedure ScanAndRunTests()
  ; Scan Examples directory and run all source files
  Protected dir.i
  Protected filename.s
  Protected filepath.s

  PrintN("Scanning: " + examplesPath)
  PrintN("")

  dir = ExamineDirectory(#PB_Any, examplesPath, C2Common::#C2_FILE_PATTERN$)
  If dir
    While NextDirectoryEntry(dir)
      filename = DirectoryEntryName(dir)
      If filename <> "." And filename <> ".."
        filepath = examplesPath + "\" + filename
        RunTest(filepath, filename)
      EndIf
    Wend
    FinishDirectory(dir)
  Else
    PrintN("ERROR: Cannot open directory: " + examplesPath)
  EndIf
EndProcedure

; ============================================================================
; Main Program
; ============================================================================

Procedure Main()
  ; Console already opened at top of file
  ; Initialize paths
  examplesPath = GetCurrentDirectory() + "Examples"
  resultsFile = GetCurrentDirectory() + "test_results.json"
  diffFile = GetCurrentDirectory() + "test_diff.txt"

  ; Check if Examples directory exists
  If Not FileSize(examplesPath) = -2  ; -2 means directory
    PrintN("ERROR: Examples directory not found: " + examplesPath)
    PrintN("")
    PrintN("Press Enter to exit...")
    Input()
    ProcedureReturn
  EndIf

  ; Initialize counters
  testCount = 0
  passCount = 0
  failCount = 0
  changedCount = 0
  newCount = 0

  ; Display header with compiler version
  Define compilerVersion.s, vf.i
  vf = ReadFile(#PB_Any, "_lj2.ver")
  If vf
    compilerVersion = Trim(ReadString(vf))
    CloseFile(vf)
  Else
    compilerVersion = "unknown"
  EndIf

  PrintN("============================================================================")
  PrintN("D-Plus Test Runner v1.9 | Compiler: " + compilerVersion)
  PrintN("============================================================================")
  PrintN("")

  ; Load previous results
  LoadTestResultsJSON(resultsFile)
  PrintN("")

  ; Run all tests
  ScanAndRunTests()

  ; Display summary
  PrintN("")
  PrintN("============================================================================")
  PrintN("Test Summary")
  PrintN("============================================================================")
  PrintN("Total:   " + Str(testCount))
  PrintN("Passed:  " + Str(passCount))
  PrintN("Failed:  " + Str(failCount))
  PrintN("Changed: " + Str(changedCount))
  PrintN("New:     " + Str(newCount))
  PrintN("============================================================================")

  ; Display failed tests if any
  If failCount > 0
    PrintN("")
    PrintN("Failed Tests:")
    PrintN("----------------------------------------------------------------------------")
    ForEach gTests()
      If gTests()\exitCode <> 0
        PrintN("  " + gTests()\filename)
        If gTests()\error <> ""
          PrintN("    Error: " + gTests()\error)
        EndIf
      EndIf
    Next
    PrintN("============================================================================")
  EndIf

  ; Display changed files if any
  If ListSize(gChangedFiles()) > 0
    PrintN("")
    PrintN("Changed Files:")
    PrintN("----------------------------------------------------------------------------")
    ForEach gChangedFiles()
      Select gChangedFiles()\changeType
        Case "NEW"
          PrintN("  [NEW]    " + gChangedFiles()\filename)
        Case "SOURCE"
          PrintN("  [SOURCE] " + gChangedFiles()\filename)
        Case "OUTPUT"
          PrintN("  [OUTPUT] " + gChangedFiles()\filename + " (POSSIBLE REGRESSION!)")
      EndSelect
    Next
    PrintN("============================================================================")
  EndIf
  PrintN("")

  ; Save results
  SaveTestResultsJSON(resultsFile)

  ; Save diff file if there are changes
  If ListSize(gChangedFiles()) > 0
    SaveDiffFile(diffFile)
  EndIf

  PrintN("")
  PrintN("Press Enter to exit...")
  Input()
EndProcedure

; Entry point
Main()

; ExecutableFormat = Console
; IDE Options = PureBasic 6.21 (Windows - x64)
; ExecutableFormat = Console
; CursorPosition = 16
; FirstLine = 12
; Folding = --
; Optimizer
; EnableAsm
; EnableThread
; CPU = 1
; EnableCompileCount = 52
; EnableBuildCount = 0
; EnableExeConstant
