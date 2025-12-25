1. Priority is on VM execution speed
2. Windows Purebasic path D:\WIP\APPS\PureBasic.610
3. Linux compilation: wsl -d Ubuntu-24.04 -e bash -c "export PUREBASIC_HOME=/home/terence/Apps/purebasic621 && cd /mnt/d/OneDrive/WIP/Sources/Intense.2020/lj2 && \$PUREBASIC_HOME/compilers/pbcompiler c2-modules-V22.pb -e lj2_linux -t -cl 2>&1" (use -t for thread-safe, -cl for console mode)
4. Windows compilation: pbcompiler --optimizer --thread /DYNAMICCPU c2-modules-V22.pb (add /CONSOLE for console mode, requires PureBasic 6.20+)
5. Always leave definitions at beginning of procedure - no exceptions
6. No procedure static variables; we use global variables which can be reset between runs
7. If there is an _*.ver file add a .1 everytime we interact (MAJ.MIN.FIX)
8. Don't touch the *.lj files unless I expressively ask you to
9. Purebasic functions and variables and constants are case insesitive
10. Compiler flow: pre-processor - scanner - AST - codegenerator - postprocessor
11. Postprocessor is crucial to properly correct JMP and CALL calls
12. Postprocessor is crucial and final step to make sure type "guessing" is not needed in the VirtualMachine
13. Backup should always be the first step while changing version be the last
14. Use powershell commands
15. gVarMeta CANNOT be used in VM code as VM needs to work independently of compiler
16. var1 + var2 is the same as var1 = var1 + var2 and va1 - var2 is the same as var1 = var1 - var2
17. LJ Language is meant to be built for speed of execution
18. Don't use intermidiate variables in VM code; use macros instead for readility
19. Create a 7z backup with version under backups\ at least 2 times a day and before any major version
20. No structure unions; we need to maintain compatibility with Spiderbasic
21. Linux GUI runs non-threaded (GTK threading causes issues); Windows GUI uses threading with timer-based queue processing
22. Command line: lj2.exe [--test|-t] [-x|--autoquit <seconds>] <filename.lj> - use --test for headless console output, -x for auto-close timer
23. Test runner: run-tests-win.ps1 runs all example tests on Windows
24. Stack size: 8MB via linker.txt for large compilation (comprehensive tests)
25. Delete executables prior to compile - always
26. Variable lookups: Use O(1) map-based functions instead of O(N) array scans:
    - FindVariableSlotByOffset(paramOffset, functionContext) for local variable lookup by stack offset
    - FindVariableSlotByName(name, functionContext) for lookup by name (tries local first, then global)
    - FindStructSlotByName(name, functionContext) for struct variable lookup (requires #C2FLAG_STRUCT)
    - When adding new codegen code, prefer these functions over For loops scanning gVarMeta
    - MapCodeElements and MapLocalByOffset provide O(1) lookups; fallback O(N) scans exist for migration
27. use the test subfolder for tests ; add scripts / move scripts here too. We should try to keep the root folder to critical files only
