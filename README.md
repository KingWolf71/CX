# CX Compiler & Virtual Machine

**Version:** 1.039.65
**Language:** PureBasic (v6.10+)
**Target:** Windows x64 / Linux x64

## Overview

CX is a high-performance compiler and virtual machine for a C-like language. The project prioritizes **VM execution speed** above all else, implementing aggressive optimizations at both compile-time and runtime.

### Language Features
- C-style syntax with semicolon-terminated statements
- Dynamic typing with type inference
- Functions with parameters and local variables
- Arrays (global and local, integer/float/string types)
- Structures with field access
- Pointers with arithmetic operations
- Lists and Maps collections
- Macros with nested expansion
- Pragmas for compile-time configuration
- Built-in assertion functions for testing
- printf() for C-style formatted output
- File I/O (fread, fwrite with append mode)
- Process execution (exec)
- JSON serialization (parse, create, export, member access)
- XML serialization (parse, create, export, node/attribute access)

> **Looking for AI integration?** See [CX+AI (CXPLUSAI)](https://github.com/KingWolf71/CXPLUSAI) -- the extended version with integrated AI code generation, RISC VM, and 659 opcodes.

## Architecture

### Compiler Pipeline

```
Source Code (.cx)
    |
[1] Preprocessor  -> Macro expansion, pragma processing
    |
[2] Scanner       -> Tokenization, lexical analysis
    |
[3] Parser        -> AST construction, syntax analysis
    |
[4] Code Generator -> Bytecode emission
    |
[5] PostProcessor -> Type inference, instruction fusion, optimizations
    |
[6] FixJMP        -> Jump address resolution, function patching
    |
Bytecode Array (arCode)
    |
[7] Virtual Machine -> Execution
```

### Key Design Principles

1. **Speed First**: VM execution performance is the top priority
2. **Type Resolution**: Types are inferred and resolved during PostProcessor phase, eliminating runtime type checks
3. **Instruction Fusion**: Multiple operations are combined into single optimized instructions
4. **Separation of Concerns**: Compile-time metadata (gVarMeta) is never accessed by VM
5. **Stack-Based VM**: Uses hybrid stack + register model for optimal performance

## Project Structure

### Core Modules

| File | Purpose |
|------|---------|
| `c2-modules-V25.pb` | Main compiler module - orchestrates compilation pipeline |
| `c2-inc-v21.pbi` | Global definitions, 508 VM opcodes, structures, constants |
| `c2-ast-v09.pbi` | Recursive descent parser, AST construction |
| `c2-scanner-v07.pbi` | Tokenizer, lexical analysis |
| `c2-codegen-v09.pbi` | AST to bytecode translation |
| `c2-codegen-emit.pbi` | EmitInt procedure |
| `c2-codegen-vars.pbi` | FetchVarOffset |
| `c2-codegen-types.pbi` | GetExprResultType, GetExprSlotOrTemp |
| `c2-codegen-rules.pbi` | Rule-based type dispatch tables |
| `c2-codegen-lookup.pbi` | O(1) variable lookup functions |
| `c2-codegen-struct.pbi` | Struct field handling |
| `c2-typeinfer-V04.pbi` | Unified type resolution |
| `c2-postprocessor-V13.pbi` | Correctness passes, instruction fusion |
| `c2-optimizer-V04.pbi` | 5-pass peephole optimization |
| `c2-vm-V19.pb` | Virtual machine core, execution loop |
| `c2-vm-commands-v17.pb` | VM instruction implementations |

### Feature Modules

| File | Purpose |
|------|---------|
| `c2-arrays-v08.pbi` | Array operations (global/local, all types) |
| `c2-builtins-v09.pbi` | Built-in function implementations |
| `c2-builtins-system-v01.pbi` | System utility builtins |
| `c2-collections-v05.pbi` | Lists and Maps |
| `c2-pointers-v07.pbi` | Pointer operations and arithmetic |
| `c2-serialize-v02.pbi` | .ocx bytecode serialization |
| `c2-ccompat-v02.pbi` | C-backend compatibility layer |

### Support Files

- `_cx.ver` - Version tracking (MAJ.MIN.FIX format)
- `DOCS/` - Detailed documentation on each compiler phase
- `Examples/` - Test programs and language demonstrations (70+)
- `tests/` - Test scripts and benchmarks

## Compiler Phases

### 1. Preprocessor
- Expands macros (supports nested macro calls)
- Processes `#define`, `#pragma`, `#include` directives
- Handles conditional compilation

### 2. Scanner
- Tokenizes source code
- Identifies keywords, operators, literals, identifiers
- Tracks line/column for error reporting

### 3. Parser (AST Builder)
- Recursive descent parser
- Builds abstract syntax tree (AST)
- Implements operator precedence

### 4. Code Generator
- Traverses AST and emits bytecode instructions
- Manages variable allocation (global vs local)
- Handles function calls, parameters, local variables
- Emits untyped generic instructions (typing happens in PostProcessor)

### 5. PostProcessor (Critical Phase)
Multiple optimization passes:
- **Type Inference**: Converts generic instructions to typed variants (INT/FLOAT/STR)
- **Instruction Fusion**: Array index/value optimization, constant folding, local/global specialization
- **Implicit Returns**: Ensures all functions have proper return instructions

### 6. FixJMP
- Resolves jump addresses after optimizations
- Patches CALL instructions with PC address, function ID, parameter/local counts

### 7. Virtual Machine
- **508 specialized opcodes** (123 builtins dispatched via generic call)
- Separate local variable arrays per stack frame
- Zero-overhead type dispatch (resolved at compile time)
- Hot/cold instruction data splitting for cache locality
- Cached pointer resolution for global variable access

## Usage

### Command Line
```bash
cx.exe program.cx               # Run with GUI
cx.exe -c program.cx            # Run in console mode (no GUI)
cx.exe -C program.cx            # Compile only to .ocx file
cx.exe -o out.ocx program.cx    # Compile with custom output name
cx.exe -a program.cx            # Output ASM listing
cx.exe --asm-debug program.cx   # Detailed ASM with FLAGS/slot info
cx.exe -x 5 program.cx          # Auto-close after 5 seconds
cx.exe compiled.ocx             # Run pre-compiled bytecode
```

### Compilation (Building the Compiler)

**Windows (ASM backend):**
```bash
pbcompiler /OPTIMIZER /THREAD /DYNAMICCPU /CONSOLE c2-modules-V25.pb /EXE cx.exe
```

**Linux:**
```bash
$PUREBASIC_HOME/compilers/pbcompiler c2-modules-V25.pb -e cx_linux -t -cl
```

### Pragma Directives

| Pragma | Values | Purpose |
|--------|--------|---------|
| `appname` | "string" | Sets console window title |
| `console` | on/off | Enables GUI console |
| `consolesize` | "WxH" | Sets console dimensions |
| `version` | - | Prints compiler version |
| `optimizecode` | on/off | Enables/disables optimizations |
| `listasm` | on/off | Shows generated bytecode |
| `asmdecimal` | on/off | Decimal line numbers in ASM |
| `decimals` | N | Float display precision |
| `floattolerance` | N | Float comparison epsilon |
| `FastPrint` | on/off | Buffered vs immediate print |
| `RunThreaded` | on/off | Execute in separate thread |
| `stackspace` | N | Stack size for values |
| `stackdepth` | N | Maximum function call depth |

## Example Program

```c
#pragma console on
#pragma appname "Array Demo"
#pragma version

array global_ints.i[10];

function fillArray() {
    array local_data.i[5];

    i = 0;
    while i < 5 {
        local_data[i] = i * 10;
        global_ints[i] = local_data[i];
        i = i + 1;
    }
}

fillArray();

i = 0;
while i < 5 {
    print("global_ints[", i, "] = ", global_ints[i]);
    i = i + 1;
}
```

## Testing

### Running Tests
```powershell
# Windows
powershell -ExecutionPolicy Bypass -File tests/run-tests-win.ps1

# Linux
./tests/run-tests-linux.sh
```

87 tests pass on Windows as of v1.039.65.

## Documentation

- `DOCS/CX_Language_Reference.html` - Language reference manual
- `DOCS/ARCHITECTURE.md` - System architecture
- `DOCS/IMPLEMENTATION_STATUS.md` - Feature status tracking
- `DOCS/1. lexical.txt` - Scanner/tokenizer
- `DOCS/2. syntax.txt` - Parser/AST builder
- `DOCS/3. AST Interpreter.txt` - AST traversal
- `DOCS/4. code generator.txt` - Bytecode emission
- `DOCS/5. vm.txt` - Virtual machine architecture

## License

This project is licensed under the [GNU General Public License v3.0](LICENSE).
Based on Rosetta Code compiler examples.

## Support

If you find CX useful, consider supporting development:

[![Buy Me a Coffee](https://img.shields.io/badge/Buy%20Me%20a%20Coffee-ffdd00?style=for-the-badge&logo=buy-me-a-coffee&logoColor=black)](https://buymeacoffee.com/kingwolf71)

---

**Author:** Terence Agius
**Date:** 2025-2026
**Platform:** PureBasic 6.10+ (Windows x64 / Linux x64)
**Repository:** https://github.com/KingWolf71/CX