# CX Compiler & Virtual Machine

**Version:** 1.039.62
**Language:** PureBasic (v6.10+)
**Target:** Windows x64 / Linux x64

## Overview

CX is a high-performance compiler and virtual machine for a simplified C-like language. The project prioritizes **VM execution speed** above all else, implementing aggressive optimizations at both compile-time and runtime.

The language features:
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

## Architecture

### Compiler Pipeline

The CX compiler follows a multi-stage pipeline:

```
Source Code (.cx)
    ↓
[1] Preprocessor  → Macro expansion, pragma processing
    ↓
[2] Scanner       → Tokenization, lexical analysis
    ↓
[3] Parser        → AST construction, syntax analysis
    ↓
[4] Code Generator → Bytecode emission
    ↓
[5] PostProcessor → Type inference, instruction fusion, optimizations
    ↓
[6] FixJMP        → Jump address resolution, function patching
    ↓
Bytecode Array (arCode)
    ↓
[7] Virtual Machine → Execution
```

### Key Design Principles

1. **Speed First**: VM execution performance is the top priority
2. **Type Resolution**: Types are inferred and resolved during PostProcessor phase, eliminating runtime type checks
3. **Instruction Fusion**: Multiple operations are combined into single optimized instructions
4. **Separation of Concerns**: Compile-time metadata (gVarMeta) is never accessed by VM
5. **Stack-Based VM**: Uses hybrid stack + register model for optimal performance

## Project Structure

### Core Modules

| File | Version | Purpose |
|------|---------|---------|
| `c2-modules-V25.pb` | V25 | Main compiler module - orchestrates compilation pipeline |
| `c2-inc-v21.pbi` | v21 | Global definitions, 505 opcodes, structures, constants |
| `c2-ast-v09.pbi` | v09 | Recursive descent parser, AST construction |
| `c2-codegen-v09.pbi` | v09 | AST to bytecode translation |
| `c2-typeinfer-V04.pbi` | V04 | Unified type resolution |
| `c2-postprocessor-V13.pbi` | V13 | Correctness passes (implicit returns, collections) |
| `c2-optimizer-V04.pbi` | V04 | 5-pass peephole optimization |
| `c2-vm-V19.pb` | V19 | Virtual machine core, execution loop |
| `c2-vm-commands-v17.pb` | v17 | VM instruction implementations (505 opcodes) |

### Support Files

- `_cx.ver` - Version tracking (MAJ.MIN.FIX format)
- `CLAUDE.md` - Development guidelines for AI assistance
- `DOCS/` - Detailed documentation on each compiler phase
- `Examples/` - Test programs and language demonstrations
- `BACKUP/` - Previous module versions

## Compiler Phases Explained

### 1. Preprocessor
- Expands macros (supports nested macro calls)
- Processes `#define`, `#pragma`, `#include` directives
- Handles conditional compilation
- Outputs preprocessed token stream

### 2. Scanner
- Tokenizes source code
- Identifies keywords, operators, literals, identifiers
- Tracks line/column for error reporting
- Produces TOKEN() list

### 3. Parser (AST Builder)
- Recursive descent parser
- Builds abstract syntax tree (AST)
- Implements operator precedence
- Validates syntax structure

### 4. Code Generator
- Traverses AST and emits bytecode instructions
- Manages variable allocation (global vs local)
- Handles function calls, parameters, local variables
- Tracks local array metadata in `gFuncLocalArraySlots`
- Emits untyped generic instructions (typing happens in PostProcessor)

### 5. PostProcessor ⭐ **Critical Phase**
The PostProcessor performs multiple optimization passes:

#### Pass 1: Type Inference
- Analyzes variable usage patterns
- Converts generic instructions to typed variants (INT/FLOAT/STR)
- Example: `PUSH` → `PUSHI` / `PUSHF` / `PUSHS`

#### Pass 2: Instruction Fusion
- **Array Index Optimization**: `PUSH index + ARRAYFETCH` → `ARRAYFETCH_OPT` (index in ndx field)
- **Array Value Optimization**: `PUSH value + ARRAYSTORE` → `ARRAYSTORE_OPT` (value in n field)
- **Constant Folding**: `PUSH 2 + PUSH 3 + ADD` → `PUSH 5`
- **Local/Global Specialization**: Creates 8 variants per operation (GLOBAL/LOCAL × OPT/STACK × OPT/STACK)

#### Pass 3: Implicit Returns
- Ensures all functions have proper return instructions
- Prevents fall-through execution bugs

### 6. FixJMP
- Resolves jump addresses after optimizations
- Patches CALL instructions with:
  - `i` field = PC address (for jumping)
  - `flags` field = Function ID (for local array metadata lookup)
  - `j` field = Parameter count
  - `n` field = Local variable count
  - `ndx` field = Local array count
- Converts instruction list (llObjects) to array (arCode)

### 7. Virtual Machine

The VM uses a stack-based architecture with specialized instruction variants for performance:

**Key Features:**
- 505 specialized opcodes (vs ~20 generic)
- Separate local variable arrays per stack frame (LocalInt, LocalFloat, LocalString)
- Local array support with proper scoping
- Zero-overhead type dispatch (resolved at compile time)
- Optimized array operations (direct register access when possible)

**Execution Model:**
```
gStack[depth]
  ├── LocalInt[]       - Local integer variables
  ├── LocalFloat[]     - Local float variables
  ├── LocalString[]    - Local string variables
  ├── LocalArrays[]    - Local array instances
  ├── pc               - Return address
  └── sp               - Saved stack pointer
```

## Recent Improvements

### v1.039.62 - VM Cache & Pointer Optimization
- **Instruction Cache Splitting**: Split `stCodeIns` into HOT (opcode, params) and COLD (`stCodeData` for anchors) — better CPU cache locality
- **Cached Pointer Resolution**: `ResolveCodePointers()` pre-resolves `*arCodeCache` pointers for global PUSH/STORE and CALL opcodes — eliminates double indirection at runtime
- **needsCleanup Fast Path**: CALL opcodes skip `CopyStructure` for int/float-only functions (use direct field copy instead)
- **Opcode field density**: `code` field changed from `.l` to `.w` for tighter instruction packing

### v1.039.61 - Struct Field Cache
- **O(1) Struct Field Lookup**: `mapStructFieldCache` caches field type and byte offset for nested struct chains (up to 3 levels deep)
- Eliminates repeated struct definition walks during codegen type resolution

### v1.039.60 - Strength Reduction & Cleanup Optimization
- **Multiply → Shift**: Optimizer converts `x * power-of-2` to `x << shift` at compile time
- **Identity/Zero Elimination**: `x * 0 → 0`, `x * 1 → x`, `x + 0 → x` patterns removed at compile time
- **needsCleanup Flag**: `stFuncTemplate\needsCleanup` tracks whether function has string/array/list/map locals

### v1.039.59 - 2D Array Variable-Index Store Fix
- Fixed `arr[var][var] = expr` inside functions silently storing 0
- Variable-index path now pushes value to eval stack first, avoiding LOPT type inferrer gap
- 83 tests pass

### v1.039.58 - String Builtin Correctness Fix
- **`\i` Length Sync**: All string builtins (`left`, `right`, `mid`, `trim`, `lcase`, `ucase`, `chr`, `hex`, `bin`, `replacestring`, `insertstring`, `removestring`, `space`, `lset`, `rset`, `capitalize`, `md5`/`sha*`/`base64`, `jsonstring`) now update cached `\i` length after writing `\ss`
- Previously, inline expressions like `len(left(s, n))` or `assertEqual(left(s,n), ...)` could return stale string lengths
- 81 tests pass (Windows)

### v1.039.57 - JSON & XML Builtins
- **JSON Write Builtins**: `jsonaddnum(h, key, value)`, `jsonaddbool(h, key, bool)` for numeric/boolean JSON members
- **14 XML Builtins**: `xmlcreate`/`xmlfree`/`xmlparse`/`xmlexport`, `xmlroot`/`xmladdnode`/`xmlchild`/`xmlnext`/`xmlparent`, `xmlsettext`/`xmlsetattr`/`xmltext`/`xmlattr`/`xmlname`/`xmltype`
- **jsoncreate fix**: `SetJSONObject` called after `CreateJSON` so `jsonadd`/`jsonaddnum`/`jsonaddbool` work correctly (root was NULL type)
- **xmlcreate fix**: Bootstrap with `ParseXML`+`DeleteXMLNode` to get a valid document node
- New test `tests/test_jsonxml.cx` with 24 assertions (all pass)
- New example `Examples/126 json xml io.cx`

### v1.039.56 - File I/O & Process Execution
- **`fread(filename)`**: Read entire file as string
- **`fwrite(filename, content [, mode])`**: Write/append file ("w"/"a" mode), returns 1 on success
- **`exec(command [, background])`**: Run external command, returns exit code
- **`assert(condition)`**: Assert non-zero condition with pass/fail output
- Fixed `vm_PushString` cross-populate bug affecting string returns across stack frames

### v1.039.45 - System/Utility Builtins
- **Date/Time**: `date()`, `time()`, `year()`, `month()`, `day()`, `hour()`, `minute()`, `second()`
- **Misc**: `delay(ms)`, `elapsed()`, `randomseed(n)`, `getenv(name)`

### v1.037.x - ASM Naming & Bug Fixes
- **Normalized ASM Names**: All long opcode display names shortened (AF_I_G_O instead of ARRAYFETCH_INT_GLOBAL_OPT)
- **Struct Declaration Fix**: `p1.Point;` syntax now properly allocates struct memory
- **Cross-Platform**: All tests pass on Windows and Linux

### v1.036.x - Struct Arrays & printf()
- **Array of Structs**: `array points.Point[10];` with `points[i]\x` field access
- **printf() Built-in**: C-style formatted output with %d, %f, %s, %.Nf specifiers
- **String Length Caching**: O(1) length access via cached `\i` field

### v1.035.x - Optimizer Enhancements
- **Rule-Based Optimizer**: Lookup tables for peephole patterns
- **MOV Fusion**: FETCH+STORE → single MOV instruction (all locality combinations)
- **DUP Optimization**: FETCH x + FETCH x → FETCH x + DUP for squared patterns
- **Constant Folding**: PUSH+NEGATE → negative constant at compile time

### v1.034.x - Large Function Support
- **8192 Max Functions**: Removed 512 function limit
- **O(1) Variable Lookups**: MapCodeElements for fast variable resolution
- **Unified Code Element Map**: Full metadata tracking with expression chains

## Usage

### Compiling and Running Programs

1. **Open in PureBasic IDE**: Load `c2-modules-V25.pb`
2. **Select Target**: Choose your .d file at the bottom of the module
3. **Compile**: Press F5 to compile and run
4. **Output**: Results appear in the VM console window

**Command Line:**
```bash
cx.exe program.cx               # Run with GUI
cx.exe -c program.cx            # Run in console mode (no GUI)
cx.exe -C program.cx            # Compile only to .ocx file
cx.exe -a program.cx            # Output ASM listing
cx.exe -x 5 program.cx          # Auto-close after 5 seconds
```

### Pragma Directives

| Pragma | Values | Purpose |
|--------|--------|---------|
| `appname` | "string" | Sets console window title |
| `console` | on/off | Enables GUI console |
| `consolesize` | "WxH" | Sets console dimensions |
| `version` | - | Prints compiler version |
| `optimizecode` | on/off | Enables/disables optimizations |
| `ListASM` | on/off | Shows generated bytecode |
| `decimals` | N | Float display precision |
| `floattolerance` | N | Float comparison epsilon |
| `FastPrint` | on/off | Buffered vs immediate print |
| `RunThreaded` | on/off | Execute in separate thread |
| `stackspace` | N | Stack size for values |
| `stackdepth` | N | Maximum function call depth |

## Performance Characteristics

### Optimization Examples

**Before Optimization:**
```
PUSH [const_2]      ; Push index
PUSH [const_100]    ; Push value
ARRAYSTORE          ; Store with stack operations
```

**After Optimization:**
```
ARRAYSTORE_INT_GLOBAL_OPT_OPT  ; Single instruction, index=2, value=100
```

This reduces 3 instructions to 1, eliminates 2 stack operations, and resolves types at compile time.

### Instruction Variants

Array operations have 8 specialized variants:
- 2 storage types: GLOBAL / LOCAL
- 2 index sources: OPTIMIZED (in ndx field) / STACK
- 2 value sources: OPTIMIZED (in n field) / STACK

Example: `ARRAYSTORE_INT_LOCAL_OPT_STACK`
- Type: Integer
- Storage: Local array
- Index: Optimized (constant or variable in ndx field)
- Value: From stack

## Development Guidelines

See `CLAUDE.md` for detailed development instructions, including:
- Code style conventions
- Variable naming rules
- PureBasic-specific gotchas
- Testing procedures
- Version management

## Testing

### Test Suite
- `Examples/*.cx` - 70+ example programs covering all language features
- `tests/test_*.cx` - Targeted unit tests (file I/O, JSON/XML, sprintf, 2D arrays, etc.)
- **87 tests pass** on Windows as of v1.039.62

### Running Tests
```powershell
# Windows: run all tests
powershell -ExecutionPolicy Bypass -File tests/run-tests-win.ps1
```

### Example Program

```c
// Example: Array operations with functions
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

## Version History

### v1.039.x (Current)
- ✅ Project renamed from D-Plus to CX
- ✅ Source extension changed from .d to .cx
- ✅ Compiled bytecode extension .ocx
- ✅ Serialization support for compiled programs
- ✅ File I/O builtins: fread, fwrite (append mode), exec (v1.039.56)
- ✅ assert() builtin (v1.039.56)
- ✅ JSON builtins: jsoncreate/free/parse/export, jsonadd/addnum/addbool, jsonmember/string/number/bool (v1.039.57)
- ✅ XML builtins: xmlcreate/free/parse/export, xmlroot/addnode/child/next/parent, xmlsettext/setattr/text/attr/name/type (v1.039.57)
- ✅ String builtin `\i` length sync fix — `len(left(s,n))` now correct (v1.039.58)
- ✅ 2D array variable-index store fix inside functions (v1.039.59)
- ✅ Strength reduction: `x * power-of-2` → `x << shift` (v1.039.60)
- ✅ Struct field cache for O(1) nested field lookups (v1.039.61)
- ✅ VM cache optimization: split HOT/COLD instruction data, cached pointer resolution (v1.039.62)
- ✅ 87 tests pass

### v1.037.x
- ✅ Normalized ASM opcode names for readable output
- ✅ Struct declaration bug fix
- ✅ Cross-platform testing (Windows + Linux)

### v1.036.x
- ✅ Array of structs implementation
- ✅ printf() C-style formatted output
- ✅ String length caching

### v1.035.x
- ✅ Rule-based peephole optimizer
- ✅ MOV fusion optimization
- ✅ DUP and negate constant folding

### v1.034.x
- ✅ 8192 max functions support
- ✅ O(1) variable lookups
- ✅ Unified code element map

## Documentation

For detailed information on each phase:
- `DOCS/1. lexical.txt` - Scanner/tokenizer
- `DOCS/2. syntax.txt` - Parser/AST builder
- `DOCS/3. AST Interpreter.txt` - AST traversal
- `DOCS/4. code generator.txt` - Bytecode emission
- `DOCS/5. vm.txt` - Virtual machine architecture

## License

Distribute and use freely.
Based on Rosetta Code compiler examples.

---

**Author:** Kingwolf71
**Date:** March 2026
**Platform:** PureBasic 6.10+ (Windows x64 / Linux x64)
**Repository:** https://github.com/KingWolf71/CX
