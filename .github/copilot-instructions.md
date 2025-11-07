# Copilot Instructions for the LJ2 Project

Welcome to the LJ2 codebase! This document provides essential guidelines for AI coding agents to be productive in this project. Please follow these instructions carefully to align with the project's structure and conventions.

## Project Overview

The LJ2 project appears to be a language or virtual machine implementation. Key components include:

- **Source Files**: `.pb` files in the root directory and `Backups/` folder, likely representing different versions of the compiler, modules, and virtual machine.
- **Documentation**: The `DOCS/` folder contains detailed notes on lexical analysis, syntax, AST interpretation, code generation, and the virtual machine.
- **Examples**: The `Examples/` folder provides `.lj` files demonstrating various language features, such as loops, conditionals, macros, and functions.
- **Tests**: The `tests/` folder includes `.pb` files for testing the language or virtual machine.

## Developer Workflows

### Compilation
- **IDE-Only Compilation**: The `CLAUDE.md` file specifies that the command-line compiler does not work. Use the IDE for manual compilation.

### Testing
- Tests are located in the `tests/` folder. Ensure that any changes to the compiler or virtual machine are validated against these tests.

### Debugging
- Refer to the `DOCS/` folder for insights into the architecture and debugging strategies for the language and virtual machine.

## Project-Specific Conventions

- **File Naming**: Version numbers are embedded in filenames (e.g., `c2-modules-V09.pb`), indicating iterative development.
- **Backup Files**: The `Backups/` folder contains older versions of key files. Avoid modifying these files directly.
- **Examples**: Use the `.lj` files in the `Examples/` folder to understand and demonstrate language features.

## Key Files and Directories

- **`DOCS/`**: Start here for a deep dive into the project's architecture.
- **`Examples/`**: Review these files to understand the language's capabilities.
- **`tests/`**: Use these files to validate changes.
- **`Backups/`**: Reference only for historical context.

## Notes for AI Agents

- Avoid suggesting command-line compilation workflows.
- When editing `.pb` files, ensure compatibility with existing tests.
- Maintain the versioning convention in filenames.
- Use examples to validate new language features or changes.

By following these guidelines, you can contribute effectively to the LJ2 project. If any instructions are unclear, please ask for clarification.