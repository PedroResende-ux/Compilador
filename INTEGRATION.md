# Integration Documentation

## Overview

This document describes the integration of two pull requests into a unified compiler implementation:

- **PR #2**: Symbol Table with Semantic Analysis
- **PR #3**: Three-Address Code (TAC) and MIPS Code Generation

## Integration Strategy

The integration was performed by merging changes from both PRs into the `integrate-pull-requests` branch. The key challenge was resolving conflicts between the two PRs while preserving the functionality of both.

## Key Changes

### 1. AST.hs
**Integration approach**: Combined symbol table structures from PR #2 with TAC definitions from PR #3.

**Changes made**:
- Added symbol table types: `Type`, `SymbolInfo`, `SymbolTable`, `Scope`
- Added declarations support: `Decl` type
- Modified `Program` to include declarations: `Program [Decl] [Stmt]`
- Added symbol table operations: `emptySymbolTable`, `enterScope`, `exitScope`, `insertSymbol`, `lookupSymbol`, `isDeclared`
- Preserved TAC definitions from PR #3

### 2. Lexer.x
**Source**: PR #2

**Changes made**:
- Added type keywords: `Integer`, `Boolean`
- Added token types: `TokenIntegerType`, `TokenBooleanType`
- Updated `getTokenPos` to handle new tokens

### 3. Parser.y
**Source**: PR #2

**Changes made**:
- Added grammar rules for declarations: `DeclList`, `Decl`, `Type`
- Modified `Program` production to support optional declarations
- Added tokens for type keywords: `inttype`, `booltype`

### 4. Semantic.hs
**Source**: PR #2 (unchanged)

**Purpose**: Performs semantic analysis including:
- Variable declaration tracking
- Redeclaration detection
- Undeclared variable usage detection
- Scope management

### 5. TAC.hs
**Source**: PR #3 with modifications

**Changes made**:
- Updated `generateTAC` to work with new `Program` structure that includes declarations
- Changed pattern match from `Program stmts` to `Program _ stmts` (ignoring declarations)

### 6. MIPS.hs
**Source**: PR #3 (unchanged)

**Purpose**: Generates MIPS assembly code from TAC

### 7. Main.hs
**Integration approach**: Combined both compilation pipelines

**Compilation pipeline**:
1. Lexical analysis (tokenization)
2. Syntactic analysis (parsing)
3. **Semantic analysis** (from PR #2)
   - Check for semantic errors
   - Exit if errors found
   - Display warnings if any
   - Display symbol table
4. **TAC generation** (from PR #3)
5. **MIPS code generation** (from PR #3)
6. Write MIPS code to file

**Key features**:
- Updated `printTree` to display declarations
- Added `printDecl` function
- Imports: `Semantic`, `TAC`, `MIPS`, `System.Exit`
- Error handling: exits with failure code on semantic errors

### 8. Makefile
**Changes made**:
- Added new module dependencies: `SEMANTIC_SRC`, `TAC_SRC`, `MIPS_SRC`
- Updated compilation rule to include all modules

### 9. .gitignore
**Changes made**:
- Added more build artifact patterns:
  - `Parser.info`
  - `*.dyn_hi`, `*.dyn_o`
  - `compilador` executable
  - `*.asm` generated files

## Functionality

### Symbol Table (PR #2)
- **Variable declarations**: Support for `Integer` and `Boolean` types
- **Scope management**: Stack-based scopes with proper nesting
- **Semantic validation**:
  - Detects undeclared variables
  - Detects variable redeclarations in the same scope
  - Uses efficient `Map.Map` for O(log n) lookups

### Code Generation (PR #3)
- **Three-Address Code**: Intermediate representation with:
  - Basic operations: `Assign`, `BinOp`, `UnOp`
  - Control flow: `Goto`, `Ifz`, `Label`
  - Automatic temporary variable and label generation
- **MIPS Assembly**: Target code generation with:
  - Register allocation ($s0-$s7 for variables, $t0-$t9 for temporaries)
  - String literal handling
  - System calls for I/O
  - Standard prologue and epilogue

## Test Results

All tests pass successfully:

### Semantic Analysis Tests
- ✓ Variable declarations
- ✓ Undeclared variable detection (error correctly reported)
- ✓ Redeclaration detection (error correctly reported)
- ✓ Nested scope handling

### Code Generation Tests
- ✓ Arithmetic operations with TAC and MIPS generation
- ✓ Comprehensive test with control flow
- ✓ Complex nested structures

## Usage

```bash
# Compile the compiler
make clean
make

# Or directly with ghc
ghc -dynamic --make Main.hs -o compilador

# Run the compiler
./compilador test.ada

# Run test suite
./run_tests.sh
```

## Example

Input program:
```ada
procedure Main is
  x : Integer;
  y : Integer;
begin
  x := 10;
  y := 20;
  Put_Line("Test complete")
end Main;
```

Output:
- Abstract Syntax Tree (AST)
- Semantic Analysis results
- Symbol Table
- Three-Address Code (TAC)
- MIPS Assembly Code
- Generated `.asm` file

## Conflict Resolution

The main conflicts between PR #2 and PR #3 were:

1. **AST.hs**: Both PRs modified `Program` structure
   - **Resolution**: Combined both - declarations from PR #2 + TAC types from both PRs
   
2. **Main.hs**: Different compilation pipelines
   - **Resolution**: Chained both pipelines: semantic analysis → TAC → MIPS
   
3. **Parser.y**: PR #2 added declarations, PR #3 had no parser changes
   - **Resolution**: Used PR #2's parser with declaration support

4. **Lexer.x**: PR #2 added type keywords, PR #3 had no lexer changes
   - **Resolution**: Used PR #2's lexer with type keywords

## Conclusion

The integration successfully combines:
- Symbol table management and semantic validation from PR #2
- Intermediate code generation and target code generation from PR #3

The unified compiler now performs complete compilation with semantic checks before generating executable code.
