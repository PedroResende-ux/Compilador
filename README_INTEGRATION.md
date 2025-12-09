# Compilador Ada - Integrated Version

## Overview

This is an integrated compiler for a subset of the Ada programming language. It combines:
- **Symbol Table Management** with semantic analysis
- **Three-Address Code (TAC)** generation
- **MIPS Assembly** code generation

## Features

### 1. Lexical Analysis
- Tokenization of Ada source code
- Support for keywords, operators, literals, and identifiers

### 2. Syntactic Analysis
- Parsing of Ada programs
- Construction of Abstract Syntax Tree (AST)
- Support for:
  - Variable declarations with types (Integer, Boolean)
  - Control structures (if-then-else, while loops)
  - Arithmetic and logical expressions
  - I/O operations (Put_Line, Get_Line)

### 3. Semantic Analysis
- **Symbol Table**: Tracks variable declarations with scope management
- **Type Checking**: Validates variable types
- **Error Detection**:
  - Undeclared variables
  - Variable redeclarations in the same scope
- **Scope Management**: Stack-based scopes for nested blocks

### 4. Intermediate Code Generation (TAC)
- Three-Address Code intermediate representation
- Operations: Assign, BinOp, UnOp, Goto, Ifz, Label
- Automatic temporary variable generation
- Label generation for control flow

### 5. Target Code Generation (MIPS)
- MIPS assembly code generation from TAC
- Register allocation:
  - $s0-$s7 for variables
  - $t0-$t9 for temporaries
- String literal handling
- System calls for I/O

## Installation

### Prerequisites
- GHC (Glasgow Haskell Compiler) 9.x or later
- Alex (lexer generator) - optional, pre-generated files included
- Happy (parser generator) - optional, pre-generated files included

### Build

```bash
# Using Make (requires Alex and Happy)
make clean
make

# Or directly with GHC (uses pre-generated Lexer.hs and Parser.hs)
ghc -dynamic --make Main.hs -o compilador
```

## Usage

```bash
# Compile an Ada program
./compilador program.ada

# This will:
# 1. Display the Abstract Syntax Tree
# 2. Perform semantic analysis and show any errors/warnings
# 3. Display the symbol table
# 4. Generate and display Three-Address Code (TAC)
# 5. Generate and display MIPS assembly code
# 6. Write MIPS code to program.asm
```

## Example

### Input (test.ada):
```ada
procedure Main is
  x : Integer;
  y : Integer;
begin
  x := 10;
  y := x + 20;
  Put_Line("Result:");
  Put_Line(y)
end Main;
```

### Output:
The compiler will generate:
1. **AST**: Visual representation of the program structure
2. **Semantic Analysis**: Reports any errors or confirms no errors
3. **Symbol Table**: Shows declared variables and their types
4. **TAC**: Intermediate code representation
5. **MIPS Code**: Final assembly code (written to `test.asm`)

## Testing

Run the test suite:
```bash
./run_tests.sh
```

Tests include:
- Variable declarations
- Semantic error detection (undeclared variables, redeclarations)
- Nested scopes
- Arithmetic operations
- Control flow structures

## Language Subset

### Supported Constructs

**Declarations:**
```ada
x : Integer;
flag : Boolean;
```

**Statements:**
- Assignment: `x := 10 + 5`
- Conditional: `if x > 0 then ... else ... end if`
- While loop: `while x < 10 loop ... end loop`
- Block: `begin ... end`
- Output: `Put_Line("text")` or `Put_Line(variable)`

**Expressions:**
- Arithmetic: `+`, `-`, `*`, `/`, `mod`, unary `-`
- Comparison: `=`, `/=`, `<`, `<=`, `>`, `>=`
- Logical: `and`, `or`, `not`
- Literals: integers, strings, `True`, `False`
- Variables
- Input: `Get_Line`

## Architecture

```
Source Code (.ada)
    ↓
Lexical Analysis (Lexer.x → Lexer.hs)
    ↓
Syntactic Analysis (Parser.y → Parser.hs)
    ↓
AST (AST.hs)
    ↓
Semantic Analysis (Semantic.hs)
    ↓
TAC Generation (TAC.hs)
    ↓
MIPS Generation (MIPS.hs)
    ↓
Assembly Code (.asm)
```

## Documentation

- `INTEGRATION.md`: Detailed integration documentation
- `README.md`: General project information (this file)

## Integration Notes

This version integrates features from:
- **PR #2**: Symbol table and semantic analysis
- **PR #3**: TAC and MIPS code generation

See `INTEGRATION.md` for detailed integration strategy and conflict resolution.

## Authors

Integrated by GitHub Copilot for the Compilador project.

## License

Educational project for DCC FCUP - Compilers course.
