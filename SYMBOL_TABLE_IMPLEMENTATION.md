# Symbol Table Implementation Documentation

## Overview

This document describes the symbol table implementation for the Ada subset compiler. The symbol table manages variable declarations, types, and scopes, performing semantic validation during compilation.

## Implementation Details

### 1. Data Structures (AST.hs)

#### Type System
```haskell
data Type = IntegerType | BooleanType
```
Currently supports two primitive types.

#### Symbol Information
```haskell
data SymbolInfo = SymbolInfo 
  { symbolName :: String
  , symbolType :: Type
  , scopeLevel :: Int
  }
```
Stores complete information about each declared symbol.

#### Symbol Table
```haskell
data SymbolTable = SymbolTable
  { scopes :: [Scope]          -- Stack of scopes (head = current)
  , currentLevel :: Int         -- Current nesting level
  }

type Scope = Map.Map String SymbolInfo
```
Uses a stack-based approach where:
- Each scope is a Map for O(log n) lookups
- New scopes are pushed on begin blocks
- Scopes are popped on end blocks
- Global scope is at the bottom (level 0)

### 2. Symbol Table Operations (AST.hs)

#### emptySymbolTable
Creates initial table with one global scope at level 0.

#### enterScope
Pushes a new empty scope onto the stack and increments level.

#### exitScope
Pops current scope from stack and decrements level.
Prevents exiting global scope with explicit error.

#### insertSymbol
Adds symbol to current scope.
- Returns `Nothing` if symbol exists (redeclaration)
- Returns `Just newTable` on success

#### lookupSymbol
Searches for symbol starting from current scope up to global.
Returns `Maybe SymbolInfo`.

#### isDeclared
Convenience function - returns True if symbol exists in any scope.

### 3. Lexer Extensions (Lexer.x)

Added tokens:
- `Integer` → `TokenIntegerType`
- `Boolean` → `TokenBooleanType`

### 4. Parser Extensions (Parser.y)

#### Grammar Changes
```
Program : procedure id is DeclList begin StmtList end id ';'
        | procedure id is begin StmtList end id ';'

DeclList : Decl
         | Decl DeclList

Decl : id ':' Type ';'

Type : inttype
     | booltype
```

#### Features
- Optional declaration section (backward compatible)
- Declarations must appear before begin
- Each declaration: `identifier : Type ;`

### 5. Semantic Analysis (Semantic.hs)

#### analyzeProgram
Main entry point:
1. Process all declarations
2. Check all statements
3. Return errors, warnings, and final symbol table

#### processDeclarations
- Inserts each declaration into symbol table
- Detects redeclarations in same scope
- Accumulates error messages

#### checkStatements / checkStmt
Validates variable usage:
- Assignments: checks if variable declared
- Expressions: recursively checks all variable references
- Blocks: enters new scope, checks statements, exits scope
- Control flow: checks conditions and bodies

#### checkExpr
Recursively validates all variable references in expressions.

### 6. Integration (Main.hs)

Compilation pipeline:
1. Lexical analysis (alexScanTokens)
2. Syntax analysis (parse)
3. **Semantic analysis (analyzeProgram)** ← NEW
4. Display results
5. Exit with error if semantic errors found

## Usage Examples

### Valid Program
```ada
procedure Main is
  x : Integer;
  y : Integer;
begin
  x := 10;
  y := x + 5;
  Put_Line("Done")
end Main;
```
Result: ✓ No semantic errors

### Undeclared Variable
```ada
procedure Main is
  x : Integer;
begin
  x := 10;
  z := 5  -- ERROR: z not declared
end Main;
```
Result: ✗ Error: Variable 'z' used but not declared

### Redeclaration
```ada
procedure Main is
  x : Integer;
  x : Integer;  -- ERROR: duplicate
begin
  x := 10
end Main;
```
Result: ✗ Error: Variable 'x' already declared in this scope

### Nested Scopes
```ada
procedure Main is
  x : Integer;
begin
  x := 10;
  begin
    x := 20;  -- OK: references outer x
    Put_Line("Inner block")
  end;
  Put_Line("Outer block")
end Main;
```
Result: ✓ No semantic errors
(Variables from outer scopes are visible in inner scopes)

## Testing

### Test Files
1. **test.ada**: Main test with all features
2. **test_declarations.ada**: Basic declaration testing
3. **test_undeclared.ada**: Undeclared variable detection
4. **test_redeclaration.ada**: Duplicate declaration detection
5. **test_nested_scope.ada**: Nested block scopes
6. **test_comprehensive.ada**: All features combined

### Running Tests
```bash
make test                           # Run main test
./compilador test_undeclared.ada   # Test specific file
```

## Error Messages

All semantic errors include:
- Error type (redeclaration vs undeclared)
- Variable name
- Context (which scope)

Example:
```
Error: Variable 'x' already declared in this scope
Error: Variable 'z' used but not declared
```

## Limitations and Future Enhancements

### Current Limitations
1. No type checking (can assign Integer to Boolean in grammar)
2. Variables cannot be shadowed in nested scopes
3. No constant declarations
4. No procedure/function declarations
5. No array or record types

### Possible Enhancements
1. Add type checking in expressions
2. Allow variable shadowing with warnings
3. Support const declarations
4. Add procedure symbol table entries
5. Extend type system (arrays, records, enumerations)
6. Add scope-specific error messages
7. Track variable usage (unused variable warnings)

## Design Decisions

### Why Map instead of List?
- O(log n) lookup vs O(n)
- Better performance for programs with many variables

### Why Stack of Scopes?
- Natural representation of nested blocks
- Easy scope entry/exit
- Automatic scope search from inner to outer

### Why Separate Semantic Module?
- Clean separation of concerns
- Parser focuses on syntax only
- Easier to extend semantic analysis
- Better error messages

### Why Optional Declarations?
- Backward compatibility with existing test files
- Gradual migration path
- Still enforces semantic validation

## Summary

The symbol table implementation provides:
- ✓ Variable declaration tracking
- ✓ Type information storage
- ✓ Nested scope management
- ✓ Redeclaration detection
- ✓ Undeclared variable detection
- ✓ Clean error reporting
- ✓ Backward compatibility
- ✓ Extensible design

All required features from the problem statement have been successfully implemented and tested.
