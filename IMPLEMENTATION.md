# Implementation Summary: TAC and MIPS Code Generation

## Overview
This implementation adds Three-Address Code (TAC) generation and MIPS assembly code generation to the existing Ada compiler.

## Architecture

```
Ada Source Code
    ↓
[Lexer (Alex)] → Tokens
    ↓
[Parser (Happy)] → AST
    ↓
[TAC Generator] → Three-Address Code
    ↓
[MIPS Generator] → MIPS Assembly (.asm)
```

## Modules Implemented

### 1. TAC.hs - Three-Address Code Generator
**Purpose**: Converts AST to intermediate Three-Address Code representation

**Key Features**:
- Automatic temporary variable generation (`t0`, `t1`, `t2`, ...)
- Label generation for control flow (`L0`, `L1`, `L2`, ...)
- Support for all expression types (arithmetic, logical, comparison)
- Statement handling (assignment, if-else, while, blocks)

**Example**:
```ada
x := a + b * c;
```
Generates:
```
t0 = b * c
t1 = a + t0
x = t1
```

### 2. MIPS.hs - MIPS Assembly Generator
**Purpose**: Translates TAC to MIPS assembly code

**Key Features**:
- Register allocation (variables → $s registers, temporaries → $t registers)
- Optimized instruction selection (uses immediate instructions when possible)
- String literal handling (.data section)
- Syscall generation for I/O operations
- Proper program structure (.data, .text, main, exit)

**Example**:
```
t0 = b * c
```
Generates:
```
mul $t0, $s1, $s2
```

### 3. Main.hs Updates
**Changes**:
- Added TAC and MIPS module imports
- Extended main function to call TAC generator
- Added MIPS code generation from TAC
- Writes generated MIPS code to .asm files
- Displays all three representations (AST, TAC, MIPS)

## TAC Instruction Set

The TAC implementation supports:

1. **Assignments**:
   - Direct: `x = y`
   - Binary operations: `x = y op z`
   - Unary operations: `x = op y`

2. **Operations**:
   - Arithmetic: Add, Sub, Mul, Div, Mod
   - Logical: And, Or, Not
   - Comparison: Eq, Neq, Lt, Lte, Gt, Gte
   - Special: Print, GetLine

3. **Control Flow**:
   - Labels: `L0:`
   - Unconditional jump: `goto L0`
   - Conditional jump: `ifz x goto L0` (jump if zero)

## MIPS Instruction Mapping

| TAC Operation | MIPS Instructions |
|--------------|-------------------|
| `t = a + b` | `add $t0, $s0, $s1` or `addi $t0, $s0, imm` |
| `t = a - b` | `sub $t0, $s0, $s1` or `addi $t0, $s0, -imm` |
| `t = a * b` | `mul $t0, $s0, $s1` |
| `t = a / b` | `div $s0, $s1; mflo $t0` |
| `t = a mod b` | `div $s0, $s1; mfhi $t0` |
| `t = a < b` | `slt $t0, $s0, $s1` or `slti $t0, $s0, imm` |
| `t = a > b` | `sgt $t0, $s0, $s1` or computed from `slti` |
| `goto L` | `j L` |
| `ifz t goto L` | `beqz $t0, L` |
| `L:` | `L:` |

## Register Allocation Strategy

1. **Variables**: Allocated to `$s0-$s7` (saved registers)
2. **Temporaries**: Allocated to `$t0-$t9` (temporary registers)
3. **I/O**: Uses `$a0` for syscall arguments, `$v0` for syscall codes

## Test Cases

1. **test.ada**: Basic functionality
2. **test_arithmetic.ada**: Arithmetic operations and precedence
3. **test_conditionals.ada**: If-then and if-then-else statements
4. **test_loop.ada**: While loops with accumulation
5. **test_nested.ada**: Nested blocks and control structures
6. **test_comprehensive.ada**: Complete program with all features

## Usage

### Compile a single file:
```bash
./compilador test.ada
```

### Run all tests:
```bash
make test-all
# or
./run_tests.sh
```

### Execute in MARS:
1. Open the generated `.asm` file in MARS
2. Assemble (F3)
3. Run (F5)

## Generated MIPS Structure

```mips
# Preamble with comments
.data
str0: .asciiz "String literal"
# ... more strings

.text
.globl main
main:
  # Program instructions
  # ...
  
  # Exit syscall
  li $v0, 10
  syscall

.data
newline: .asciiz "\n"
```

## Limitations and Future Work

### Current Limitations:
- Simple register allocation (no spilling)
- No type checking
- Limited optimizations
- No function/procedure support

### Potential Improvements:
- Implement register spilling to memory
- Add constant folding and propagation
- Optimize away redundant moves
- Support for arrays and records
- Better error messages

## Files Modified/Created

### New Files:
- `TAC.hs` - TAC generator module
- `MIPS.hs` - MIPS generator module
- `test_arithmetic.ada` - Arithmetic test
- `test_conditionals.ada` - Conditional test
- `test_loop.ada` - Loop test
- `test_nested.ada` - Nested structures test
- `test_comprehensive.ada` - Complete test
- `run_tests.sh` - Test automation script
- `.gitignore` - Git ignore file

### Modified Files:
- `Main.hs` - Added TAC and MIPS generation
- `Makefile` - Added new modules and test targets
- `README.md` - Comprehensive documentation updates

## Testing Results

All test cases successfully compile and generate valid MIPS assembly:
- ✓ test.ada
- ✓ test_arithmetic.ada
- ✓ test_conditionals.ada
- ✓ test_loop.ada
- ✓ test_nested.ada
- ✓ test_comprehensive.ada

## Conclusion

This implementation successfully extends the Ada compiler with:
1. Complete TAC generation from AST
2. Full MIPS assembly generation from TAC
3. Comprehensive test suite
4. Updated documentation

The generated MIPS code is compatible with the MARS simulator and follows MIPS assembly standards.
