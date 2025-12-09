#!/bin/bash
# Script to compile all test Ada files and generate MIPS assembly

echo "==================================="
echo "Ada to MIPS Compiler - Test Suite"
echo "==================================="
echo ""

# Compile the compiler if needed
if [ ! -f ./compilador ]; then
    echo "Compiler not found. Building..."
    make
    echo ""
fi

# Test files
test_files=(
    "test.ada"
    "test_arithmetic.ada"
    "test_conditionals.ada"
    "test_loop.ada"
    "test_nested.ada"
    "test_comprehensive.ada"
)

echo "Compiling test files..."
echo ""

success_count=0
fail_count=0

for file in "${test_files[@]}"; do
    if [ -f "$file" ]; then
        echo "Processing $file..."
        if ./compilador "$file" > /dev/null 2>&1; then
            asm_file="${file%.ada}.asm"
            echo "  ✓ Generated $asm_file"
            ((success_count++))
        else
            echo "  ✗ Failed to compile $file"
            ((fail_count++))
        fi
    else
        echo "  ⚠ File $file not found"
    fi
done

echo ""
echo "==================================="
echo "Summary:"
echo "  Successful: $success_count"
echo "  Failed: $fail_count"
echo "==================================="
echo ""

if [ $success_count -gt 0 ]; then
    echo "Generated .asm files can be run in MARS MIPS Simulator"
    echo "Download MARS from: http://courses.missouristate.edu/kenvollmar/mars/"
fi
