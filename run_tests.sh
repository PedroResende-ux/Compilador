#!/bin/bash
# Test script for the integrated compiler
# Tests both symbol table and code generation functionality

set -e

echo "=========================================="
echo "Integrated Compiler Test Suite"
echo "=========================================="
echo ""

PASSED=0
FAILED=0

# Function to run a test
run_test() {
    local test_name="$1"
    local test_file="$2"
    local should_fail="$3"
    
    echo -n "Testing $test_name... "
    
    if [ "$should_fail" = "fail" ]; then
        # Test should produce semantic errors
        if ./compilador "$test_file" 2>&1 | grep -q "Semantic errors found"; then
            echo "PASS (error detected as expected)"
            PASSED=$((PASSED + 1))
        else
            echo "FAIL (expected semantic error not detected)"
            FAILED=$((FAILED + 1))
        fi
    else
        # Test should succeed
        if ./compilador "$test_file" > /dev/null 2>&1; then
            echo "PASS"
            PASSED=$((PASSED + 1))
        else
            echo "FAIL"
            FAILED=$((FAILED + 1))
        fi
    fi
}

# Test semantic analysis (from PR #2)
echo "=== Semantic Analysis Tests ==="
run_test "Declarations" "test_declarations.ada" "pass"
run_test "Undeclared variable" "test_undeclared.ada" "fail"
run_test "Redeclaration" "test_redeclaration.ada" "fail"
run_test "Nested scope" "test_nested_scope.ada" "pass"
echo ""

# Test code generation (from PR #3)
echo "=== Code Generation Tests ==="
run_test "Arithmetic operations" "test_arithmetic_decl.ada" "pass"
run_test "Comprehensive test" "test_comprehensive.ada" "pass"
run_test "Complex nested test" "test_comprehensive_pr2.ada" "pass"
echo ""

# Summary
echo "=========================================="
echo "Test Summary:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo "=========================================="

if [ $FAILED -eq 0 ]; then
    echo "All tests passed! ✓"
    exit 0
else
    echo "Some tests failed! ✗"
    exit 1
fi
