package main

import (
	"bytes"
	"fmt"
	"github.com/tobinjt/assert"
	"sort"
	"testing"
)

// Shut up about unused import.
var _ = fmt.Println

func TestProjectEuler(t *testing.T) {
	table := []struct {
		result   int64
		function func() int64
		name     string
	}{
		{7273, projectEuler67, "projectEuler67"},
		{6531031914842725, projectEuler68, "projectEuler68"},
		{510510, projectEuler69, "projectEuler69"},
		{8319823, projectEuler70, "projectEuler70"},
		{2, projectEuler71test, "projectEuler71"},
		{21, projectEuler72test, "projectEuler72"},
		{3, projectEuler73test, "projectEuler73"},
		{11, projectEuler75test, "projectEuler75"},
		{626, projectEuler76test, "projectEuler76"},
		{20, projectEuler77test, "projectEuler77"},
		{5, projectEuler78test, "projectEuler78"},
	}
	for _, test := range table {
		assert.Equal(t, test.name, test.result, test.function())
	}
}

func TestGreatestCommonDenominator(t *testing.T) {
	tests := []struct {
		a, b, expected int64
	}{
		{12, 8, 4},
		{12, 7, 1},
		{99, 44, 11},
	}
	for _, test := range tests {
		assert.Equal(t, "GreatestCommonDenominator", test.expected,
			GreatestCommonDenominator(test.a, test.b))
	}
}

func TestRealMain(t *testing.T) {
	bad_args := [][]string{
		[]string{},
		[]string{"does not exist"},
		[]string{"a", "b"},
	}
	for _, args := range bad_args {
		_, err := realMain(args)
		assert.ErrContains(t, "realMain()", err,
			"Only 1 arg accepted from this list")
	}
	_, err := realMain([]string{"fortesting"})
	assert.ErrIsNil(t, "realMain()", err)
}

func TestBreakpoint(t *testing.T) {
	assert.Equal(t, "breakpoint()", "breakpoint reached", breakpoint())
}

func TestParseTriangle(t *testing.T) {
	fh := bytes.NewBufferString("1\n2 3")
	triangle, err := parseTriangle(fh)
	if err == nil {
		t.Error("missing newline should have failed")
	}

	fh = bytes.NewBufferString("x\n")
	triangle, err = parseTriangle(fh)
	if err == nil {
		t.Error("non-number should have failed")
	}

	fh = bytes.NewBufferString("1\n2 3\n")
	triangle, err = parseTriangle(fh)
	if assert.ErrIsNil(t, "parseTriangle()", err) {
		expected := [][]int{
			[]int{1},
			[]int{2, 3},
		}
		assert.Equal(t, "parseTriangle()", expected, triangle)
	}
}

func TestNgons(t *testing.T) {
	gon := NewNGon(3)
	gon.Set(0, []int{2, 5, 8})
	gon.Set(1, []int{1, 6, 7})
	gon.Set(2, []int{3, 4, 9})
	assert.Equal(t, "gon.Get()", []int{2, 9, 6}, gon.Get(0))
	assert.Equal(t, "gon.String()", "sum: 11: first: 1 1,6,4; 3,4,9; 2,9,6",
		gon.String())
	assert.Equal(t, "gon.Copy()", gon, gon.Copy())
	actual_int, err := gon.ToInt()
	assert.ErrIsNil(t, "gon.ToInt()", err)
	assert.Equal(t, "gon.ToInt()", int64(164349296), actual_int)
}

func TestPermutable(t *testing.T) {
	perm := NewIntPermutation([]int{1, 2, 3}, 2)
	Permute(&perm)
	expected_perm := [][]int{
		[]int{1, 2},
		[]int{1, 3},
		[]int{2, 1},
		[]int{2, 3},
		[]int{3, 1},
		[]int{3, 2},
	}
	assert.Equal(t, "Permute()", expected_perm, perm.dest)
}

func TestInt64Sort(t *testing.T) {
	sort_me := Int64Slice{6, 3, 8, 1}
	sort.Sort(sort_me)
	assert.Equal(t, "Sort()", Int64Slice{1, 3, 6, 8}, sort_me)
}

func TestSieveOfEratosthenes(t *testing.T) {
	expected := []bool{false, false, true, true, false, true, false, true,
		false, false, false}
	assert.Equal(t, "SieveOfEratosthenes()", expected,
		SieveOfEratosthenes(10))
}

func TestIntsArePermutations(t *testing.T) {
	tests := []struct {
		a, b   int
		result bool
	}{
		{0, 0, true},
		{0, 1, false},
		{17, 71, true},
		{17, 72, false},
		{112, 121, true},
	}
	for _, test := range tests {
		if IntsArePermutations(test.a, test.b) == test.result {
			continue
		}
		t.Errorf("IntsArePermutations(%v, %v): bad result: %v\n",
			test.a, test.b, test.result)
	}
}

func TestPrimeFactors(t *testing.T) {
	primes := SieveOfEratosthenes(10)
	tests := []struct {
		number   int
		expected []int
	}{
		{15, []int{3, 5}},
		{0, []int{}},
		{9, []int{3, 3}},
	}
	for _, test := range tests {
		assert.Equal(t, "PrimeFactors", test.expected,
			PrimeFactors(test.number, primes))
	}
}

func TestMakePhiLookupTablet(t *testing.T) {
	assert.Equal(t, "MakePhiLookupTable",
		[]int64{0, 1, 1, 2, 2, 4, 2, 6, 4, 6, 4},
		MakePhiLookupTable(10))
}

func TestCalculateFactorialSum(t *testing.T) {
	// The test results are copied from the description of problem 74.
	assert.Equal(t, "CalculateFactorialSum",
		1, CalculateFactorialSum(0))
	assert.Equal(t, "CalculateFactorialSum",
		720, CalculateFactorialSum(6))
	assert.Equal(t, "CalculateFactorialSum",
		363600, CalculateFactorialSum(69))
	assert.Equal(t, "CalculateFactorialSum",
		362881, CalculateFactorialSum(90))
	assert.Equal(t, "CalculateFactorialSum",
		145, CalculateFactorialSum(145))
	assert.Equal(t, "CalculateFactorialSum",
		169, CalculateFactorialSum(1454))
	assert.Equal(t, "CalculateFactorialSum",
		363601, CalculateFactorialSum(169))
	assert.Equal(t, "CalculateFactorialSum",
		45361, CalculateFactorialSum(871))
	assert.Equal(t, "CalculateFactorialSum",
		45360, CalculateFactorialSum(78))
}

func TestCalculateFactorialChainLength(t *testing.T) {
	assert.Equal(t, "CalculateFactorialChainLength 169", 3,
		CalculateFactorialChainLength(169))
	assert.Equal(t, "CalculateFactorialChainLength 871", 2,
		CalculateFactorialChainLength(871))
	assert.Equal(t, "CalculateFactorialChainLength 69", 5,
		CalculateFactorialChainLength(69))
	assert.Equal(t, "CalculateFactorialChainLength 78", 4,
		CalculateFactorialChainLength(78))
	assert.Equal(t, "CalculateFactorialChainLength 540", 2,
		CalculateFactorialChainLength(540))
	assert.Equal(t, "CalculateFactorialChainLength 4197", 60,
		CalculateFactorialChainLength(4197))
}

func TestMakeChildren(t *testing.T) {
	parent := PythagoreanTriple{a: 3, b: 4, c: 5}
	expected_children := []PythagoreanTriple{
		PythagoreanTriple{a: 5, b: 12, c: 13},
		PythagoreanTriple{a: 21, b: 20, c: 29},
		PythagoreanTriple{a: 15, b: 8, c: 17},
	}
	children := parent.MakeChildren()
	assert.Equal(t, "MakeChildren", expected_children, children)
}

func TestNumIntegerPartitions(t *testing.T) {
	// Caching shouldn't break the tests.
	assert.Equal(t, "NumIntegerPartitions", 6, NumIntegerPartitions(5, 4))
	assert.Equal(t, "NumIntegerPartitions", 7, NumIntegerPartitions(5, 5))
	assert.Equal(t, "NumIntegerPartitions", 6, NumIntegerPartitions(5, 4))
}

func TestSumOfPrimeFactors(t *testing.T) {
	sieve := SieveOfEratosthenes(50)
	assert.Equal(t, "SumOfPrimeFactors", 0, SumOfPrimeFactors(0, sieve))
	assert.Equal(t, "SumOfPrimeFactors", 5, SumOfPrimeFactors(5, sieve))
	assert.Equal(t, "SumOfPrimeFactors", 7, SumOfPrimeFactors(10, sieve))
	// Duplicate prime factors should not increase the sum.
	assert.Equal(t, "SumOfPrimeFactors", 7, SumOfPrimeFactors(20, sieve))
	assert.Equal(t, "SumOfPrimeFactors", 15, SumOfPrimeFactors(105, sieve))
	assert.Equal(t, "SumOfPrimeFactors", 14, SumOfPrimeFactors(140, sieve))
}

func TestNumPrimePartitions(t *testing.T) {
	sieve := SieveOfEratosthenes(50)
	sopf_cache := make(map[int]int)
	npp_cache := make(map[int]int)
	assert.Equal(t, "NumPrimePartitions", 1,
		NumPrimePartitions(2, sieve, npp_cache, sopf_cache))
	assert.Equal(t, "NumPrimePartitions", 7,
		NumPrimePartitions(12, sieve, npp_cache, sopf_cache))
	assert.Equal(t, "NumPrimePartitions", 10,
		NumPrimePartitions(14, sieve, npp_cache, sopf_cache))
	assert.Equal(t, "NumPrimePartitions", 26,
		NumPrimePartitions(20, sieve, npp_cache, sopf_cache))
}

func TestPentagonalNumber(t *testing.T) {
	assert.Equal(t, "PentagonalNumber", 1, PentagonalNumber(1))
	assert.Equal(t, "PentagonalNumber", 5, PentagonalNumber(2))
	assert.Equal(t, "PentagonalNumber", 35, PentagonalNumber(5))
	assert.Equal(t, "PentagonalNumber", 92, PentagonalNumber(8))
}
