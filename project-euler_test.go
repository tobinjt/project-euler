package main

import (
	"bytes"
	"container/heap"
	"errors"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"testing"

	"github.com/tobinjt/assert"
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
		{291, projectEuler70test, "projectEuler70"},
		{2, projectEuler71test, "projectEuler71"},
		{21, projectEuler72test, "projectEuler72"},
		{3, projectEuler73test, "projectEuler73"},
		{42, projectEuler74test, "projectEuler74"},
		{11, projectEuler75test, "projectEuler75"},
		{626, projectEuler76test, "projectEuler76"},
		{20, projectEuler77test, "projectEuler77"},
		{5, projectEuler78test, "projectEuler78"},
		{475, projectEuler80test, "projectEuler80"},
		{2427, projectEuler81test, "projectEuler81"},
		{994, projectEuler82test, "projectEuler82"},
		{2297, projectEuler83test, "projectEuler83"},
		// {102400, projectEuler84test, "projectEuler84"},
		{2770 + 2, projectEuler85test, "projectEuler85"},
		{4, projectEuler87test, "projectEuler87"},
		{61, projectEuler88test, "projectEuler88"},
		{5, projectEuler89test, "projectEuler89"},
		{7, projectEuler92test, "projectEuler92"},
		{9347898369, projectEuler97test, "projectEuler97"},
		{2, projectEuler99test, "projectEuler99"},
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
	badArgs := [][]string{
		[]string{},
		[]string{"does not exist"},
		[]string{"a", "b"},
	}
	for _, args := range badArgs {
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
	fh := bytes.NewBufferString("1\n2 3\n")
	triangle := parseTriangle(fh)
	expected := [][]int{
		[]int{1},
		[]int{2, 3},
	}
	assert.Equal(t, "parseTriangle()", expected, triangle)

	defer func(t *testing.T) {
		r := recover()
		if str, ok := r.(string); ok {
			err := errors.New(str)
			assert.ErrContains(t, "parseTriangle should have called panic()", err, "Parse error")
		} else {
			panic("Return value from recover() wasn't a string?")
		}
	}(t)
	fh = bytes.NewBufferString("x\n")
	triangle = parseTriangle(fh)
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
	actualInt, err := gon.ToInt()
	assert.ErrIsNil(t, "gon.ToInt()", err)
	assert.Equal(t, "gon.ToInt()", int64(164349296), actualInt)
}

func TestPermutable(t *testing.T) {
	perm := NewIntPermutation([]int{1, 2, 3}, 2)
	Permute(&perm)
	expectedPerm := [][]int{
		[]int{1, 2},
		[]int{1, 3},
		[]int{2, 1},
		[]int{2, 3},
		[]int{3, 1},
		[]int{3, 2},
	}
	assert.Equal(t, "Permute()", expectedPerm, perm.dest)
}

func TestInt64Sort(t *testing.T) {
	sortMe := Int64Slice{6, 3, 8, 1}
	sort.Sort(sortMe)
	assert.Equal(t, "Sort()", Int64Slice{1, 3, 6, 8}, sortMe)
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

func TestcalculateFactorialSum(t *testing.T) {
	// The test results are copied from the description of problem 74.
	assert.Equal(t, "calculateFactorialSum",
		1, calculateFactorialSum(0))
	assert.Equal(t, "calculateFactorialSum",
		720, calculateFactorialSum(6))
	assert.Equal(t, "calculateFactorialSum",
		363600, calculateFactorialSum(69))
	assert.Equal(t, "calculateFactorialSum",
		362881, calculateFactorialSum(90))
	assert.Equal(t, "calculateFactorialSum",
		145, calculateFactorialSum(145))
	assert.Equal(t, "calculateFactorialSum",
		169, calculateFactorialSum(1454))
	assert.Equal(t, "calculateFactorialSum",
		363601, calculateFactorialSum(169))
	assert.Equal(t, "calculateFactorialSum",
		45361, calculateFactorialSum(871))
	assert.Equal(t, "calculateFactorialSum",
		45360, calculateFactorialSum(78))
}

func TestcalculateFactorialChainLength(t *testing.T) {
	assert.Equal(t, "calculateFactorialChainLength 169", 3,
		calculateFactorialChainLength(169))
	assert.Equal(t, "calculateFactorialChainLength 871", 2,
		calculateFactorialChainLength(871))
	assert.Equal(t, "calculateFactorialChainLength 69", 5,
		calculateFactorialChainLength(69))
	assert.Equal(t, "calculateFactorialChainLength 78", 4,
		calculateFactorialChainLength(78))
	assert.Equal(t, "calculateFactorialChainLength 540", 2,
		calculateFactorialChainLength(540))
	assert.Equal(t, "calculateFactorialChainLength 4197", 60,
		calculateFactorialChainLength(4197))
}

func TestMakeChildren(t *testing.T) {
	parent := PythagoreanTriple{a: 3, b: 4, c: 5}
	expectedChildren := []PythagoreanTriple{
		PythagoreanTriple{a: 5, b: 12, c: 13},
		PythagoreanTriple{a: 21, b: 20, c: 29},
		PythagoreanTriple{a: 15, b: 8, c: 17},
	}
	children := parent.MakeChildren()
	assert.Equal(t, "MakeChildren", expectedChildren, children)
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
	sopfCache := make(map[int]int)
	nppCache := make(map[int]int)
	assert.Equal(t, "NumPrimePartitions", 1,
		NumPrimePartitions(2, sieve, nppCache, sopfCache))
	assert.Equal(t, "NumPrimePartitions", 7,
		NumPrimePartitions(12, sieve, nppCache, sopfCache))
	assert.Equal(t, "NumPrimePartitions", 10,
		NumPrimePartitions(14, sieve, nppCache, sopfCache))
	assert.Equal(t, "NumPrimePartitions", 26,
		NumPrimePartitions(20, sieve, nppCache, sopfCache))
}

func TestpentagonalNumber(t *testing.T) {
	assert.Equal(t, "pentagonalNumber", 1, pentagonalNumber(1))
	assert.Equal(t, "pentagonalNumber", 5, pentagonalNumber(2))
	assert.Equal(t, "pentagonalNumber", 35, pentagonalNumber(5))
	assert.Equal(t, "pentagonalNumber", 92, pentagonalNumber(8))
}

func TestGeneralisedpentagonalNumber(t *testing.T) {
	expected := []int{0, 1, 2, 5, 7, 12, 15, 22, 26, 35, 40}
	for number, result := range expected {
		assert.Equal(t, "generalisedPentagonalNumber", result,
			generalisedPentagonalNumber(number))
	}
}

func TestNumIntegerPartitions2(t *testing.T) {
	expected := []int64{1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101,
		135, 176, 231, 297, 385, 490, 627, 792, 1002, 1255, 1575, 1958,
		2436, 3010, 3718, 4565, 5604, 6842, 8349, 10143, 12310, 14883,
		17977, 21637, 26015, 31185, 37338, 44583, 53174, 63261, 75175,
		89134, 105558, 124754, 147273, 173525}
	for number, result := range expected {
		assert.Equal(t, "NumIntegerPartitions2", result,
			NumIntegerPartitions2(number).Int64())
	}
}

func TestSqrt(t *testing.T) {
	expected := []int{1, 4, 1, 4, 2, 1, 3, 5, 6, 2}
	assert.Equal(t, "SqrtPE80(2, 10)", expected, SqrtPE80(2, 10))
	expected = []int{5}
	assert.Equal(t, "SqrtPE80(25, 10)", expected, SqrtPE80(25, 10))
	expected = []int{3, 1}
	assert.Equal(t, "SqrtPE80(961, 10)", expected, SqrtPE80(961, 10))
	expected = []int{1, 4, 1, 4, 2, 1, 3, 5, 6, 2, 3, 7, 3, 0, 9, 5, 0, 4,
		8, 8, 0, 1, 6, 8, 8, 7, 2, 4, 2, 0, 9, 6, 9, 8, 0, 7, 8, 5, 6,
		9, 6, 7, 1, 8, 7, 5, 3, 7, 6, 9, 4, 8, 0, 7, 3, 1, 7, 6, 6, 7,
		9, 7, 3, 7, 9, 9, 0, 7, 3, 2, 4, 7, 8, 4, 6, 2, 1, 0, 7, 0, 3,
		8, 8, 5, 0, 3, 8, 7, 5, 3, 4, 3, 2, 7, 6, 4, 1, 5, 7, 2}
	assert.Equal(t, "SqrtPE80(2, 100)", expected, SqrtPE80(2, 100))
}

func TestReadIntsFromCSVFile(t *testing.T) {
	tests := []struct {
		csv, err string
		expected [][]uint64
	}{
		{csv: "1,7,42", expected: [][]uint64{{1, 7, 42}}},
		{csv: "1,7,42,qwerty", err: "strconv.ParseUint"},
		{csv: "1,7,42,qwe\"rty", err: "non-quoted-field"},
	}
	for _, test := range tests {
		actual, err := readIntsFromCSVFile(strings.NewReader(test.csv))
		desc := fmt.Sprintf("readIntsFromCSVFile(%q)", test.csv)
		if test.err == "" {
			assert.ErrIsNil(t, "gon.ToInt()", err)
			assert.Equal(t, desc, test.expected, actual)
		} else {
			assert.ErrContains(t, desc, err, test.err)
		}
	}
}

func TestSquareChain(t *testing.T) {
	cache := make([]uint, squareChainCacheSize)
	cache[1] = 1
	cache[89] = 89
	assert.Equal(t, "squareChain(1)", uint(1), squareChain(1, cache))
	assert.Equal(t, "squareChain(89)", uint(89), squareChain(89, cache))
	assert.Equal(t, "squareChain(44)", uint(1), squareChain(44, cache))
	assert.Equal(t, "squareChain(85)", uint(89), squareChain(85, cache))
}

func TestRomanNumeralsToUint(t *testing.T) {
	shouldAssert := []struct {
		input string
		msg   string
	}{
		{input: "", msg: "empty string is invalid"},
		{input: "JTXIV", msg: "unrecognised sequence: JTXIV"},
		{input: "XIJI", msg: "unrecognised sequence: JI"},
		{input: "DIXX", msg: "sequence \"X\" followed smaller sequence \"IX\""},
	}
	for _, test := range shouldAssert {
		_, err := romanNumeralsToUint(test.input)
		assert.ErrContains(t, "romanNumeralsToUint(\""+test.input+"\")",
			err, test.msg)
	}

	valid := []struct {
		input  string
		result uint
	}{
		{input: "III", result: 3},
		{input: "V", result: 5},
		{input: "X", result: 10},
		{input: "XIX", result: 19},
		{input: "LV", result: 55},
		{input: "DCLXVI", result: 666},
		{input: "MCMXCVII", result: 1997},
	}
	for _, test := range valid {
		result, err := romanNumeralsToUint(test.input)
		assert.ErrIsNil(t, "romanNumeralsToUint(\""+test.input+"\")", err)
		assert.Equal(t, "romanNumeralsToUint(\""+test.input+"\")", test.result, result)
	}
}

func TestUintToRomanNumerals(t *testing.T) {
	result, err := uintToRomanNumerals(0)
	assert.ErrContains(t, "uintToRomanNumerals(0)", err, "0 is invalid")
	valid := []struct {
		input  uint
		result string
	}{
		{input: 3, result: "III"},
		{input: 5, result: "V"},
		{input: 10, result: "X"},
		{input: 19, result: "XIX"},
		{input: 55, result: "LV"},
		{input: 666, result: "DCLXVI"},
		{input: 1997, result: "MCMXCVII"},
	}
	for _, test := range valid {
		result, err = uintToRomanNumerals(test.input)
		msg := "uintToRomanNumerals(" + strconv.FormatUint(uint64(test.input), 10) + ")"
		assert.ErrIsNil(t, msg, err)
		assert.Equal(t, msg, test.result, result)
	}
}

func TestReadLinesFromFile(t *testing.T) {
	fh := bytes.NewBufferString("1\n2 3\n")
	expected := []string{"1", "2 3"}
	actual := readLinesFromFile(fh)
	assert.Equal(t, "readLinesFromFile bad result", expected, actual)
	// TODO(johntobin): test failure handling.
}

func TestNumCombinations(t *testing.T) {
	tests := []struct {
		n, k, output uint64
	}{
		{n: 7, k: 2, output: 21},
		{n: 9, k: 4, output: 126},
		{n: 3, k: 1, output: 3},
		{n: 22, k: 15, output: 170544},
	}
	for _, test := range tests {
		assert.Equal(t, fmt.Sprintf("numCombinations(%v, %v)", test.n, test.k),
			numCombinations(test.n, test.k), test.output)
	}
}

func TestTwoDPointHeap(t *testing.T) {
	n1 := TwoDPoint{cost: 1}
	n2 := TwoDPoint{cost: 2}
	n3 := TwoDPoint{cost: 3}
	n4 := TwoDPoint{cost: 4}

	h := &TwoDPointHeap{n2, n4, n1}
	heap.Init(h)
	assert.Equal(t, "Expecting n1", n1, heap.Pop(h))
	heap.Push(h, n3)
	assert.Equal(t, "Expecting n2", n2, heap.Pop(h))
	assert.Equal(t, "Expecting n3", n3, heap.Pop(h))
	assert.Equal(t, "Expecting n4", n4, heap.Pop(h))
}

func TestFillDiceRolls(t *testing.T) {
	faces, position, rollsLeft := int64(2), int64(0), int64(2)
	rolls := make([]int64, faces*rollsLeft*2)
	fillDiceRolls(rolls, faces, position, rollsLeft)

	expected := []int64{4, 0, 1, 2, 1, 2, 0, 2}
	assert.Equal(t, "fillDiceRolls", expected, rolls)
}

func TestFactors(t *testing.T) {
	// Note that 4 should only be in the list once.
	assert.Equal(t, "Factors", []uint64{1, 2, 4, 8, 16}, Factors(16, true))
	assert.Equal(t, "Factors", []uint64{2, 4, 8}, Factors(16, false))
}

func TestPermuteKOfN(t *testing.T) {
	p := make([]bool, 4)
	for _, test := range [][]bool{
		{false, false, false, true},
		{false, false, true, false},
		{false, false, true, true},
		{false, true, false, false},
		{false, true, false, true},
		{false, true, true, false},
		{true, false, false, false},
		{true, false, false, true},
		{true, false, true, false},
		{true, true, false, false},
	} {
		PermuteUpToKOfN(p, 2)
		assert.Equal(t, "PermuteUpToKOfN", test, p)
	}
}

// NOTE BEWARE ACHTUNG!
// The first character after 'Test' in the function name must be uppercase or
// 'go test' will silently ignore it.  Gah.
