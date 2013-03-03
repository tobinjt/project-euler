package main

import (
	"bytes"
	"fmt"
	"reflect"
	"sort"
	"testing"
)

// Shut up about unused import.
var _ = fmt.Println

func TestParseTriangle(t *testing.T) {
	fh := bytes.NewBufferString("1\n2 3")
	triangle, err := parseTriangle(fh)
	if err == nil {
		t.Fatal("missing newline should have failed")
	}

	fh = bytes.NewBufferString("x\n")
	triangle, err = parseTriangle(fh)
	if err == nil {
		t.Fatal("non-number should have failed")
	}

	fh = bytes.NewBufferString("1\n2 3\n")
	triangle, err = parseTriangle(fh)
	if err != nil {
		t.Fatal(err)
	}
	expected := [][]int{
		[]int{1},
		[]int{2, 3},
	}
	if !reflect.DeepEqual(expected, triangle) {
		t.Errorf("%#v != %#v\n", expected, triangle)
	}
}

func TestNgons(t *testing.T) {
	gon := NewNGon(3)
	gon.Set(0, []int{2, 5, 8})
	gon.Set(1, []int{1, 6, 7})
	gon.Set(2, []int{3, 4, 9})
	triple := gon.Get(0)
	expected_triple := []int{2, 9, 6}
	if !reflect.DeepEqual(expected_triple, triple) {
		t.Errorf("%#v != %#v\n", expected_triple, triple)
	}
	str := gon.String()
	if str != "sum: 11: first: 1 1,6,4; 3,4,9; 2,9,6" {
		t.Error("bad string:", str)
	}
	gon_copy := gon.Copy()
	if !reflect.DeepEqual(gon, gon_copy) {
		t.Errorf("copy != original: %#v != %#v\n", gon, gon_copy)
	}
	actual_int, err := gon.ToInt()
	expected_int := int64(164349296)
	if err != nil {
		t.Error(err)
	}
	if actual_int != expected_int {
		t.Errorf("ToInt: %v != %v\n", expected_int, actual_int)
	}
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
	if !reflect.DeepEqual(expected_perm, perm.dest) {
		t.Errorf("bad Permute() result: %#v != %#v\n", expected_perm,
			perm.dest)
	}
}

func TestInt64Sort(t *testing.T) {
	sort_me := Int64Slice{6, 3, 8, 1}
	sort.Sort(sort_me)
	sorted := Int64Slice{1, 3, 6, 8}
	if !reflect.DeepEqual(sort_me, sorted) {
		t.Errorf("sort failed: %#v != %#v\n", sort_me, sorted)
	}
}

func TestSieveOfEratosthenes(t *testing.T) {
	primes := SieveOfEratosthenes(10)
	expected := []bool{false, false, true, true, false, true, false, true,
		false, false, false}
	if !reflect.DeepEqual(primes, expected) {
		t.Errorf("SieveOfEratosthenes: %#v != %#v\n", primes, expected)
	}
}

func TestIntsArePermutations(t *testing.T) {
	tests := []struct{
		a, b int
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
	factors := PrimeFactors(15, primes)
	expected := []int{3, 5}
	if !reflect.DeepEqual(expected, factors) {
		t.Errorf("PrimeFactors: %#v != %#v\n", factors, expected)
	}
	factors = PrimeFactors(0, primes)
	expected = []int{}
	if !reflect.DeepEqual(expected, factors) {
		t.Errorf("PrimeFactors: %#v != %#v\n", factors, expected)
	}
	factors = PrimeFactors(9, primes)
	expected = []int{3, 3}
	if !reflect.DeepEqual(expected, factors) {
		t.Errorf("PrimeFactors: %#v != %#v\n", factors, expected)
	}
}

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
	}
	for _, test := range table {
		actual := test.function()
		if actual != test.result {
			t.Errorf("bad result from %v(): %v != %v\n", test.name,
				actual, test.result)
		}
	}
}
