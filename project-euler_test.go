package main

import (
	"bytes"
	"fmt"
	"reflect"
	"testing"
)

// Shut up about unused import.
var _ = fmt.Println

func TestParseTriangle(t *testing.T) {
	fh := bytes.NewBufferString("1\n2 3\n")
	triangle, err := parseTriangle(fh)
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
	if !gon.ContainsTriple([]int{1, 6, 4}) {
		t.Error("expected triple not found")
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
