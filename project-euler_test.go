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
