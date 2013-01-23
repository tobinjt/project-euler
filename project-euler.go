package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// Shut up about unused import.
var _ = fmt.Println

/*
* By starting at the top of the triangle below and moving to adjacent numbers on
* the row below, the maximum total from top to bottom is 23.
*
* 3
* 7 4
* 2 4 6
* 8 5 9 3
*
* That is, 3 + 7 + 4 + 9 = 23.
*
* Find the maximum total from top to bottom in triangle.txt (right click and
* 'Save Link/Target As...'), a 15K text file containing a triangle with
* one-hundred rows.
*
* NOTE: This is a much more difficult version of Problem 18. It is not possible
* to try every route to solve this problem, as there are 299 altogether! If you
* could check one trillion (1012) routes every second it would take over twenty
* billion years to check them all. There is an efficient algorithm to solve it.
* ;o)
 */
func parseTriangle(filename string) ([][]int, error) {
	fh, err := os.Open(filename)
	if err != nil {
		log.Println(err)
		return nil, err
	}
	defer fh.Close()
	bfh := bufio.NewReader(fh)
	triangle := make([][]int, 0)
	for {
		line, err := bfh.ReadString(byte('\n'))
		if err != nil && len(line) > 0 {
			// Incomplete read
			log.Println(err)
			return nil, err
		}
		if err != nil {
			break
		}
		line = strings.TrimRight(line, "\n")
		numbers := make([]int, 0)
		for _, ascii_number := range strings.Fields(line) {
			parsed_number, err := strconv.Atoi(ascii_number)
			if err != nil {
				// Parsing error
				log.Println(err)
				return nil, err
			}
			numbers = append(numbers, parsed_number)
		}
		triangle = append(triangle, numbers)
	}
	return triangle, nil
}

func projectEuler67() {
	triangle, err := parseTriangle("triangle.txt")
	if err != nil {
		log.Fatalln(err)
	}
	// Start at the second last row and work upwards.
	for i := len(triangle) - 2; i >= 0; i-- {
		for j := 0; j <= i; j++ {
			left, right := triangle[i+1][j], triangle[i+1][j+1]
			if left > right {
				triangle[i][j] += left
			} else {
				triangle[i][j] += right
			}
		}
	}
	fmt.Println(triangle[0][0])
}

/*
* Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and
* each line adding to nine.
*
*
* Working clockwise, and starting from the group of three with the numerically
* lowest external node (4,3,2 in this example), each solution can be described
* uniquely. For example, the above solution can be described by the set: 4,3,2;
* 6,2,1; 5,1,3.
*
* It is possible to complete the ring with four different totals: 9, 10, 11, and
* 12. There are eight solutions in total.
*
* Total	Solution Set
* 9	4,2,3; 5,3,1; 6,1,2
* 9	4,3,2; 6,2,1; 5,1,3
* 10	2,3,5; 4,5,1; 6,1,3
* 10	2,5,3; 6,3,1; 4,1,5
* 11	1,4,6; 3,6,2; 5,2,4
* 11	1,6,4; 5,4,2; 3,2,6
* 12	1,5,6; 2,6,4; 3,4,5
* 12	1,6,5; 3,5,4; 2,4,6
* By concatenating each group it is possible to form 9-digit strings; the
* maximum string for a 3-gon ring is 432621513.
*
* Using the numbers 1 to 10, and depending on arrangements, it is possible to
* form 16- and 17-digit strings. What is the maximum 16-digit string for a
* "magic" 5-gon ring?
*/
/*
* The images are missing, so here's a description: an X-gon ring has X/2 nodes
* in a ring, and X/2 nodes hanging off the ring.  E.g. a 3-gon looks like:
* O--O   O   A--B   D
*   / \ /      / \ /
*  O---O      E---C
*   \          \
*    O          F
* It's read as: A,B,C; D,C,E; F,E,B - you start with the lowest external node.
*/

type NGonOuter struct {
	value int
	inner *NGonInner
}
type NGonInner struct {
	value int
	inner *NGonInner
	outer *NGonOuter
}
type NGon struct {
	inners []NGonInner
	outers []NGonOuter
}

func (inner NGonInner) String() string {
	return fmt.Sprintf("%v, %v, %v", inner.outer.value, inner.value,
		inner.inner.value)
}

func (gon *NGon) String() string {
	results := make([]string, 0)
	first, value := 0, gon.inners[0].outer.value
	for i := range gon.inners {
		if gon.inners[i].outer.value < value {
			first = i
			value = gon.inners[i].outer.value
		}
	}
	for i := range gon.inners {
		inner := gon.inners[(first + i) % len(gon.inners)]
		results = append(results, fmt.Sprint(inner))
	}
	return fmt.Sprintf("%v: ", len(gon.inners)) +
		strings.Join(results, "; ")
}

func NewNGon(n int) *NGon {
	gon := &NGon{
		inners: make([]NGonInner, n),
		outers: make([]NGonOuter, n),
	}
	for i := range gon.inners {
		gon.inners[i].outer = &gon.outers[i]
		gon.inners[i].inner = &gon.inners[(i + 1) % n]
		gon.outers[i].inner = &gon.inners[i]
	}
	return gon
}

/*
* An interface for permutable arrays.
*/
type Permutable interface {
	// self.dest[dest_i][dest_j] = self.src[src_i].
	Copy(src_i, dest_i, dest_j int)
	// len(self.dest)
	NumPermutations() int
	// self,permutation_size
	PermutationSize() int
	// len(self.dest)
	SetSize() int
}

type PermutableInt struct {
	src []int
	dest [][]int
	permutation_size int
}
func (self PermutableInt) Copy(src_i, dest_i, dest_j int) {
	self.dest[dest_i][dest_j] = self.src[src_i]
}
func (self PermutableInt) NumPermutations() int {
	return len(self.dest)
}
func (self PermutableInt) PermutationSize() int {
	return self.permutation_size
}
func (self PermutableInt) SetSize() int {
	return len(self.src)
}
func NewPermutableInt(set []int, permutation_size int) PermutableInt {
	set_size := len(set)
	num_permutations, err := numPermutations(set_size, permutation_size)
	if err != nil {
		log.Fatalln(err)
	}
	result := PermutableInt{
		permutation_size: permutation_size,
		src: make([]int, set_size),
		dest: make([][]int, num_permutations),
	}
	for i, value := range set {
		result.src[i] = value
	}
	for i := range result.dest {
		result.dest[i] = make([]int, len(set))
	}
	return result
}

func numPermutations(set_size, permutation_size int) (int, error) {
	if set_size < permutation_size {
		return 0, errors.New(fmt.Sprintf(
			"set_size (%v) < permutation_size (%v)", set_size,
			permutation_size))
	}
	result := 1
	for i := set_size; i > set_size - permutation_size; i-- {
		result *= i
	}
	return result, nil
}

func Permute(set Permutable) {
	fmt.Printf("%+v", set)
}

func main() {
	pairs := [][]int {
		{10, 3},
		{10, 0},
		{4, 3},
		{4, 2},
		{3, 2},
		{5, 3},
	}
	for _, pair := range pairs {
		result, err := numPermutations(pair[0], pair[1])
		if err != nil {
			log.Fatalln(err)
		}
		fmt.Printf("%d/%d: %d\n", pair[0], pair[1], result)
	}
	set := NewPermutableInt([]int{1, 2, 3, 4}, 3)
	fmt.Printf("%v\n", set)
	Permute(set)
}
