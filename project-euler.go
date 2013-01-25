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
/*
* Thoughts:
* - The 10 must be in the outer ring; we want a 16 digit number, and if the 10
*   is in the inner ring it will appear twice in the output, forcing a 17 digit
*   solution.
* - The first number cannot be 10, because the output would never sort 10
*   first.
* - There are 9*8*7 (504) permutations for the first triple (10 is excluded from
*   the inner ring and from being the first number).
* - The answer will not begin with 9, 8, or 7, because there will always be a
*   smaller starting digit in the 5-gon.
* - Starting with the higher valued triples, check if the triple is present in
*   an existing 5-gon; if not try to generate a 5-gon.
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

func (gon *NGon) ContainsTriple(triple []int) bool {
	for _, outer := range gon.outers {
		if triple[0] == outer.value &&
			triple[1] == outer.inner.value &&
			triple[2] == outer.inner.inner.value {
			return true
		}
	}
	return false
}

func (gon * NGon) Set(index int, triple []int) {
	if index >= len(gon.outers) {
		log.Fatalf("index out of range: %d >= %d\n", index,
			len(gon.outers))
	}
	gon.outers[index].value = triple[0]
	gon.outers[index].inner.value = triple[1]
	gon.outers[index].inner.inner.value = triple[2]
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

/*
* Impement Permutable for ints.
*/
type IntPermutation struct {
	src []int
	dest [][]int
	permutation_size int
}
func (self *IntPermutation) Copy(src_i, dest_i, dest_j int) {
	self.dest[dest_i][dest_j] = self.src[src_i]
}
func (self *IntPermutation) NumPermutations() int {
	return len(self.dest)
}
func (self *IntPermutation) PermutationSize() int {
	return self.permutation_size
}
func (self *IntPermutation) SetSize() int {
	return len(self.src)
}
func NewIntPermutation(set []int, permutation_size int) IntPermutation {
	set_size := len(set)
	num_permutations, err := NumPermutations(set_size, permutation_size)
	if err != nil {
		log.Fatalln(err)
	}
	result := IntPermutation{
		permutation_size: permutation_size,
		src: make([]int, set_size),
		dest: make([][]int, num_permutations),
	}
	for i, value := range set {
		result.src[i] = value
	}
	for i := range result.dest {
		result.dest[i] = make([]int, permutation_size)
	}
	return result
}

/*
* Calculates the number of permutations that would be generated.
* Args:
*  set_size: the number of elements in the set.
*  permutation_size: the number of elements in each permutation.
*/
func NumPermutations(set_size, permutation_size int) (int, error) {
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

/*
* Generate all the permutations.
*/
func Permute(set Permutable) {
	used := make([]bool, set.SetSize())
	permute(set, used, 0, 0, set.NumPermutations(), set.SetSize())
}

/*
* permute: does the real work of generating permutations.
* Args:
*  set: the set to operate on.
*  used: whether each element has already been used.
*  col: the column in the dest array to operate on.
*  start: the first index to operate on.
*  end: the first index NOT to operate on.
*  num_unused: how many elements are unused.
*/
func permute(set Permutable, used []bool, col, start, end, num_unused int) {
	reps := (end - start) / num_unused
	permutation_size := set.PermutationSize()
	for i := range used {
		if used[i] {
			continue
		}
		for j := 0; j < reps; j++ {
			set.Copy(i, start + j, col)
		}
		if col + 1 < permutation_size {
			used[i] = true
			permute(set, used, col + 1, start, start + reps,
				num_unused - 1)
			used[i] = false
		}
		start += reps
	}
}

func projectEuler68() {
	ngon_size := 3
	numbers := []int{1, 2, 3, 4, 5, 6}
	set := NewIntPermutation(numbers, 3)
	Permute(&set)
	ngons := make([]NGon, 0)

	TRIPLE:
	for i := set.NumPermutations() - 1; i >= 0; i-- {
		triple := set.dest[i]
		if triple[0] > 6 {
			continue
		}
		for _, ngon := range ngons {
			if ngon.ContainsTriple(triple) {
				continue TRIPLE
			}
		}

		gon := NewNGon(ngon_size)
		gon.Set(0, triple)
		sum := 0
		used := make([]bool, len(set.src) + 1)
		for _, num := range triple {
			sum += num
			used[num] = true
		}
		fmt.Print(triple)
	}
}

func main() {
	projectEuler68()
	// gon := NewNGon(3)
	// gon.Set(

}
