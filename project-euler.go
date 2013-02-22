package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func breakpoint() {
	fmt.Println("breakpoint reached")
}

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
* to try every route to solve this problem, as there are 2**99 altogether! If
* you could check one trillion (1012) routes every second it would take over
* twenty billion years to check them all. There is an efficient algorithm to
* solve it.  ;o)
 */
func parseTriangle(fh io.Reader) ([][]int, error) {
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
	fh, err := os.Open("triangle.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	triangle, err := parseTriangle(fh)
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
*   an existing 5-gon; if not try to generate a 5-gon.  This doesn't happen much
*   with 3-gons, but eliminates 102 of 504 5-Gons.  TODO(johntobin): Are we
*   guaranteed to generate all the possible NGons?  It works, but is it correct?
* - If the outer value in a starting triple is lower than the outer value of the
*   first triple (as it would be printed) in a valid NGon, we can discard that
*   triple.  An NGon starting with that triple would not be the answer we want,
*   and if the NGon is better it must start with a different triple and so we
*   would find it anyway.
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
	return fmt.Sprintf("%v,%v,%v", inner.outer.value, inner.value,
		inner.inner.value)
}
func (gon NGon) String() string {
	results := make([]string, 0)
	first := gon.StartIndex()
	triple := gon.Get(first)
	sum := triple[0] + triple[1] + triple[2]
	for i := range gon.inners {
		inner := gon.inners[(first+i)%len(gon.inners)]
		results = append(results, fmt.Sprint(inner))
	}
	return fmt.Sprintf("sum: %v: first: %v ", sum, first) +
		strings.Join(results, "; ")
}
func NewNGon(n int) *NGon {
	gon := &NGon{
		inners: make([]NGonInner, n),
		outers: make([]NGonOuter, n),
	}
	for i := range gon.inners {
		gon.inners[i].outer = &gon.outers[i]
		gon.inners[i].inner = &gon.inners[(i+1)%n]
		gon.outers[i].inner = &gon.inners[i]
	}
	return gon
}
func (gon *NGon) StartIndex() int {
	first, value := 0, gon.inners[0].outer.value
	for i := range gon.inners {
		if gon.inners[i].outer.value < value {
			first = i
			value = gon.inners[i].outer.value
		}
	}
	return first
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
func (gon *NGon) Set(index int, triple []int) {
	gon.outers[index].value = triple[0]
	gon.outers[index].inner.value = triple[1]
	gon.outers[index].inner.inner.value = triple[2]
}
func (gon *NGon) Get(index int) []int {
	return []int{
		gon.outers[index].value,
		gon.outers[index].inner.value,
		gon.outers[index].inner.inner.value,
	}
}
func (gon *NGon) Copy() *NGon {
	newgon := NewNGon(len(gon.inners))
	for i := range gon.inners {
		newgon.Set(i, gon.Get(i))
	}
	return newgon
}
func (gon *NGon) ToInt() (int64, error) {
	number := ""
	offset := gon.StartIndex()
	for i := range gon.inners {
		j := (i + offset) % len(gon.inners)
		number += fmt.Sprintf("%d%d%d",
			gon.outers[j].value,
			gon.outers[j].inner.value,
			gon.outers[j].inner.inner.value)
	}
	return strconv.ParseInt(number, 10, 64)
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
	src              []int
	dest             [][]int
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
func NewIntPermutation(set []int, permutation_size int) (IntPermutation, error) {
	set_size := len(set)
	num_permutations, err := NumPermutations(set_size, permutation_size)
	if err != nil {
		return IntPermutation{}, err
	}
	result := IntPermutation{
		permutation_size: permutation_size,
		src:              make([]int, set_size),
		dest:             make([][]int, num_permutations),
	}
	for i, value := range set {
		result.src[i] = value
	}
	for i := range result.dest {
		result.dest[i] = make([]int, permutation_size)
	}
	return result, nil
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
	for i := set_size; i > set_size-permutation_size; i-- {
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
			set.Copy(i, start+j, col)
		}
		if col+1 < permutation_size {
			used[i] = true
			permute(set, used, col+1, start, start+reps,
				num_unused-1)
			used[i] = false
		}
		start += reps
	}
}

// Go doesn't provide sorting methods for int64.
// Int64Slice attaches the methods of Interface to []int64, sorting in increasing order.
type Int64Slice []int64

func (p Int64Slice) Len() int           { return len(p) }
func (p Int64Slice) Less(i, j int) bool { return p[i] < p[j] }
func (p Int64Slice) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

// Recursively fill an NGon, returning an array of filled NGons.
func fillNGon(gon *NGon, sum, index_to_fill int, used []bool) []NGon {
	if index_to_fill == len(gon.outers) {
		for i, val := range used {
			if !val {
				err := (fmt.Sprintf("%d is unused: %v\n%v\n", i,
					used, gon))
				fmt.Print(err)
				panic(err)
			}
		}
		return []NGon{*gon}
	}

	// We're constructing a triple [X, Y, Z].  Y is already set from the
	// previous triple.  X + Y + Z == sum.
	results := make([]NGon, 0)
	y := gon.Get(index_to_fill - 1)[2]
NUMBER:
	for x := range used {
		z := sum - (x + y)
		if x == y || x == z || y == z {
			continue NUMBER
		}
		if z >= len(used) || z <= 0 {
			continue NUMBER
		}
		if used[x] {
			continue NUMBER
		}
		// 10 must be in the outer ring.
		if z == 10 {
			continue NUMBER
		}
		// When filling the final triple, z will already have been used.
		if used[z] {
			if index_to_fill != len(gon.outers)-1 {
				continue NUMBER
			}
			// Check that the calculated z equals y from the first
			// triple.
			if z != gon.Get(0)[1] {
				continue NUMBER
			}
		}

		// This triple has passed the checks, recurse and see if
		// we can fill the rest of the NGon.
		used[x] = true
		used[z] = true
		newgon := gon.Copy()
		newgon.Set(index_to_fill, []int{x, y, z})
		results = append(results, fillNGon(newgon, sum,
			index_to_fill+1, used)...)
		used[x] = false
		// It is incorrect to mark z as unused when filling the last
		// triple, because it's being used for the second time, and
		// marking it unused would let it be used in other triples,
		// resulting in it being used in the first, Nth, and final
		// triples.
		if index_to_fill != len(gon.outers)-1 {
			used[z] = false
		}
	}
	return results
}

func projectEuler68() {
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	set, err := NewIntPermutation(numbers, 3)
	if err != nil {
		log.Fatalln(err)
	}
	Permute(&set)
	ngons := make([]NGon, 0)
	ngon_size := len(numbers) / 2
	// We won't consider triples whose first value is lower than this;
	// either the NGon would not be the answer or we would find it from
	// another starting triple.
	best_start_digit := 0

TRIPLE:
	for i := set.NumPermutations() - 1; i >= 0; i-- {
		triple := set.dest[i]
		if triple[0] > 6 {
			// The NGon would start with a lower number.
			continue TRIPLE
		}
		if triple[0] < best_start_digit {
			// We have a better answer already.
			continue TRIPLE
		}
		if triple[1] == 10 || triple[2] == 10 {
			// 10 must be in the outer ring.
			continue TRIPLE
		}
		for _, ngon := range ngons {
			if ngon.ContainsTriple(triple) {
				continue TRIPLE
			}
		}

		newgon := NewNGon(ngon_size)
		newgon.Set(0, triple)
		sum := 0
		used := make([]bool, len(set.src)+1)
		// We'll never use 0, but marking it used here simplifies the
		// logic later.
		used[0] = true
		for _, num := range triple {
			sum += num
			used[num] = true
		}
		gons := fillNGon(newgon, sum, 1, used)
		ngons = append(ngons, gons...)
		for _, gon := range gons {
			i := gon.StartIndex()
			if gon.outers[i].value > best_start_digit {
				best_start_digit = gon.outers[i].value
			}
		}
	}

	sort_me := make([]int64, len(ngons))
	for i, gon := range ngons {
		value, err := gon.ToInt()
		if err != nil {
			log.Fatalln(err)
		}
		sort_me[i] = value
	}
	sort.Sort(Int64Slice(sort_me))
	if len(sort_me) == 0 {
		log.Fatalln("No results found :/")
	}
	fmt.Println(sort_me[len(sort_me)-1])
}

func main() {
	projectEuler67()
	projectEuler68()
}
