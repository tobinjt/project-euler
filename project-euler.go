/*
Solutions to Project Euler problems.
*/
package main

import (
	"bufio"
	"bytes"
	"encoding/csv"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"math"
	"math/big"
	"os"
	"runtime/pprof"
	"sort"
	"strconv"
	"strings"
	"time"
)

var _ = time.Now()
var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")
var memprofile = flag.String("memprofile", "", "write memory profile to this file")

// A function for ad-hoc code during development.
func test() int64 {
	return int64(0)
}

// A dummy function to be called during testing of realMain.
func fortesting() int64 {
	return 0
}

func main() {
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	result, err := realMain(flag.Args())
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println(result)
	// TODO(johntobin): is this too late?  Have we cleaned up the memory we allocated?
	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.WriteHeapProfile(f)
		f.Close()
	}
}

func realMain(args []string) (int64, error) {
	functions := map[string]func() int64{
		"67":         projectEuler67,
		"68":         projectEuler68,
		"69":         projectEuler69,
		"70":         projectEuler70,
		"71":         projectEuler71,
		"72":         projectEuler72,
		"73":         projectEuler73,
		"74":         projectEuler74,
		"75":         projectEuler75,
		"76":         projectEuler76,
		"77":         projectEuler77,
		"78":         projectEuler78,
		"80":         projectEuler80,
		"81":         projectEuler81,
		"85":         projectEuler85,
		"89":         projectEuler89,
		"92":         projectEuler92,
		"test":       test,
		"fortesting": fortesting,
	}
	if len(args) != 1 || functions[args[0]] == nil {
		keys := []string{}
		for key, _ := range functions {
			if key != "fortesting" {
				keys = append(keys, key)
			}
		}
		sort.Strings(keys)
		return 0, errors.New("Only 1 arg accepted from this list: " +
			strings.Join(keys, " "))
	}
	return functions[args[0]](), nil
}

func breakpoint() string {
	return fmt.Sprint("breakpoint reached")
}

// Shut up about unused import.
var _ = fmt.Println

/*
* Read all the lines from a filehandle.
* Note that if there isn't a newline on the last line an error will be returned.
 */
func readLinesFromFile(fh io.Reader) ([]string, error) {
	s := bufio.NewScanner(fh)
	var lines []string
	for s.Scan() {
		lines = append(lines, s.Text())
	}
	if err := s.Err(); err != nil {
		return nil, err
	}
	return lines, nil
}

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

// Parse a file containing a triangle of numbers.
func parseTriangle(fh io.Reader) ([][]int, error) {
	lines, err := readLinesFromFile(fh)
	if err != nil {
		return nil, err
	}
	triangle := make([][]int, 0)
	for _, line := range lines {
		line = strings.TrimRight(line, "\n")
		numbers := make([]int, 0)
		for _, ascii_number := range strings.Fields(line) {
			parsed_number, err := strconv.Atoi(ascii_number)
			if err != nil {
				// Parsing error
				return nil, err
			}
			numbers = append(numbers, parsed_number)
		}
		triangle = append(triangle, numbers)
	}
	return triangle, nil
}

func projectEuler67() int64 {
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
	return int64(triangle[0][0])
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
	// self.permutation_size
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
func NewIntPermutation(set []int, permutation_size int) IntPermutation {
	set_size := len(set)
	num_permutations := NumPermutations(set_size, permutation_size)
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
	return result
}

/*
* Calculates the number of permutations that would be generated.
* Args:
*  set_size: the number of elements in the set.
*  permutation_size: the number of elements in each permutation.
 */
func NumPermutations(set_size, permutation_size int) int {
	result := 1
	for i := set_size; i > set_size-permutation_size; i-- {
		result *= i
	}
	return result
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

func projectEuler68() int64 {
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	set := NewIntPermutation(numbers, 3)
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
		// It's not good to ignore the error, but I know it cannot
		// happen, because I generate everything.
		value, _ := gon.ToInt()
		sort_me[i] = value
	}
	sort.Sort(Int64Slice(sort_me))
	return sort_me[len(sort_me)-1]
}

/*
* Euler's Totient function, φ(n) [sometimes called the phi function], is used to
* determine the number of numbers less than n which are relatively prime to n.
* For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively
* prime to nine, φ(9)=6.
*
* [See table in original description.]
* It can be seen that n=6 produces a maximum n/φ(n) for n  10.
*
* Find the value of n  1,000,000 for which n/φ(n) is a maximum.
 */
/*
* You calculate phi(N) with Euler's Totient function:
* http://en.wikipedia.org/wiki/Totient_function
* - phi(N) = N(product of (1-1/p) where p is a prime divisor of N)
* - N/phi(N) = (product of (1-1/p) where p is a prime divisor of N)
* Note that to get a larger N/phi(N) does not require a larger N, it requires
* more prime factors, and the best way to get more prime factors is to multiply
* all the small primes together.  Calculate the prime numbers less than
* sqrt(1000000), then multiply them together to find the smallest product of
* primes less than 1000000.
 */

// Generate a prime table using Sieve of Erastosthenes.
func SieveOfEratosthenes(size int) []bool {
	primes := make([]bool, size+1)
	for i := range primes {
		primes[i] = true
	}
	primes[0] = false
	primes[1] = false
	bound := int(math.Ceil(math.Sqrt(float64(size+1)))) + 1
	for i := 0; i < bound; i++ {
		if primes[i] {
			for multiple := i * 2; multiple <= size; multiple += i {
				primes[multiple] = false
			}
		}
	}
	return primes
}

// Generate a list of prime factors for a number.  Factors are not deduplicated.
func PrimeFactors(number int, sieve []bool) []int {
	factors := []int{}
	remainder := number
	if number <= 1 {
		return factors
	}
	for divisor, is_prime := range sieve {
		if remainder == 1 {
			break
		}
		if !is_prime {
			continue
		}
		if remainder%divisor != 0 {
			continue
		}
		for remainder%divisor == 0 {
			factors = append(factors, divisor)
			remainder /= divisor
		}
	}
	return factors
}

func projectEuler69() int64 {
	bound := 1000000
	primes := SieveOfEratosthenes(int(math.Ceil(math.Sqrt(float64(bound)))))
	result := 1
	for number, is_prime := range primes {
		if !is_prime {
			continue
		}
		if result*number > bound {
			break
		}
		result *= number
	}
	return int64(result)
}

/*
* Euler's Totient function, φ(n) [sometimes called the phi function], is used to
* determine the number of positive numbers less than or equal to n which are
* relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than
* nine and relatively prime to nine, φ(9)=6.
* The number 1 is considered to be relatively prime to every positive number, so
* φ(1)=1.
*
* Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation
* of 79180.
*
* Find the value of n, 1 <= n <= 10**7, for which φ(n) is a permutation of n and
* the ratio n/φ(n) produces a minimum.
 */
/*
* http://www.doc.ic.ac.uk/~mrh/330tutor/ch05s02.html
* - when n is a prime number (e.g. 2, 3, 5, 7, 11, 13), φ(n) = n-1.
* - when m and n are coprime, φ(m*n) = φ(m)*φ(n).
* - If the prime factorisation of n is given by n =p1e1*...*pnen, then
*   φ(n) = n *(1 - 1/p1)* ... (1 - 1/pn).
 */
/*
* To minimise n/φ(n), we need to:
* a) minimise the difference between n and φ(n), by minimising the number of
*    prime factors in n.
* b) maximise n so that the difference between n and φ(n) is a small fraction of
*    n.
* Pick a threshold, e.g. sqrt(bound)*1.5, find the primes below it, sort them in
* descending order, generate all the pairs, and test them.  If the bound is too
* low, increase it; if pairs don't work, try triples.
 */

// Check if the digits in two non-negative ints are permutations.
func IntsArePermutations(a, b int) bool {
	exists := make(map[int]int)
	for a > 0 {
		exists[int(a%10)]++
		a /= 10
	}
	for b > 0 {
		exists[int(b%10)]--
		b /= 10
	}
	for _, count := range exists {
		if count != 0 {
			return false
		}
	}
	return true
}

func projectEuler70actual(bound int) int64 {
	prime_bound := int(1.5 * math.Sqrt(float64(bound)))
	sieve := SieveOfEratosthenes(prime_bound + 1)
	primes := make([]int, 0)
	for prime, is_prime := range sieve {
		if !is_prime {
			continue
		}
		primes = append(primes, prime)
	}

	permutation := NewIntPermutation(primes, 2)
	Permute(&permutation)
	number := 0
	ratio := 10.0
	for _, pair := range permutation.dest {
		n := pair[0] * pair[1]
		if n > bound {
			continue
		}
		phi_n := (pair[0] - 1) * (pair[1] - 1)
		ratio_n := float64(n) / float64(phi_n)
		if ratio_n < ratio && IntsArePermutations(n, phi_n) {
			ratio = ratio_n
			number = n
		}
	}

	return int64(number)
}

func projectEuler70test() int64 {
	return projectEuler70actual(10 * 1000)
}

func projectEuler70() int64 {
	return projectEuler70actual(10 * 1000 * 1000)
}

/*
* Consider the fraction, n/d, where n and d are positive integers. If n<d and
* HCF(n,d)=1, it is called a reduced proper fraction.
*
* If we list the set of reduced proper fractions for d <= 8 in ascending order
* of size, we get:
*
* 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
* 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
*
* It can be seen that 2/5 is the fraction immediately to the left of 3/7.
*
* By listing the set of reduced proper fractions for d <= 1,000,000 in ascending
* order of size, find the numerator of the fraction immediately to the left of
* 3/7.
 */
/*
* We have upper (2/5) and lower (3/7) bounds, so we don't need to search the
* entire problem space.  We don't need to generate all the possible fractions
* between the lower and upper bounds: every time we find a fraction that is
* better than our current lower bound, we can tighten the lower bound, and the
* lower bound will eventually be our answer.  We do need to generate all
* denominators from 5 to 1,000,000; but for each denominator, the first
* numerator we use will be the numerator of the current lower bound, and we stop
* incrementing the numerator when the current fraction exceeds the upper bound.
* If any of the fractions we generate are better than the current lower bound,
* we replace the lower bound with that fraction.
 */

// Determine the GCD of two numbers.
func GreatestCommonDenominator(a, b int64) int64 {
	return big.NewInt(0).GCD(nil, nil, big.NewInt(a), big.NewInt(b)).Int64()
}

func projectEuler71() int64 {
	return projectEuler71actual(1000000)
}

func projectEuler71test() int64 {
	// This function is for testing projectEuler71actual, because it takes
	// about 30 seconds to do the real calculation.
	return projectEuler71actual(8)
}

func projectEuler71actual(max_denominator int64) int64 {
	upper_bound := big.NewRat(3, 7)
	answer := big.NewRat(1, 5)
	var denominator int64
	for denominator = 1; denominator <= max_denominator; denominator++ {
		numerator := answer.Num().Int64()
		current := big.NewRat(numerator, denominator)
		for upper_bound.Cmp(current) == 1 {
			if GreatestCommonDenominator(numerator, denominator) ==
				1 && answer.Cmp(current) == -1 {
				answer.Set(current)
			}
			numerator++
			current = big.NewRat(numerator, denominator)
		}
	}
	return answer.Num().Int64()
}

/*
* Consider the fraction, n/d, where n and d are positive integers. If n < d and
* HCF(n,d)=1, it is called a reduced proper fraction.
*
* If we list the set of reduced proper fractions for d <= 8 in ascending order
* of size, we get:
*
* 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
* 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
*
* It can be seen that there are 21 elements in this set.
*
* How many elements would be contained in the set of reduced proper fractions
* for d ≤ 1,000,000?
 */

/*
* A reduced proper fraction has gcd(numerator, denominator) == 1, which means
* the numerator is relatively prime to the denominator.  The number of reduced
* proper fractions with denominator D is the number of relatively prime integers
* less than D; this is Euler's Totient function, phi.  To calculate phi for a
* number, generate all the prime numbers less than the number, and multiply (1 -
* 1/prime) for all prime numbers (see
* http://en.wikipedia.org/wiki/Euler's_totient_function#Euler.27s_product_formula).
* Rather than calculating all the primes for each denominator, we can use a
* method similar to the Sieve of Eratosthenes:
* - initialise an array so that array[i] = i;
* - iterate over the array from 2 onwards; when array[i] == i, that's a prime
*   number, and we multiply every multiple of i by (1 - 1/i).
* Now we know phi(denominator) for every denominator, so we sum them to get the
* answer to the problem.
 */

// Generate a lookup table for Euler's Totient function, phi.  See description
// above.
func MakePhiLookupTable(size int) []int64 {
	table := make([]float64, size+1)
	for i := range table {
		table[i] = float64(i)
	}
	for i := range table {
		if i <= 1 || table[i] != float64(i) {
			// i is not prime.
			continue
		}
		for j := i; j <= size; j += i {
			tmp := float64(i)
			table[j] *= (1 - 1/tmp)
		}
	}
	result := make([]int64, size+1)
	for i, value := range table {
		result[i] = int64(value)
	}
	return result
}

func projectEuler72actual(size int) int64 {
	phi_table := MakePhiLookupTable(size)
	var total int64 = 0
	for _, value := range phi_table {
		total += value
	}
	// 1/1 is not a reduced proper fraction, because the numerator must be
	// less than the denominator.
	total -= phi_table[1]
	return total
}

func projectEuler72test() int64 {
	return projectEuler72actual(8)
}

func projectEuler72() int64 {
	return projectEuler72actual(1000000)
}

/*
* Consider the fraction, n/d, where n and d are positive integers. If n < d and
* HCF(n,d)=1, it is called a reduced proper fraction.
*
* If we list the set of reduced proper fractions for d <= 8 in ascending order of
* size, we get:
*
* 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
* 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
*
* It can be seen that there are 3 fractions between 1/3 and 1/2.
*
* How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper
* fractions for d <= 12,000?
 */

func projectEuler73actual(n int64) int64 {
	count := int64(0)
	lower := big.NewRat(1, 3)
	upper := big.NewRat(1, 2)
	// Use Farey Sequences (http://en.wikipedia.org/wiki/Farey_sequence)
	// This is taken mostly-unchanged from Wikipedia.
	// This is complete voodoo and I have no idea how it works :(
	var a, b, c, d int64
	a, b, c, d = 0, 1, 1, n
	for c <= n {
		k := int64((n + b) / d)
		a, b, c, d = c, d, k*c-a, k*d-b
		// With older versions of go, changing the denominator fails, so
		// create a new variable on each iteration.
		current := big.NewRat(a, b)
		if lower.Cmp(current) == -1 && upper.Cmp(current) == 1 {
			count++
		}
	}
	return count
}

func projectEuler73test() int64 {
	return projectEuler73actual(8)
}

func projectEuler73() int64 {
	return projectEuler73actual(12000)
}

/*
* The number 145 is well known for the property that the sum of the factorial of
* its digits is equal to 145:
*
* 1! + 4! + 5! = 1 + 24 + 120 = 145
*
* Perhaps less well known is 169, in that it produces the longest chain of
* numbers that link back to 169; it turns out that there are only three such
* loops that exist:
*
* 169 → 363601 → 1454 → 169
* 871 → 45361 → 871
* 872 → 45362 → 872
*
* It is not difficult to prove that EVERY starting number will eventually get
* stuck in a loop. For example,
*
* 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
* 78 → 45360 → 871 → 45361 (→ 871)
* 540 → 145 (→ 145)
*
* Starting with 69 produces a chain of five non-repeating terms, but the longest
* non-repeating chain with a starting number below one million is sixty terms.
*
* How many chains, with a starting number below one million, contain exactly
* sixty non-repeating terms?
*
 */

/*
* I started with heavy caching, and fought to get it right in all the possible
* cases, but then I found that removing the caching gave me a >25% performance
* boost.  The non-caching version is not just faster, it's much simpler and
* clearer.
 */

func CalculateFactorialSum(number int) int {
	factorials := []int{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}
	if number == 0 {
		return factorials[number]
	}
	sum := 0
	for number > 0 {
		digit := number % 10
		sum += factorials[digit]
		number /= 10
	}
	return sum
}

func CalculateFactorialChainLength(number int) int {
	sum := CalculateFactorialSum(number)
	// The problem tells us that the longest non-repeating chain contains 60
	// elements.
	chain := make([]int, 62)
	chain[0] = number
	chain[1] = sum
	chain_index := 2

	for {
		sum = CalculateFactorialSum(sum)
		// Check if we have found a loop.
		for i := 0; i < chain_index; i++ {
			if chain[i] == sum {
				return chain_index
			}
		}
		// Still no loop, extend the chain.
		chain[chain_index] = sum
		chain_index++
	}
}

func projectEuler74() int64 {
	count := 0
	for i := 1; i < 1000000; i++ {
		if CalculateFactorialChainLength(i) == 60 {
			count++
		}
	}
	return int64(count)
}

/*
* It turns out that 12 cm is the smallest length of wire that can be bent to
* form an integer sided right angle triangle in exactly one way, but there are
* many more examples.
*
* 12 cm: (3,4,5)
* 24 cm: (6,8,10)
* 30 cm: (5,12,13)
* 36 cm: (9,12,15)
* 40 cm: (8,15,17)
* 48 cm: (12,16,20)
*
* In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
* integer sided right angle triangle, and other lengths allow more than one
* solution to be found; for example, using 120 cm it is possible to form exactly
* three different integer sided right angle triangles.
*
* 120 cm: (30,40,50), (20,48,52), (24,45,51)
*
* Given that L is the length of the wire, for how many values of L ≤ 1,500,000
* can exactly one integer sided right angle triangle be formed?
*
 */

/*
* I'm going to take a similar approach to Sieve of Eratosthenes.  Initialise an
* array of 1.5M ints.  Start with 3, 4, 5, then generate all the child triples.
* Maintain a queue of children, sum the lengths of each child, and increment
* every multiple of that sum.  Eventually there will be no more children whose
* sum is <=1.5M.  Then count all the array elements whose value is 1, and that's
* the answer.
 */

type PythagoreanTriple struct {
	a, b, c int
}

// Generate three child PythagoreanTriple from a parent PythagoreanTriple.
// http://en.wikipedia.org/wiki/Pythagorean_triple#Parent.2Fchild_relationships
func (parent PythagoreanTriple) MakeChildren() []PythagoreanTriple {
	child1 := PythagoreanTriple{
		a: parent.a - (2 * parent.b) + (2 * parent.c),
		b: (2 * parent.a) - parent.b + (2 * parent.c),
		c: (2 * parent.a) - (2 * parent.b) + (3 * parent.c),
	}
	child2 := PythagoreanTriple{
		a: parent.a + (2 * parent.b) + (2 * parent.c),
		b: (2 * parent.a) + parent.b + (2 * parent.c),
		c: (2 * parent.a) + (2 * parent.b) + (3 * parent.c),
	}
	child3 := PythagoreanTriple{
		a: (-1 * parent.a) + (2 * parent.b) + (2 * parent.c),
		b: (-2 * parent.a) + parent.b + (2 * parent.c),
		c: (-2 * parent.a) + (2 * parent.b) + (3 * parent.c),
	}
	return []PythagoreanTriple{
		child1,
		child2,
		child3,
	}
}

func projectEuler75() int64 {
	return projectEuler75_actual(1500000)
}

func projectEuler75test() int64 {
	return projectEuler75_actual(100)
}

func projectEuler75_actual(upper_bound int) int64 {
	counts := make([]int, upper_bound+1)
	triples := make([]PythagoreanTriple, 1)
	triples[0] = PythagoreanTriple{a: 3, b: 4, c: 5}

	for index := 0; index < len(triples); index++ {
		triple := triples[index]
		children := triple.MakeChildren()
		for _, child := range children {
			sum := child.a + child.b + child.c
			if sum <= upper_bound {
				triples = append(triples, child)
			}
		}

		sum := triple.a + triple.b + triple.c
		multiple := sum
		for multiple <= upper_bound {
			counts[multiple]++
			multiple += sum
		}
	}

	result := 0
	for _, count := range counts {
		if count == 1 {
			result++
		}
	}
	return int64(result)
}

/*
* It is possible to write five as a sum in exactly six different ways:
*
* 4 + 1
* 3 + 2
* 3 + 1 + 1
* 2 + 2 + 1
* 2 + 1 + 1 + 1
* 1 + 1 + 1 + 1 + 1
*
* How many different ways can one hundred be written as a sum of at least two
* positive integers?
 */

/*
* Running NumIntegerPartitions(i, i-1) for 2 <= i <= 20 calls
* NumIntegerPartitions(1, 1) 1597 times.  Caching should be effective.
* Testing shows that PE 76 takes 0.5 seconds with caching, 88 seconds without
* caching.
 */

type IPArgs struct {
	number, max_component int
}

var IPResults map[IPArgs]int = make(map[IPArgs]int)

func NumIntegerPartitions(number, max_component int) int {
	result, exists := IPResults[IPArgs{number, max_component}]
	if exists {
		return result
	}
	if number <= 1 {
		return 1
	}
	sum := 0
	if max_component <= number {
		sum += NumIntegerPartitions(number-max_component, max_component)
	}
	if max_component > 1 {
		if max_component <= number {
			sum += NumIntegerPartitions(number, max_component-1)
		} else {
			sum += NumIntegerPartitions(number, number)
		}
	}
	IPResults[IPArgs{number, max_component}] = sum
	return sum
}

func projectEuler76test() int64 {
	return int64(NumIntegerPartitions(20, 19))
}
func projectEuler76() int64 {
	return int64(NumIntegerPartitions(100, 99))
}

/*
* It is possible to write ten as the sum of primes in exactly five different
* ways:
*
* 7 + 3
* 5 + 5
* 5 + 3 + 2
* 3 + 3 + 2 + 2
* 2 + 2 + 2 + 2 + 2
*
* What is the first value which can be written as the sum of primes in over five
* thousand different ways?
 */

/*
* The [formula for calculating the number of prime
* partitions](http://oeis.org/A000607) is voodoo, and it's not good for
* implementing an algorithm.  The formula can be transformed using [Euler's
* Transform](http://mathworld.wolfram.com/EulerTransform.html) into [something
* that can be implemented reasonably
* easily](http://math.stackexchange.com/a/89661).  I admit that I don't
* understand *why* or *how* either of these formulae work.
 */

func SumOfPrimeFactors(number int, sieve []bool) int {
	factors := PrimeFactors(number, sieve)
	sum := 0
	last_factor := 0
	for _, factor := range factors {
		if factor != last_factor {
			sum += factor
			last_factor = factor
		}
	}
	return sum
}

func SumOfPrimeFactors_Cached(number int, sieve []bool, sopf_cache map[int]int) int {
	result, exists := sopf_cache[number]
	if !exists {
		result = SumOfPrimeFactors(number, sieve)
		sopf_cache[number] = result
	}
	return result
}

func NumPrimePartitions(number int, sieve []bool, npp_cache map[int]int,
	sopf_cache map[int]int) int {
	if number == 1 {
		return 0
	}
	result, exists := npp_cache[number]
	if exists {
		return result
	}

	sum := SumOfPrimeFactors_Cached(number, sieve, sopf_cache)
	for j := 1; j < number; j++ {
		sopf_j := SumOfPrimeFactors_Cached(j, sieve, sopf_cache)
		npp_number_minus_j := NumPrimePartitions(number-j, sieve,
			npp_cache, sopf_cache)
		sum += npp_number_minus_j * sopf_j
	}
	result = sum / number
	// Without caching, the time taken to calculate NumPrimePartitions(n) is
	// O(2^n).  n=29 is when it takes longer than 1 minute on my laptop.
	npp_cache[number] = result
	return result
}

func projectEuler77actual(target int) int64 {
	sieve := SieveOfEratosthenes(100)
	npp_cache := make(map[int]int)
	sopf_cache := make(map[int]int)

	number, result := 0, 0
	for number = 1; result < target; number++ {
		result = NumPrimePartitions(number, sieve, npp_cache,
			sopf_cache)
	}
	return int64(number - 1)
}

func projectEuler77test() int64 {
	return projectEuler77actual(26)
}

func projectEuler77() int64 {
	return projectEuler77actual(5000)
}

/*
* Let p(n) represent the number of different ways in which n coins can be
* separated into piles. For example, five coins can separated into piles in
* exactly seven different ways, so p(5)=7.
*
* OOOOO
* OOOO   O
* OOO   OO
* OOO   O   O
* OO   OO   O
* OO   O   O   O
* O   O   O   O   O
* Find the least value of n for which p(n) is divisible by one million.
 */

/*
* Calling NumIntegerPartitions in a loop would work, except that performance
* falls off a cliff:
* 7.307216ms 6028 => -5051972599416803847
* 9.74248ms 6029 => 1953721510414805365
* 3.615389811s 6030 => -9113343080961881073
* 257.840184ms 6031 => 63892779937861297
* 227.194138ms 6032 => 8738253903924976475
* 223.567489ms 6033 => 5975595644718247526
 */

func PentagonalNumber(number int) int {
	return ((3 * number * number) - number) / 2
}

func GeneralisedPentagonalNumber(number int) int {
	// input:   0, 1,  2, 3,  4, 5,  6, 7,  8
	// becomes: 0, 1, -1, 2, -2, 3, -3, 4, -4
	if number%2 == 0 {
		return PentagonalNumber(number / -2)
	} else {
		return PentagonalNumber((number + 1) / 2)
	}
}

var IPresults2 map[int]*big.Int = map[int]*big.Int{0: big.NewInt(1)}

/*
* The formula is described in
* http://en.wikipedia.org/wiki/Partition_(number_theory)#Generating_function
* but it needs a bit of interpretation to get something that you can implement.
 */
func NumIntegerPartitions2(number int) *big.Int {
	result, exists := IPresults2[number]
	if exists {
		return result
	}
	// This is rotated one place to the right because we start with i=1
	// rather than i=0.
	signs := []*big.Int{big.NewInt(-1), big.NewInt(1), big.NewInt(1),
		big.NewInt(-1)}
	sum, i := big.NewInt(0), 0
	for {
		i++
		pentagonal_number := GeneralisedPentagonalNumber(i)
		if pentagonal_number > number {
			break
		}
		num_ip := NumIntegerPartitions2(number - pentagonal_number)
		temp := big.NewInt(0)
		temp.Mul(signs[i%len(signs)], num_ip)
		sum.Add(sum, temp)
	}
	IPresults2[number] = sum
	return sum
}

func projectEuler78actual(multiple int64) int64 {
	big_multiple := big.NewInt(multiple)
	modulus := big.NewInt(0)
	i := 0
	for {
		i++
		nip := NumIntegerPartitions2(i)
		modulus.Mod(nip, big_multiple)
		if modulus.Int64() == 0 {
			return int64(i)
		}
	}
}

func projectEuler78test() int64 {
	return projectEuler78actual(7)
}

func projectEuler78() int64 {
	return projectEuler78actual(1000 * 1000)
}

/*
* It is well known that if the square root of a natural number is not an
* integer, then it is irrational. The decimal expansion of such square roots is
* infinite without any repeating pattern at all.
*
* The square root of two is 1.41421356237309504880..., and the digital sum of
* the first one hundred decimal digits is 475.
*
* For the first one hundred natural numbers, find the total of the digital sums
* of the first one hundred decimal digits for all the irrational square roots.
 */

// http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Decimal_.28base_10.29
// The numbered comments in the function refer to the steps in the Wikipedia
// article.
func SqrtPE80(number, precision int) []int {
	digit_stack := []int{}
	for remainder := number; remainder > 0; remainder /= 10 {
		digit_stack = append(digit_stack, remainder%10)
	}
	// Pad the digit stack so that an odd number of digits is interpreted
	// as 0A, BC rather than AB, 0C.
	if len(digit_stack)%2 == 1 {
		digit_stack = append(digit_stack, 0)
	}
	digit_stack_i := len(digit_stack) - 1

	remainder, root_so_far := big.NewInt(0), big.NewInt(0)
	zero, one := big.NewInt(0), big.NewInt(1)
	ten, twenty := big.NewInt(10), big.NewInt(20)
	result := make([]int, 0)
	for len(result) < precision {
		// Step 1.
		current := big.NewInt(0)
		current.Set(remainder)
		for j := 0; j < 2; j++ {
			current = current.Mul(current, ten)
			if digit_stack_i >= 0 {
				current.Add(current,
					big.NewInt(int64(digit_stack[digit_stack_i])))
			}
			digit_stack_i--
		}

		// Step 2.
		next_digit, subtract_me := big.NewInt(0), big.NewInt(0)
		for {
			next_digit.Add(next_digit, one)
			temp := big.NewInt(0)
			temp.Set(root_so_far)
			temp.Mul(temp, twenty)
			temp.Add(temp, next_digit)
			temp.Mul(temp, next_digit)
			if temp.Cmp(current) == 1 {
				next_digit.Sub(next_digit, one)
				break
			}
			subtract_me.Set(temp)
		}

		// Step 3.
		remainder.Sub(current, subtract_me)
		root_so_far.Mul(root_so_far, ten)
		root_so_far.Add(root_so_far, next_digit)
		result = append(result, int(next_digit.Int64()))

		// Step 4.
		if remainder.Cmp(zero) == 0 && digit_stack_i < 0 {
			break
		}
	}
	return result
}

func projectEuler80actual(upper_bound int) int64 {
	result := 0
	for i := 0; i <= upper_bound; i++ {
		sqrt := SqrtPE80(i, 100)
		// Irrational roots will have more than a single digit.  I'm
		// cheating somewhat because I know this function won't be
		// called with an upper bound >99.
		if len(sqrt) > 1 {
			for j := 0; j < len(sqrt); j++ {
				result += sqrt[j]
			}
		}
	}
	return int64(result)
}

func projectEuler80test() int64 {
	return projectEuler80actual(2)
}

func projectEuler80() int64 {
	return projectEuler80actual(99)
}

/*
* In the 5 by 5 matrix below, the minimal path sum from the top left to the
* bottom right, by only moving to the right and down, is indicated in bold red
* and is equal to 2427.
*
* 131 637 234 103  18
* 201  96 342 965 150
* 630 803 746 422 111
* 537 699 497 121 956
* 805 732 524  37 331
*
* Find the minimal path sum, in matrix.txt (right click and "Save Link/Target
* As..."), a 31K text file containing a 80 by 80 matrix, from the top left to
* the bottom right by only moving right and down.
 */

func readIntsFromCSVFile(r io.Reader) ([][]uint64, error) {
	csvr := csv.NewReader(r)
	ints := [][]uint64{}
	rows, err := csvr.ReadAll()
	if err != nil {
		return nil, err
	}

	for i, row := range rows {
		ints = append(ints, make([]uint64, len(row)))
		for j, s := range row {
			ints[i][j], err = strconv.ParseUint(s, 10, 64)
			if err != nil {
				return nil, err
			}
		}
	}
	return ints, nil
}

func projectEuler81actual(r io.Reader) int64 {
	matrix, err := readIntsFromCSVFile(r)
	size := len(matrix)
	if err != nil {
		return -1
	}
	grid := make([][]uint64, size)
	// There's only one way to fill the first row or column, so prefill them and make filling the remainder easier.
	grid[0] = make([]uint64, size)
	grid[0][0] = matrix[0][0]
	for i := 1; i < size; i++ {
		grid[i] = make([]uint64, size)
		grid[i][0] = grid[i-1][0] + matrix[i][0]
		grid[0][i] = grid[0][i-1] + matrix[0][i]
	}
	// Each square in the grid is min(grid square above, grid square left) + matrix square.  The bottom right grid square is the sum of the minimum path.
	for i := 1; i < size; i++ {
		for j := 1; j < size; j++ {
			a, b := grid[i-1][j], grid[i][j-1]
			if a < b {
				grid[i][j] = a + matrix[i][j]
			} else {
				grid[i][j] = b + matrix[i][j]
			}
		}
	}
	return int64(grid[len(grid)-1][len(grid[0])-1])
}

func projectEuler81test() int64 {
	data := `131,637,234,103,18
201,96,342,965,150
630,803,746,422,111
537,699,497,121,956
805,732,524,37,331`
	return projectEuler81actual(strings.NewReader(data))
}

func projectEuler81() int64 {
	fd, err := os.Open("matrix.txt")
	if err != nil {
		return -1
	}
	return projectEuler81actual(fd)
}

/*
* The minimal path sum in the 5 by 5 matrix below, by starting in any cell in
* the left column and finishing in any cell in the right column, and only moving
* up, down, and right, is indicated in red and bold; the sum is equal to 994.
*
* 131 637 234 103  18
* 201  96 342 965 150
* 630 803 746 422 111
* 537 699 497 121 956
* 805 732 524  37 331
*
* Find the minimal path sum, in matrix.txt (right click and "Save Link/Target
* As..."), a 31K text file containing a 80 by 80 matrix, from the left column to
* the right column.
 */

func projectEuler82actual(r io.Reader) int64 {
	return 0
}

func projectEuler82test() int64 {
	data := `131,637,234,103,18
201,96,342,965,150
630,803,746,422,111
537,699,497,121,956
805,732,524,37,331`
	return projectEuler82actual(strings.NewReader(data))
}

func projectEuler82() int64 {
	fd, err := os.Open("matrix.txt")
	if err != nil {
		return -1
	}
	return projectEuler82actual(fd)
}

/*
* For a number written in Roman numerals to be considered valid there are basic
* rules which must be followed. Even though the rules allow some numbers to be
* expressed in more than one way there is always a "best" way of writing a
* particular number.
*
* For example, it would appear that there are at least six ways of writing the
* number sixteen:
*
* IIIIIIIIIIIIIIII
* VIIIIIIIIIII
* VVIIIIII
* XIIIIII
* VVVI
* XVI
*
* However, according to the rules only XIIIIII and XVI are valid, and the last
* example is considered to be the most efficient, as it uses the least number of
* numerals.
*
* The 11K text file, roman.txt (right click and 'Save Link/Target As...'),
* contains one thousand numbers written in valid, but not necessarily minimal,
* Roman numerals; see About... Roman Numerals for the definitive rules for this
* problem.
*
* Find the number of characters saved by writing each of these in their minimal
* form.
*
* Note: You can assume that all the Roman numerals in the file contain no more
* than four consecutive identical units.
 */

type romanNumeral struct {
	numerals string
	value    uint
}

var allRomanNumerals = []romanNumeral{
	{"M", 1000},
	{"CM", 900},
	{"D", 500},
	{"CD", 400},
	{"C", 100},
	{"XC", 90},
	{"L", 50},
	{"XL", 40},
	{"X", 10},
	{"IX", 9},
	{"V", 5},
	{"IV", 4},
	{"I", 1},
}

func romanNumeralsToUint(s string) (uint, error) {
	if len(s) == 0 {
		return 0, errors.New("empty string is invalid")
	}
	var result uint
	pointer := s
	smallest := allRomanNumerals[0]
	for len(pointer) > 0 {
		matched := false
		for _, r := range allRomanNumerals {
			if !strings.HasPrefix(pointer, r.numerals) {
				continue
			}
			if r.value > smallest.value {
				return 0, errors.New("sequence \"" + r.numerals + "\" followed smaller sequence \"" + smallest.numerals + "\"")
			}
			result += r.value
			pointer = pointer[len(r.numerals):]
			smallest = r
			matched = true
			break
		}
		if !matched {
			return 0, errors.New("unrecognised sequence: " + pointer)
		}
	}
	return result, nil
}

func uintToRomanNumerals(n uint) (string, error) {
	if n == 0 {
		return "", errors.New("0 is invalid")
	}
	var numerals []string
	for _, r := range allRomanNumerals {
		for r.value <= n {
			n -= r.value
			numerals = append(numerals, r.numerals)
		}
	}
	return strings.Join(numerals, ""), nil
}

func projectEuler89actual(fh io.Reader) int64 {
	lines, err := readLinesFromFile(fh)
	if err != nil {
		panic("Reading lines failed!")
	}
	saved := 0
	for _, line := range lines {
		value, err := romanNumeralsToUint(line)
		if err != nil {
			panic(fmt.Sprintf("romanNumeralsToUint(%v) failed: %v", line, err))
		}
		numerals, err := uintToRomanNumerals(value)
		if err != nil {
			panic(fmt.Sprintf("uintToRomanNumerals(%v) failed: %v", value, err))
		}
		saved += (len(line) - len(numerals))
	}
	return int64(saved)
}

func projectEuler89test() int64 {
	fh := bytes.NewBufferString("VVIIIIII")
	return projectEuler89actual(fh)
}

func projectEuler89() int64 {
	fh, err := os.Open("roman.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	return projectEuler89actual(fh)
}

/*
* By counting carefully it can be seen that a rectangular grid measuring 3 by 2
* contains eighteen rectangles:
* [Diagram missing, see https://projecteuler.net/problem=85]
* Although there exists no rectangular grid that contains exactly two million
* rectangles, find the area of the grid with the nearest solution.
 */

/*
* To choose a rectangle from an MxN grid, you choose 2 lines from the M+1
* horizontal lines, and choose 2 lines from the N+1 vertical lines, so there are
* C(M+1, 2) * C(N+1, 2) possible rectangles in an MxN grid.  Once the number of
* possible rectangles exceeds two million there's no point in increasing M or N.
* M should always be >= N, because any grid where M<N can be rotated 90 degrees
* so that M>N.
 */

func numCombinations(n, k uint64) uint64 {
	// TODO(johntobin): error checking of args?
	n_fac := big.NewInt(0).MulRange(1, int64(n))
	k_fac := big.NewInt(0).MulRange(1, int64(k))
	n_k_fac := big.NewInt(0).MulRange(1, int64(n-k))
	combs := n_fac.Div(n_fac, k_fac.Mul(k_fac, n_k_fac))
	return uint64(combs.Int64())
}

func difference(a, b uint64) uint64 {
	if a > b {
		return a - b
	}
	return b - a
}

func projectEuler85actual() int64 {
	const target uint64 = 2000 * 1000
	var best_m, best_n, best_combs uint64
	// TODO(johntobin): hardcoding the upper bound is bad, how do I avoid that?
	// TODO(johntobin): it would be nicer if the test had a different target.
	for m := uint64(5); m < 80; m++ {
		for n := uint64(5); n <= m; n++ {
			combs := numCombinations(m+1, 2) * numCombinations(n+1, 2)
			if difference(target, combs) < difference(target, best_combs) {
				// fmt.Printf("%v %v %v %v better than %v %v %v %v\n", m, n, combs, difference(target, combs), best_m, best_n, best_combs, difference(target, best_combs))
				best_combs = combs
				best_m = m
				best_n = n
			}
		}
	}
	return int64(best_m * best_n)
}

func projectEuler85test() int64 {
	return projectEuler85actual()
}

func projectEuler85() int64 {
	return projectEuler85actual()
}

/*
 * A number chain is created by continuously adding the square of the digits in
 * a number to form a new number until it has been seen before.
 *
 * For example,
 *
 * 44 → 32 → 13 → 10 → 1 → 1
 * 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
 *
 * Therefore any chain that arrives at 1 or 89 will become stuck in an endless
 * loop. What is most amazing is that EVERY starting number will eventually
 * arrive at 1 or 89.
 *
 * How many starting numbers below ten million will arrive at 89?
 */

/*
 * The biggest possible number to check is 9999999 -> 81*7 -> 567, so we
 * maintain a cache of 568 elements mapping to 1 or 89.
 */
const _SQUARE_CHAIN_CACHE_SIZE uint = 568

/*
* Return the last element in the square chain for number, either 1 or 89.
* cache is used to short circuit following the chain and is updated.
 */
func squareChain(number uint, cache []uint) uint {
	if number < _SQUARE_CHAIN_CACHE_SIZE && cache[number] != 0 {
		return cache[number]
	}
	var sum uint
	num := number
	for num != 0 {
		digit := num % 10
		num = num / 10
		sum += digit * digit
	}
	result := squareChain(sum, cache)
	if number < _SQUARE_CHAIN_CACHE_SIZE {
		cache[number] = result
	}
	return result
}

func projectEuler92actual(limit uint) int64 {
	cache := make([]uint, _SQUARE_CHAIN_CACHE_SIZE)
	cache[1] = 1
	cache[89] = 89
	count_1, count_89 := 0, 0
	for i := uint(1); i < limit; i++ {
		result := squareChain(i, cache)
		if result == 1 {
			count_1++
		} else {
			count_89++
		}
	}
	return int64(count_89)
}

func projectEuler92test() int64 {
	return projectEuler92actual(10)
}

func projectEuler92() int64 {
	return projectEuler92actual(10 * 1000 * 1000)
}
