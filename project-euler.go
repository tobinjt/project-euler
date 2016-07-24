/*
Solutions to Project Euler problems.
*/
package main

import (
	"bufio"
	"bytes"
	"container/heap"
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"math/big"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/skelterjohn/go.matrix"
)

var _ = time.Now()

// A function for ad-hoc code during development.
func test() int64 {
	return int64(0)
}

// A dummy function to be called during testing of realMain.
func fortesting() int64 {
	return 0
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
		"82":         projectEuler82,
		"83":         projectEuler83,
		"84":         projectEuler84,
		"85":         projectEuler85,
		"87":         projectEuler87,
		"88":         projectEuler88,
		"89":         projectEuler89,
		"92":         projectEuler92,
		"97":         projectEuler97,
		"99":         projectEuler99,
		"test":       test,
		"fortesting": fortesting,
	}
	if len(args) != 1 || functions[args[0]] == nil {
		keys := []string{}
		for key := range functions {
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

func openOrDie(file string) io.Reader {
	fh, err := os.Open(file)
	if err != nil {
		panic(fmt.Sprintf("openOrDie(): %v", err))
	}
	return fh
}

/*
* Read all the lines from a filehandle.
* Note that if there isn't a newline on the last line an error will be raised.
 */
func readLinesFromFile(fh io.Reader) []string {
	s := bufio.NewScanner(fh)
	var lines []string
	for s.Scan() {
		lines = append(lines, s.Text())
	}
	if err := s.Err(); err != nil {
		panic(fmt.Sprintf("Reading lines failed: %v", err))
	}
	return lines
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
func parseTriangle(fh io.Reader) [][]int {
	lines := readLinesFromFile(fh)
	var triangle [][]int
	for _, line := range lines {
		line = strings.TrimRight(line, "\n")
		var numbers []int
		for _, asciiNumber := range strings.Fields(line) {
			parsedNumber, err := strconv.Atoi(asciiNumber)
			if err != nil {
				// Parsing error
				panic(fmt.Sprintf("Parse error: %v; line: \"%v\"", err, line))
			}
			numbers = append(numbers, parsedNumber)
		}
		triangle = append(triangle, numbers)
	}
	return triangle
}

func projectEuler67() int64 {
	fh, err := os.Open("triangle.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	triangle := parseTriangle(fh)
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

// NGonOuter is an external node; see description above.
type NGonOuter struct {
	value int
	inner *NGonInner
}

// NGonInner is an internal node; see description above.
type NGonInner struct {
	value int
	inner *NGonInner
	outer *NGonOuter
}

// NGon holds all the NGonOuter and NGonInner structs; see description above.
type NGon struct {
	inners []NGonInner
	outers []NGonOuter
}

func (inner NGonInner) String() string {
	return fmt.Sprintf("%v,%v,%v", inner.outer.value, inner.value,
		inner.inner.value)
}
func (gon NGon) String() string {
	var results []string
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

// NewNGon creates a new NGon, creates the necessary NGonInner and NGonOuter structs, and creates the links between them.
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

// StartIndex returns the first index to use when processing an NGon in ToInt.
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

// Set updates the NGon at index using the values in triple.
func (gon *NGon) Set(index int, triple []int) {
	gon.outers[index].value = triple[0]
	gon.outers[index].inner.value = triple[1]
	gon.outers[index].inner.inner.value = triple[2]
}

// Get returns the numbers making the NGon at index.
func (gon *NGon) Get(index int) []int {
	return []int{
		gon.outers[index].value,
		gon.outers[index].inner.value,
		gon.outers[index].inner.inner.value,
	}
}

// Copy creates a deep copy of an NGon.
func (gon *NGon) Copy() *NGon {
	newgon := NewNGon(len(gon.inners))
	for i := range gon.inners {
		newgon.Set(i, gon.Get(i))
	}
	return newgon
}

// ToInt produces the integer representation of an NGon.
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

// Permutable is the interface for permutable arrays.
type Permutable interface {
	// self.dest[destI][destJ] = self.src[srcI].
	Copy(srcI, destI, destJ int)
	// len(self.dest)
	NumPermutations() int
	// self.permutationSize
	PermutationSize() int
	// len(self.dest)
	SetSize() int
}

// IntPermutation impements Permutable for ints.
type IntPermutation struct {
	src             []int
	dest            [][]int
	permutationSize int
}

// Copy from source to destination.
func (set *IntPermutation) Copy(srcI, destI, destJ int) {
	set.dest[destI][destJ] = set.src[srcI]
}

// NumPermutations returns the number of permutations.
func (set *IntPermutation) NumPermutations() int {
	return len(set.dest)
}

// PermutationSize is the size of the permutation to be generated from the inputs.
func (set *IntPermutation) PermutationSize() int {
	return set.permutationSize
}

// SetSize returns the size of the set.
func (set *IntPermutation) SetSize() int {
	return len(set.src)
}

// NewIntPermutation creates a new Int permutation.
func NewIntPermutation(set []int, permutationSize int) IntPermutation {
	setSize := len(set)
	numPermutations := NumPermutations(setSize, permutationSize)
	result := IntPermutation{
		permutationSize: permutationSize,
		src:             make([]int, setSize),
		dest:            make([][]int, numPermutations),
	}
	for i, value := range set {
		result.src[i] = value
	}
	for i := range result.dest {
		result.dest[i] = make([]int, permutationSize)
	}
	return result
}

// NumPermutations calculates the number of permutations that would be generated.
// Args:
//  setSize: the number of elements in the set.
//  permutationSize: the number of elements in each permutation.
func NumPermutations(setSize, permutationSize int) int {
	result := 1
	for i := setSize; i > setSize-permutationSize; i-- {
		result *= i
	}
	return result
}

// Permute generates all the permutations.
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
*  numUnused: how many elements are unused.
 */
func permute(set Permutable, used []bool, col, start, end, numUnused int) {
	reps := (end - start) / numUnused
	permutationSize := set.PermutationSize()
	for i := range used {
		if used[i] {
			continue
		}
		for j := 0; j < reps; j++ {
			set.Copy(i, start+j, col)
		}
		if col+1 < permutationSize {
			used[i] = true
			permute(set, used, col+1, start, start+reps,
				numUnused-1)
			used[i] = false
		}
		start += reps
	}
}

// Int64Slice attaches the methods of Interface to []int64, sorting in increasing order.
// Go doesn't provide sorting methods for int64.
type Int64Slice []int64

func (p Int64Slice) Len() int           { return len(p) }
func (p Int64Slice) Less(i, j int) bool { return p[i] < p[j] }
func (p Int64Slice) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

// Recursively fill an NGon, returning an array of filled NGons.
func fillNGon(gon *NGon, sum, indexToFill int, used []bool) []NGon {
	if indexToFill == len(gon.outers) {
		return []NGon{*gon}
	}

	// We're constructing a triple [X, Y, Z].  Y is already set from the
	// previous triple.  X + Y + Z == sum.
	var results []NGon
	y := gon.Get(indexToFill - 1)[2]
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
			if indexToFill != len(gon.outers)-1 {
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
		newgon.Set(indexToFill, []int{x, y, z})
		results = append(results, fillNGon(newgon, sum,
			indexToFill+1, used)...)
		used[x] = false
		// It is incorrect to mark z as unused when filling the last
		// triple, because it's being used for the second time, and
		// marking it unused would let it be used in other triples,
		// resulting in it being used in the first, Nth, and final
		// triples.
		if indexToFill != len(gon.outers)-1 {
			used[z] = false
		}
	}
	return results
}

func projectEuler68() int64 {
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	set := NewIntPermutation(numbers, 3)
	Permute(&set)
	var ngons []NGon
	ngonSize := len(numbers) / 2
	// We won't consider triples whose first value is lower than this;
	// either the NGon would not be the answer or we would find it from
	// another starting triple.
	bestStartDigit := 0

TRIPLE:
	for i := set.NumPermutations() - 1; i >= 0; i-- {
		triple := set.dest[i]
		if triple[0] > 6 {
			// The NGon would start with a lower number.
			continue TRIPLE
		}
		if triple[0] < bestStartDigit {
			// We have a better answer already.
			continue TRIPLE
		}
		if triple[1] == 10 || triple[2] == 10 {
			// 10 must be in the outer ring.
			continue TRIPLE
		}

		newgon := NewNGon(ngonSize)
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
			if gon.outers[i].value > bestStartDigit {
				bestStartDigit = gon.outers[i].value
			}
		}
	}

	sortMe := make([]int64, len(ngons))
	for i, gon := range ngons {
		// It's not good to ignore the error, but I know it cannot
		// happen, because I generate everything.
		value, _ := gon.ToInt()
		sortMe[i] = value
	}
	sort.Sort(Int64Slice(sortMe))
	return sortMe[len(sortMe)-1]
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
* http://en.wikipedia.org/wiki/TotientFunction
* - phi(N) = N(product of (1-1/p) where p is a prime divisor of N)
* - N/phi(N) = (product of (1-1/p) where p is a prime divisor of N)
* Note that to get a larger N/phi(N) does not require a larger N, it requires
* more prime factors, and the best way to get more prime factors is to multiply
* all the small primes together.  Calculate the prime numbers less than
* sqrt(1000000), then multiply them together to find the smallest product of
* primes less than 1000000.
 */

// SieveOfEratosthenes generates a prime table using Sieve of Erastosthenes.
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

// PrimeFactors generates a list of prime factors for a number.  Factors are not deduplicated.
func PrimeFactors(number int, sieve []bool) []int {
	factors := []int{}
	remainder := number
	if number <= 1 {
		return factors
	}
	for divisor, isPrime := range sieve {
		if remainder == 1 {
			break
		}
		if !isPrime {
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
	for number, isPrime := range primes {
		if !isPrime {
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

// IntsArePermutations checks if the digits in two non-negative ints are permutations.
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
	primeBound := int(1.5 * math.Sqrt(float64(bound)))
	sieve := SieveOfEratosthenes(primeBound + 1)
	var primes []int
	for prime, isPrime := range sieve {
		if !isPrime {
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
		phiN := (pair[0] - 1) * (pair[1] - 1)
		ratioN := float64(n) / float64(phiN)
		if ratioN < ratio && IntsArePermutations(n, phiN) {
			ratio = ratioN
			number = n
		}
	}

	return int64(number)
}

func projectEuler70test() int64 {
	return projectEuler70actual(10 * 1000)
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

// GreatestCommonDenominator determines the GCD of two numbers.
func GreatestCommonDenominator(a, b int64) int64 {
	return big.NewInt(0).GCD(nil, nil, big.NewInt(a), big.NewInt(b)).Int64()
}

func projectEuler71test() int64 {
	// This function is for testing projectEuler71actual, because it takes
	// about 30 seconds to do the real calculation.
	return projectEuler71actual(8)
}

func projectEuler71actual(maxDenominator int64) int64 {
	upperBound := big.NewRat(3, 7)
	answer := big.NewRat(1, 5)
	var denominator int64
	for denominator = 1; denominator <= maxDenominator; denominator++ {
		numerator := answer.Num().Int64()
		current := big.NewRat(numerator, denominator)
		for upperBound.Cmp(current) == 1 {
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
* http://en.wikipedia.org/wiki/Euler'sTotientFunction#Euler.27sProductFormula).
* Rather than calculating all the primes for each denominator, we can use a
* method similar to the Sieve of Eratosthenes:
* - initialise an array so that array[i] = i;
* - iterate over the array from 2 onwards; when array[i] == i, that's a prime
*   number, and we multiply every multiple of i by (1 - 1/i).
* Now we know phi(denominator) for every denominator, so we sum them to get the
* answer to the problem.
 */

// MakePhiLookupTable generates a lookup table for Euler's Totient function, phi.  See description above.
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
	phiTable := MakePhiLookupTable(size)
	var total int64
	for _, value := range phiTable {
		total += value
	}
	// 1/1 is not a reduced proper fraction, because the numerator must be
	// less than the denominator.
	total -= phiTable[1]
	return total
}

func projectEuler72test() int64 {
	return projectEuler72actual(8)
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
	// Use Farey Sequences (http://en.wikipedia.org/wiki/FareySequence)
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

func calculateFactorialSum(number int) int {
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

func calculateFactorialChainLength(number int) int {
	sum := calculateFactorialSum(number)
	// The problem tells us that the longest non-repeating chain contains 60
	// elements.
	chain := make([]int, 62)
	chain[0] = number
	chain[1] = sum
	chainIndex := 2

	for {
		sum = calculateFactorialSum(sum)
		// Check if we have found a loop.
		for i := 0; i < chainIndex; i++ {
			if chain[i] == sum {
				return chainIndex
			}
		}
		// Still no loop, extend the chain.
		chain[chainIndex] = sum
		chainIndex++
	}
}

func projectEuler74actual(limit int) int64 {
	count := 0
	for i := 1; i < 100000; i++ {
		if calculateFactorialChainLength(i) == 60 {
			count++
		}
	}
	return int64(count)
}

func projectEuler74test() int64 {
	return projectEuler74actual(10000)
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

// PythagoreanTriple stores the components of a Pythagorean triple.
type PythagoreanTriple struct {
	a, b, c int
}

// MakeChildren generates three child PythagoreanTriple from a parent PythagoreanTriple.
// http://en.wikipedia.org/wiki/PythagoreanTriple#Parent.2FchildRelationships
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

func projectEuler75test() int64 {
	return projectEuler75Actual(100)
}

func projectEuler75Actual(upperBound int) int64 {
	counts := make([]int, upperBound+1)
	triples := make([]PythagoreanTriple, 1)
	triples[0] = PythagoreanTriple{a: 3, b: 4, c: 5}

	for index := 0; index < len(triples); index++ {
		triple := triples[index]
		children := triple.MakeChildren()
		for _, child := range children {
			sum := child.a + child.b + child.c
			if sum <= upperBound {
				triples = append(triples, child)
			}
		}

		sum := triple.a + triple.b + triple.c
		multiple := sum
		for multiple <= upperBound {
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

// IPArgs is the struct stored in IPResults.
type IPArgs struct {
	number, maxComponent int
}

// IPResults is a cache used by NumIntegerPartitions.
var IPResults = make(map[IPArgs]int)

// NumIntegerPartitions calculates the number of integer partitions for a number.  It uses IPResults as a cache to speed up processing.
func NumIntegerPartitions(number, maxComponent int) int {
	result, exists := IPResults[IPArgs{number, maxComponent}]
	if exists {
		return result
	}
	if number <= 1 {
		return 1
	}
	sum := 0
	if maxComponent <= number {
		sum += NumIntegerPartitions(number-maxComponent, maxComponent)
	}
	if maxComponent > 1 {
		if maxComponent <= number {
			sum += NumIntegerPartitions(number, maxComponent-1)
		} else {
			sum += NumIntegerPartitions(number, number)
		}
	}
	IPResults[IPArgs{number, maxComponent}] = sum
	return sum
}

func projectEuler76test() int64 {
	return int64(NumIntegerPartitions(20, 19))
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

// SumOfPrimeFactors The [formula for calculating the number of prime
// partitions](http://oeis.org/A000607) is voodoo, and it's not good for
// implementing an algorithm.  The formula can be transformed using [Euler's
// Transform](http://mathworld.wolfram.com/EulerTransform.html) into [something
// that can be implemented reasonably
// easily](http://math.stackexchange.com/a/89661).  I admit that I don't
// understand *why* or *how* either of these formulae work.
func SumOfPrimeFactors(number int, sieve []bool) int {
	factors := PrimeFactors(number, sieve)
	sum := 0
	lastFactor := 0
	for _, factor := range factors {
		if factor != lastFactor {
			sum += factor
			lastFactor = factor
		}
	}
	return sum
}

// SumOfPrimeFactorsCached is the cached version of SumOfPrimeFactors.
func SumOfPrimeFactorsCached(number int, sieve []bool, sopfCache map[int]int) int {
	result, exists := sopfCache[number]
	if !exists {
		result = SumOfPrimeFactors(number, sieve)
		sopfCache[number] = result
	}
	return result
}

// NumPrimePartitions calculates the number of prime partitions for a number.
// sieve is a prime sieve, nppCache is a cache that is updated in place to speed up processing.
func NumPrimePartitions(number int, sieve []bool, nppCache map[int]int,
	sopfCache map[int]int) int {
	if number == 1 {
		return 0
	}
	result, exists := nppCache[number]
	if exists {
		return result
	}

	sum := SumOfPrimeFactorsCached(number, sieve, sopfCache)
	for j := 1; j < number; j++ {
		sopfJ := SumOfPrimeFactorsCached(j, sieve, sopfCache)
		nppNumberMinusJ := NumPrimePartitions(number-j, sieve,
			nppCache, sopfCache)
		sum += nppNumberMinusJ * sopfJ
	}
	result = sum / number
	// Without caching, the time taken to calculate NumPrimePartitions(n) is
	// O(2^n).  n=29 is when it takes longer than 1 minute on my laptop.
	nppCache[number] = result
	return result
}

func projectEuler77actual(target int) int64 {
	sieve := SieveOfEratosthenes(100)
	nppCache := make(map[int]int)
	sopfCache := make(map[int]int)

	number, result := 0, 0
	for number = 1; result < target; number++ {
		result = NumPrimePartitions(number, sieve, nppCache,
			sopfCache)
	}
	return int64(number - 1)
}

func projectEuler77test() int64 {
	return projectEuler77actual(26)
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

func pentagonalNumber(number int) int {
	return ((3 * number * number) - number) / 2
}

func generalisedPentagonalNumber(number int) int {
	// input:   0, 1,  2, 3,  4, 5,  6, 7,  8
	// becomes: 0, 1, -1, 2, -2, 3, -3, 4, -4
	if number%2 == 0 {
		return pentagonalNumber(number / -2)
	}
	return pentagonalNumber((number + 1) / 2)
}

// IPresults2 is a cache used in NumIntegerPartitions2.
var IPresults2 = map[int]*big.Int{0: big.NewInt(1)}

// NumIntegerPartitions2 implements the formula described in
// http://en.wikipedia.org/wiki/Partition_(numberTheory)#GeneratingFunction
// but it needs a bit of interpretation to get something that you can implement.
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
		pentagonalNumber := generalisedPentagonalNumber(i)
		if pentagonalNumber > number {
			break
		}
		numIP := NumIntegerPartitions2(number - pentagonalNumber)
		temp := big.NewInt(0)
		temp.Mul(signs[i%len(signs)], numIP)
		sum.Add(sum, temp)
	}
	IPresults2[number] = sum
	return sum
}

func projectEuler78actual(multiple int64) int64 {
	bigMultiple := big.NewInt(multiple)
	modulus := big.NewInt(0)
	i := 0
	for {
		i++
		nip := NumIntegerPartitions2(i)
		modulus.Mod(nip, bigMultiple)
		if modulus.Int64() == 0 {
			return int64(i)
		}
	}
}

func projectEuler78test() int64 {
	return projectEuler78actual(7)
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

// SqrtPE80 implements http://en.wikipedia.org/wiki/MethodsOfComputingSquareRoots#Decimal_.28base_10.29
// The numbered comments in the function refer to the steps in the Wikipedia
// article.
func SqrtPE80(number, precision int) []int {
	digitStack := []int{}
	for remainder := number; remainder > 0; remainder /= 10 {
		digitStack = append(digitStack, remainder%10)
	}
	// Pad the digit stack so that an odd number of digits is interpreted
	// as 0A, BC rather than AB, 0C.
	if len(digitStack)%2 == 1 {
		digitStack = append(digitStack, 0)
	}
	digitStackI := len(digitStack) - 1

	remainder, rootSoFar := big.NewInt(0), big.NewInt(0)
	zero, one := big.NewInt(0), big.NewInt(1)
	ten, twenty := big.NewInt(10), big.NewInt(20)
	var result []int
	for len(result) < precision {
		// Step 1.
		current := big.NewInt(0)
		current.Set(remainder)
		for j := 0; j < 2; j++ {
			current = current.Mul(current, ten)
			if digitStackI >= 0 {
				current.Add(current,
					big.NewInt(int64(digitStack[digitStackI])))
			}
			digitStackI--
		}

		// Step 2.
		nextDigit, subtractMe := big.NewInt(0), big.NewInt(0)
		for {
			nextDigit.Add(nextDigit, one)
			temp := big.NewInt(0)
			temp.Set(rootSoFar)
			temp.Mul(temp, twenty)
			temp.Add(temp, nextDigit)
			temp.Mul(temp, nextDigit)
			if temp.Cmp(current) == 1 {
				nextDigit.Sub(nextDigit, one)
				break
			}
			subtractMe.Set(temp)
		}

		// Step 3.
		remainder.Sub(current, subtractMe)
		rootSoFar.Mul(rootSoFar, ten)
		rootSoFar.Add(rootSoFar, nextDigit)
		result = append(result, int(nextDigit.Int64()))

		// Step 4.
		if remainder.Cmp(zero) == 0 && digitStackI < 0 {
			break
		}
	}
	return result
}

func projectEuler80actual(upperBound int) int64 {
	result := 0
	for i := 0; i <= upperBound; i++ {
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
	if err != nil {
		return -1
	}
	size := len(matrix)
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

// TwoDPoint represents a point in a two dimensional array.  i,j are the indices of the ppoint.  cost is the cost to get to this point, and heuristic is an estimate of the extra cost to get to the end point.
type TwoDPoint struct {
	i, j            int
	cost, heuristic uint64
}

// TwoDPointHeap implements the Heap interface for []TwoDPoint.
// https://golang.org/pkg/container/heap/
type TwoDPointHeap []TwoDPoint

// Implement the Heap interface for []TwoDPoint, starting with the Sort interface.
func (h TwoDPointHeap) Len() int           { return len(h) }
func (h TwoDPointHeap) Less(i, j int) bool { return h[i].cost+h[i].heuristic < h[j].cost+h[j].heuristic }
func (h TwoDPointHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }

// Push appends an element to the array.
func (h *TwoDPointHeap) Push(x interface{}) {
	*h = append(*h, x.(TwoDPoint))
}

// Pop removes the last element from the array.
func (h *TwoDPointHeap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

// TwoDAStar82 implements A Star Search for a two dimensional array.
type TwoDAStar82 struct {
	// The cost of a node in the path.
	costs [][]uint64
	// Heuristics for the remaining cost to reach an end node.
	heuristics []uint64
	// The best cost we've seen getting to a particular node.  If the cost of the current path is higher we abandon the current path.
	bestCosts [][]uint64
	// A heap of nodes to check, sorted by smallest (cost + heuristic).
	nodes TwoDPointHeap
}

// AStarNode is a node in an A Star searchable graph.
type AStarNode interface{}

// AStarSearchable is the interface to implement to make a data structure A Star searchable.
type AStarSearchable interface {
	// Init initialises internal state.
	Init()
	// AddStartNodes adds the starting nodes to the heap.
	AddStartNodes()
	// IsEndNode determines if a node is an end node.
	IsEndNode(node AStarNode) bool
	// PopNode pops the next node off the heap and returns it.
	PopNode() AStarNode
	// AddChildNodes adds the child nodes of node to the heap.
	AddChildNodes(node AStarNode)
}

// Init initialises internal state.
func (a *TwoDAStar82) Init() {
	// Start by populating heuristics.  Figure out the minimum cost, then set each heuristic to (minimum cost * minimum num squares to end).
	a.heuristics = make([]uint64, len(a.costs))
	minCost := a.costs[0][0]
	for i := range a.costs {
		for j := range a.costs[0] {
			if a.costs[i][j] < minCost {
				minCost = a.costs[i][j]
			}
		}
	}
	for i := range a.costs {
		numSquares := uint64(len(a.costs) - i - 1)
		a.heuristics[i] = minCost * numSquares
	}

	a.bestCosts = make([][]uint64, len(a.costs))
	for i := range a.costs {
		a.bestCosts[i] = make([]uint64, len(a.costs[0]))
	}
	heap.Init(&a.nodes)
}

// AddStartNodes adds the starting nodes to the heap.
func (a *TwoDAStar82) AddStartNodes() {
	for i := range a.costs {
		n := TwoDPoint{
			i:         i,
			j:         0,
			cost:      a.costs[i][0],
			heuristic: a.heuristics[i],
		}
		heap.Push(&a.nodes, n)
	}
}

// IsEndNode determines if a node is an end node.
func (a TwoDAStar82) IsEndNode(node AStarNode) bool {
	n := node.(TwoDPoint)
	return n.j == len(a.costs[0])-1
}

// PopNode pops the next node off the heap and returns it.
func (a *TwoDAStar82) PopNode() AStarNode {
	return heap.Pop(&a.nodes)
}

// ExtendPath adds a new step to n and adds it to the heap.
func (a *TwoDAStar82) ExtendPath(n TwoDPoint, i, j int) {
	node := TwoDPoint{
		i:         i,
		j:         j,
		cost:      n.cost + a.costs[i][j],
		heuristic: a.heuristics[i],
	}
	heap.Push(&a.nodes, node)
}

// AddChildNodes adds the child nodes of node to the heap.
func (a *TwoDAStar82) AddChildNodes(node AStarNode) {
	n := node.(TwoDPoint)
	if a.bestCosts[n.i][n.j] != 0 && a.bestCosts[n.i][n.j] <= n.cost {
		// We've already seen a better path to this node, discard the current path.
		return
	}
	a.bestCosts[n.i][n.j] = n.cost
	if n.i != 0 {
		// Add node above.
		a.ExtendPath(n, n.i-1, n.j)
	}
	if n.i != len(a.costs)-1 {
		// Add node below.
		a.ExtendPath(n, n.i+1, n.j)
	}
	if n.j != len(a.costs[0])-1 {
		// Add node to the right.
		a.ExtendPath(n, n.i, n.j+1)
	}
}

// AStarSearch performs an A* Search on a graph.
func AStarSearch(a AStarSearchable) AStarNode {
	a.Init()
	a.AddStartNodes()
	n := a.PopNode()
	for !a.IsEndNode(n) {
		a.AddChildNodes(n)
		n = a.PopNode()
	}
	return n
}

func projectEuler82actual(matrix *TwoDAStar82) int64 {
	node := AStarSearch(matrix).(TwoDPoint)
	return int64(node.cost)
}

func projectEuler82test() int64 {
	data := `131,637,234,103,18
201,96,342,965,150
630,803,746,422,111
537,699,497,121,956
805,732,524,37,331`
	costs, err := readIntsFromCSVFile(strings.NewReader(data))
	if err != nil {
		return -1
	}
	return projectEuler82actual(&TwoDAStar82{costs: costs})
}

/*
* In the 5 by 5 matrix below, the minimal path sum from the top left to the
* bottom right, by moving left, right, up, and down, is indicated in bold red
* and is equal to 2297.
*
*   131 673 234 103  18
*   201  96 342 965 150
*   630 803 746 422 111
*   537 699 497 121 956
*   805 732 524  37 331
*
* Find the minimal path sum, in matrix.txt (right click and "Save Link/Target
* As..."), a 31K text file containing a 80 by 80 matrix, from the top left to
* the bottom right by moving left, right, up, and down.
 */

// TwoDAStar83 implements A Star Search for a two dimensional array.
type TwoDAStar83 struct {
	// The cost of a node in the path.
	costs [][]uint64
	// Heuristics for the remaining cost to reach an end node.
	heuristics [][]uint64
	// The best cost we've seen getting to a particular node.  If the cost of the current path is higher we abandon the current path.
	bestCosts [][]uint64
	// A heap of nodes to check, sorted by smallest (cost + heuristic).
	nodes TwoDPointHeap
}

// Init initialises internal state.
func (a *TwoDAStar83) Init() {
	// Start by populating heuristics.  Figure out the minimum cost, then set each heuristic to (minimum cost * minimum num squares to end).
	a.heuristics = make([][]uint64, len(a.costs))
	for i := range a.costs[0] {
		a.heuristics[i] = make([]uint64, len(a.costs[0]))
	}
	minCost := a.costs[0][0]
	for i := range a.costs {
		for j := range a.costs[0] {
			if a.costs[i][j] < minCost {
				minCost = a.costs[i][j]
			}
		}
	}
	for i := range a.costs {
		for j := range a.costs[0] {
			numSquares := len(a.costs) - i + len(a.costs[0]) - j - 2
			a.heuristics[i][j] = minCost * uint64(numSquares)
		}
	}

	a.bestCosts = make([][]uint64, len(a.costs))
	for i := range a.costs {
		a.bestCosts[i] = make([]uint64, len(a.costs[0]))
	}
	heap.Init(&a.nodes)
}

// AddStartNodes adds the starting nodes to the heap.
func (a *TwoDAStar83) AddStartNodes() {
	n := TwoDPoint{
		i:         0,
		j:         0,
		cost:      a.costs[0][0],
		heuristic: a.heuristics[0][0],
	}
	heap.Push(&a.nodes, n)
}

// IsEndNode determines if a node is an end node.
func (a TwoDAStar83) IsEndNode(node AStarNode) bool {
	n := node.(TwoDPoint)
	return n.i == len(a.costs)-1 && n.j == len(a.costs[0])-1
}

// PopNode pops the next node off the heap and returns it.
func (a *TwoDAStar83) PopNode() AStarNode {
	return heap.Pop(&a.nodes)
}

// ExtendPath adds a new step to n and adds it to the heap.
func (a *TwoDAStar83) ExtendPath(n TwoDPoint, i, j int) {
	node := TwoDPoint{
		i:         i,
		j:         j,
		cost:      n.cost + a.costs[i][j],
		heuristic: a.heuristics[i][j],
	}
	heap.Push(&a.nodes, node)
}

// AddChildNodes adds the child nodes of node to the heap.
func (a *TwoDAStar83) AddChildNodes(node AStarNode) {
	n := node.(TwoDPoint)
	if a.bestCosts[n.i][n.j] != 0 && a.bestCosts[n.i][n.j] <= n.cost {
		// We've already seen a better path to this node, discard the current path.
		return
	}
	a.bestCosts[n.i][n.j] = n.cost
	if n.i != 0 {
		// Add node above.
		a.ExtendPath(n, n.i-1, n.j)
	}
	if n.i != len(a.costs)-1 {
		// Add node below.
		a.ExtendPath(n, n.i+1, n.j)
	}
	if n.j != len(a.costs[0])-1 {
		// Add node to the right.
		a.ExtendPath(n, n.i, n.j+1)
	}
	if n.j != 0 {
		// Add node to the left.
		a.ExtendPath(n, n.i, n.j-1)
	}
}

func projectEuler83actual(matrix *TwoDAStar83) int64 {
	node := AStarSearch(matrix).(TwoDPoint)
	return int64(node.cost)
}

func projectEuler83test() int64 {
	data := `131,673,234,103,18
201,96,342,965,150
630,803,746,422,111
537,699,497,121,956
805,732,524,37,331`
	costs, err := readIntsFromCSVFile(strings.NewReader(data))
	if err != nil {
		return -1
	}
	return projectEuler83actual(&TwoDAStar83{costs: costs})
}

/*
* See https://projecteuler.net/problem=84, it's too hard to put in here.
 */

/*
* Thoughts:
* - A Markov chain/Markov matrix seems to be the way to go here.
* - There will be 40 states in the chain.
* - The initial distribution will be {1, 0, 0, ..., 0} because we always
*   start at Go.  Though I don't think the initial distribution matters much
*   because we won't be doing anything with it.
* - There'll be a 40x40 state transition matrix.
* - I don't understand yet how to populate the matrix.
*   - Initialise row 1 of the matrix to {0, 0, 1/36, 2/36, ..., 1/36, 0, ...},
*     i.e. the probability of rolling dice and reaching square N.
*   - Row 2 of the matrix is the same as row 1 but shifted right 1, and so on.
*   - There's a 1/(6^3) chance of rolling 3 doubles, and it's independent of
*     the squares.  Maybe just subtract 1/(6^3) from each of the squares and
*     add it to Jail?
*   - Once that's done, the special squares have to be examined.  Subtract the
*     chance of moving to another square and add it to the destination square.
 */

// MarkovMatrixInvarientCheck checks that
// 1) the sum of probabilities in each row is 1
// 2) all values are positive
// and panics if any condition is not met
// Use index=-1 to check all rows, or index=X to check row X only.
func MarkovMatrixInvarientCheck(matrix [][]*big.Rat, index int) {
	expected := big.NewRat(1, 1)
	zero := big.NewRat(0, 1)
	for i := range matrix {
		if index == -1 || index == i {
			sum := big.NewRat(0, 1)
			for j := range matrix[i] {
				sum.Add(sum, matrix[i][j])
				if zero.Cmp(matrix[i][j]) == 1 {
					panic(fmt.Sprintf("matrix[%v][%v] is negative: %v\n", i, j, matrix[i][j]))
				}
			}
			if sum.Cmp(expected) != 0 {
				panic(fmt.Sprintf("sum of row %v: want %v, got %v: %v\n", i, expected, sum, matrix[i]))
			}
		}
	}
}

// RatMul multiplies two Rats, returning a new Rat.
func RatMul(a, b *big.Rat) *big.Rat {
	r := big.NewRat(0, 1)
	return r.Mul(a, b)
}

// MarkovShiftProbability subtracts probability from one element and adds it to another.
func MarkovShiftProbability(matrix [][]*big.Rat, probability *big.Rat, i, j, k int) {
	matrix[i][k].Add(matrix[i][k], probability)
	matrix[i][j].Sub(matrix[i][j], probability)
}

func firstBiggerElement(c int, l []int) int {
	for _, e := range l {
		if e > c {
			return e
		}
	}
	panic(fmt.Sprintf("firstBiggerElement: %v bigger than %v\n", c, l))
}

type ratWithIndex struct {
	i int
	r *big.Rat
}
type ratWithIndexSlice []ratWithIndex

func (l ratWithIndexSlice) Len() int           { return len(l) }
func (l ratWithIndexSlice) Less(i, j int) bool { return l[i].r.Cmp(l[j].r) == -1 }
func (l ratWithIndexSlice) Swap(i, j int)      { l[i], l[j] = l[j], l[i] }

type floatWithIndex struct {
	i int
	f float64
}
type floatWithIndexSlice []floatWithIndex

func (l floatWithIndexSlice) Len() int           { return len(l) }
func (l floatWithIndexSlice) Less(i, j int) bool { return l[i].f < l[j].f }
func (l floatWithIndexSlice) Swap(i, j int)      { l[i], l[j] = l[j], l[i] }

// fillDiceRolls recursively fills in rolls rollsLeft times, using two dice, starting from position.
// If rollsLeft is 1 and doubles are rolled, we don't recurse and we increment index 0 rather than the index of the roll; this means that to go to jail after 3 doubles, rollsLeft should start at 3.
func fillDiceRolls(rolls []int64, faces, position, rollsLeft int64) {
	for i := int64(1); i <= faces; i++ {
		for j := int64(1); j <= faces; j++ {
			if rollsLeft == 1 && i == j {
				rolls[0]++
			} else {
				newPosition := position + i + j
				rolls[newPosition]++
				if rollsLeft > 1 && i == j {
					fillDiceRolls(rolls, faces, newPosition, rollsLeft-1)
				}
			}
		}
	}
}

func projectEuler84actual(diceSize int64) int64 {
	const numSquares = 40
	const jailSquare = 10
	// Start by working out the number of dice combinations for each offset, and how often you roll doubles three times and go to jail.
	rolls := make([]int64, (diceSize * 3 * 2))
	fillDiceRolls(rolls, diceSize, 0, 3)
	denominator := int64(0)
	for i := range rolls {
		denominator += rolls[i]
	}
	rollThreeDoublesChance := big.NewRat(rolls[0], denominator)
	rolls[0] = 0

	// Now create the board with empty values.
	board := make([][]*big.Rat, numSquares)
	for i := range board {
		board[i] = make([]*big.Rat, numSquares)
		for j := range board[i] {
			board[i][j] = big.NewRat(0, 1)
		}
	}

	// Add the data from rolls, offsetting by 1 on each line.
	for i := range board {
		for j := range rolls {
			// Wrap around when we get towards the end of the row.
			x := (i + j) % numSquares
			board[i][x].SetFrac64(rolls[j], denominator)
		}
		// Add the probability of rolling three doubles *after* initialising from rolls, otherwise initialising jailSquare will clobber the additions we've already made to jailSquare.

		board[i][jailSquare].Add(board[i][jailSquare], rollThreeDoublesChance)
		MarkovMatrixInvarientCheck(board, i)
	}

	MarkovMatrixInvarientCheck(board, -1)

	// Now deal with each of the special squares.
	oneSixteenth := big.NewRat(1, 16)
	const goSquare = 0

	// Community Chest.
	for _, square := range []int{2, 17, 33} {
		for i := range board {
			move := RatMul(oneSixteenth, board[i][square])
			// Advance to Go.
			MarkovShiftProbability(board, move, i, square, goSquare)
			// Go to Jail.
			MarkovShiftProbability(board, move, i, square, jailSquare)
		}
	}
	MarkovMatrixInvarientCheck(board, -1)

	// Chance
	for _, square := range []int{7, 22, 36} {
		for i := range board {
			move := RatMul(oneSixteenth, board[i][square])
			// Advance to Go.
			MarkovShiftProbability(board, move, i, square, goSquare)
			// Go to Jail.
			MarkovShiftProbability(board, move, i, square, jailSquare)
			// Go to C1.
			MarkovShiftProbability(board, move, i, square, 11)
			// Go to E3.
			MarkovShiftProbability(board, move, i, square, 24)
			// Go to H2.
			MarkovShiftProbability(board, move, i, square, 38)
			// Go to R1.
			MarkovShiftProbability(board, move, i, square, 5)
			// Go to next R, twice.
			nextR := firstBiggerElement(square, []int{5, 15, 25, 35, 5 + numSquares})
			nextR = nextR % numSquares
			MarkovShiftProbability(board, move, i, square, nextR)
			MarkovShiftProbability(board, move, i, square, nextR)
			// Go to next U.
			nextU := firstBiggerElement(square, []int{12, 28, 12 + numSquares})
			nextU = nextU % numSquares
			MarkovShiftProbability(board, move, i, square, nextU)
			// Go back 3 squares.
			MarkovShiftProbability(board, move, i, square, square-3)
		}
	}
	MarkovMatrixInvarientCheck(board, -1)

	// Go to Jail.
	const goToJailSquare = 30
	for i := range board {
		MarkovShiftProbability(board, board[i][goToJailSquare], i, goToJailSquare, jailSquare)
	}
	MarkovMatrixInvarientCheck(board, -1)

	// Convert the board to a DenseMatrix of probabilities.
	floats := make([]float64, numSquares*numSquares)
	for i := range board {
		for j := range board[0] {
			// There's nothing we can do if f is not exact, so ignore the second return value.
			f, _ := board[i][j].Float64()
			floats[(i*numSquares)+j] = f
		}
	}
	probabilities := matrix.MakeDenseMatrix(floats, numSquares, numSquares)

	// Now apply probability matrix to the starting position 100 times; after about 50 iterations the result of the multiplication doesn't change.
	state := matrix.MakeDenseMatrix(make([]float64, numSquares), 1, numSquares)
	state.Set(0, 0, 1)
	for i := 0; i < 100; i++ {
		state = matrix.Product(state, probabilities)
		// fmt.Printf("%v\n", state)
	}

	sortMe := make([]floatWithIndex, numSquares)
	for i := range sortMe {
		sortMe[i] = floatWithIndex{i: i, f: state.Get(0, i)}
	}
	sort.Sort(floatWithIndexSlice(sortMe))
	for i := range sortMe {
		fmt.Printf("%v %v\n", sortMe[i].i, sortMe[i].f)
	}

	x := len(sortMe) - 1
	return int64((sortMe[x].i * 10000) + (sortMe[x-1].i * 100) + sortMe[x-2].i)
}

func projectEuler84test() int64 {
	return projectEuler84actual(6)
}

/*
* The smallest number expressible as the sum of a prime square, prime cube, and
* prime fourth power is 28. In fact, there are exactly four numbers below fifty
* that can be expressed in such a way:
*
*     28 = 2^2 + 2^3 + 2^4
*     33 = 3^2 + 2^3 + 2^4
*     49 = 5^2 + 2^3 + 2^4
*     47 = 2^2 + 3^3 + 2^4
*
* How many numbers below fifty million can be expressed as the sum of a prime
* square, prime cube, and prime fourth power?
 */

/*
* First idea:
* - Generate primes up to sqrt(limit).
* - Triply-nested loop calculating sums.
* - Store results in a map[int]bool, so we don't double count.
 */

func projectEuler87actual(limit int) int64 {
	// Generate primes up to sqrt(limit) and put them in a slice.
	// Counting and explicitly allocating should be more efficient that repeatedly reallocating the slice.
	size := int(math.Sqrt(float64(limit))) + 1
	sieve := SieveOfEratosthenes(size)
	numPrimes := 0
	for _, prime := range sieve {
		if prime {
			numPrimes++
		}
	}
	primes := make([]int, numPrimes)
	j := 0
	for i, prime := range sieve {
		if prime {
			primes[j] = i
			j++
		}
	}

	// Calculate the sums.
	sums := make(map[int]bool)
	for _, square := range primes {
		for _, cube := range primes {
			partialSum := (square * square) + (cube * cube * cube)
			if partialSum > limit {
				break
			}
			for _, fourth := range primes {
				sum := partialSum + (fourth * fourth * fourth * fourth)
				if sum > limit {
					break
				}
				sums[sum] = true
			}
		}
	}

	// How many did we find?
	return int64(len(sums))
}

func projectEuler87test() int64 {
	return projectEuler87actual(50)
}

/*
* A natural number, N, that can be written as the sum and product of a given set
* of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum
* number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.
*
* For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.
*
* For a given set of size, k, we shall call the smallest N with this property a
* minimal product-sum number. The minimal product-sum numbers for sets of size,
* k = 2, 3, 4, 5, and 6 are as follows.
*
*     k=2: 4 = 2 × 2 = 2 + 2
*     k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
*     k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
*     k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
*     k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6
*
* Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 =
* 30; note that 8 is only counted once in the sum.
*
* In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4,
* 6, 8, 12, 15, 16}, the sum is 61.
*
* What is the sum of all the minimal product-sum numbers for 2≤k≤12000
 */

/*
* Thoughts:
* - For each k, k is the lower bound for N, because N = k*1 is the lower bound.
* - Find the factors of N by trial division and cache them.  No point in pre-populating this.
* - For each k, generate size k subsets of the factors of N.  Increment N until we find an answer for k.
* - Map tracking N => [k]?  Or just a bool?
 */

// Uint64Slice attaches the methods of Interface to []int64, sorting in increasing order.
// Go doesn't provide sorting methods for int64.
type Uint64Slice []uint64

func (p Uint64Slice) Len() int           { return len(p) }
func (p Uint64Slice) Less(i, j int) bool { return p[i] < p[j] }
func (p Uint64Slice) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

// Factors returns the factors of n, in ascending order.  If inc is true include 1 and n.
func Factors(n uint64, inc bool) []uint64 {
	f := []uint64{}
	if inc {
		f = append(f, 1)
		f = append(f, n)
	}
	l := uint64(math.Ceil(math.Sqrt(float64(n))))
	var i uint64
	for i = 2; i <= l; i++ {
		// Check that i <= n/i.  If n=42, i=6 will add 6 and 7, but i=7 shouldn't add anything.
		if n%i == 0 && i <= n/i {
			f = append(f, i)
			if i != n/i {
				f = append(f, n/i)
			}
		}
	}
	sort.Sort(Uint64Slice(f))
	return f
}

// PermuteUpToKOfN permutes p, ensuring up to k elements are selected.  Returns true until it runs out of numbers; when false is returned don't use the values in p.
// Note that you'll need to use the initial permutation yourself, it won't be returned.
func PermuteUpToKOfN(p []bool, k int) bool {
	for {
		// This is basically p++.
		var i int
		for i = len(p) - 1; i >= 0 && p[i]; i-- {
			p[i] = false
		}
		// Stop if everything is true.
		if i < 0 {
			return false
		}
		p[i] = true
		// Count the true bits.
		c := 0
		for _, v := range p {
			if v {
				c++
			}
		}
		// If the right number of bits are set return, otherwise loop.
		if c <= k {
			return true
		}
	}
}

// FindProductSum looks for a product-sum number in factors, returning true if one is found.
// This is a dummy implementation for testing.
func FindProductSum(k, n uint64, factors []uint64) bool {
	if k == 2 && n == 4 {
		return true
	}
	if k == 3 && n == 6 {
		return true
	}
	if k == 4 && n == 8 {
		return true
	}
	if k == 5 && n == 8 {
		return true
	}
	if k == 6 && n == 12 {
		return true
	}
	// I don't know if the pairings are right from here on, the question doesn't supply them.
	if k == 7 && n == 12 {
		return true
	}
	if k == 8 && n == 15 {
		return true
	}
	if k == 9 && n == 15 {
		return true
	}
	if k == 10 && n == 15 {
		return true
	}
	if k == 11 && n == 16 {
		return true
	}
	if k == 12 && n == 16 {
		return true
	}
	return false
}

func projectEuler88actual(upperBound uint64) int64 {
	factorsCache := make(map[uint64][]uint64)
	numbers := make(map[uint64]uint64)
	for k := uint64(2); k <= upperBound; k++ {
		for n := k; true; n++ {
			factors, e := factorsCache[n]
			if !e {
				factors = Factors(n, false)
				factorsCache[n] = factors
			}
			if FindProductSum(k, n, factors) {
				numbers[k] = n
				break
			}
		}
	}

	// Reverse the numbers so that each n is only present once, then sum them.
	results := make(map[uint64]bool)
	for _, n := range numbers {
		results[n] = true
	}
	sum := uint64(0)
	for n := range results {
		sum += n
	}
	return int64(sum)
}

func projectEuler88test() int64 {
	return projectEuler88actual(12)
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
	lines := readLinesFromFile(fh)
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
	nFac := big.NewInt(0).MulRange(1, int64(n))
	// This should be kFac, but starting a variable with k[UPPER] is a hard-coded lint warning in golang.  Sigh.
	xkFac := big.NewInt(0).MulRange(1, int64(k))
	nKFac := big.NewInt(0).MulRange(1, int64(n-k))
	combs := nFac.Div(nFac, xkFac.Mul(xkFac, nKFac))
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
	var bestM, bestN, bestCombs uint64
	// TODO(johntobin): hardcoding the upper bound is bad, how do I avoid that?
	// TODO(johntobin): it would be nicer if the test had a different target.
	for m := uint64(5); m < 80; m++ {
		for n := uint64(5); n <= m; n++ {
			combs := numCombinations(m+1, 2) * numCombinations(n+1, 2)
			if difference(target, combs) < difference(target, bestCombs) {
				bestCombs = combs
				bestM = m
				bestN = n
			}
		}
	}
	return int64(bestM * bestN)
}

func projectEuler85test() int64 {
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
const squareChainCacheSize uint = 568

/*
* Return the last element in the square chain for number, either 1 or 89.
* cache is used to short circuit following the chain and is updated.
 */
func squareChain(number uint, cache []uint) uint {
	if number < squareChainCacheSize && cache[number] != 0 {
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
	if number < squareChainCacheSize {
		cache[number] = result
	}
	return result
}

func projectEuler92actual(limit uint) int64 {
	cache := make([]uint, squareChainCacheSize)
	cache[1] = 1
	cache[89] = 89
	count1, count89 := 0, 0
	for i := uint(1); i < limit; i++ {
		result := squareChain(i, cache)
		if result == 1 {
			count1++
		} else {
			count89++
		}
	}
	return int64(count89)
}

func projectEuler92test() int64 {
	return projectEuler92actual(10)
}

/*
* The first known prime found to exceed one million digits was discovered in
* 1999, and is a Mersenne prime of the form 26972593−1; it contains exactly
* 2,098,960 digits. Subsequently other Mersenne primes, of the form 2p−1, have
* been found which contain more digits.
*
* However, in 2004 there was found a massive non-Mersenne prime which contains
* 2,357,207 digits: 28433×2^7830457+1.
*
* Find the last ten digits of this prime number.
 */

func projectEuler97actual(exp, mul int64) int64 {
	prime := big.NewInt(1).Exp(big.NewInt(2), big.NewInt(exp), nil)
	prime = prime.Mul(prime, big.NewInt(mul)).Add(prime, big.NewInt(1))
	str := fmt.Sprintf("%v", prime)
	l := len(str)
	res, _ := strconv.ParseUint(str[l-10:l], 10, 64)
	return int64(res)
}

func projectEuler97test() int64 {
	return projectEuler97actual(50, 7)
}

/*
* Comparing two numbers written in index form like 2^11 and 3^7 is not
* difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
*
* However, confirming that 632382^518061 > 519432^525806 would be much more
* difficult, as both numbers contain over three million digits.
*
* Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file
* containing one thousand lines with a base/exponent pair on each line,
* determine which line number has the greatest numerical value.
*
* NOTE: The first two lines in the file represent the numbers in the example
* given above.
 */

func projectEuler99actual(r io.Reader) int64 {
	pairs, err := readIntsFromCSVFile(r)
	if err != nil {
		return -1
	}
	biggest := big.NewInt(0)
	biggestL := 0
	val := big.NewInt(0)
	for l, pair := range pairs {
		val.SetInt64(int64(pair[0]))
		val.Exp(val, big.NewInt(int64(pair[1])), nil)
		if biggest.Cmp(val) == -1 {
			biggest.Set(val)
			biggestL = l
		}
	}
	return int64(biggestL + 1)
}

func projectEuler99test() int64 {
	data := `2,11
3,7`
	return projectEuler99actual(strings.NewReader(data))
}
