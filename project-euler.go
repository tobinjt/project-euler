package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"math"
	"math/big"
	"os"
	"sort"
	"strconv"
	"strings"
)

func breakpoint() string {
	return fmt.Sprint("breakpoint reached")
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

func projectEuler70() int64 {
	bound := 10 * 1000 * 1000
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

func CalculateFactorialSum(number int) int {
	factorials := []int{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}
	if number == 0 {
		return factorials[number]
	}
	sum := 0
	for number > 0 {
		digit := number%10
		sum += factorials[digit]
		number /= 10
	}
	return sum
}

// Modifies chain_lengths if a necessary number is missing.
func CalculateFactorialChainLength(chain_lengths map[int]int, number int) int {
	sum := CalculateFactorialSum(number)
	length, present := chain_lengths[sum]
	if present {
		return length + 1
	}

	// The problem tells us that the longest non-repeating chain contains 60
	// elements.
	chain := make([]int, 62)
	chain[0] = number
	chain[1] = sum
	chain_index := 2

	for ! present {
		sum = CalculateFactorialSum(sum)
		length, present = chain_lengths[sum]
		if present {
			break
		}
		// Check if we have found a loop.
		for i := 0; i < chain_index; i++ {
			if chain[i] != sum {
				continue
			}
			present = true
			length = chain_index
			// We can only cache the length of the first sum we
			// calculated.  Consider this chain:
			// 69 -> 363600 -> 1454 -> 169 -> 363601 -> 1454
			// We cannot record a chain length for 169 or 363601.
			// We *could* record a chain length for 1454, but that's
			// complex and doesn't save us much overall.
			chain_lengths[chain[1]] = chain_index - 1
		}
		if ! present {
			// Still no loop, extend the chain.
			chain[chain_index] = sum
			chain_index++
		}
	}
	return length
}

func projectEuler74() int64 {
	// The first 1,000,000 numbers produce 4015 unique sums, none of which
	// are greater than 1,000,000.  Recording the chain lengths means we'll
	// only have to follow 4015 chains fully, so we'll have a fast-path
	// lookup for >99.5% of numbers.
	// Maps [sum of factorials] to [chain length].
	// Note that you'll need to add 1 to the length, because the first
	// number is not included in the length.
	// chain_lengths := map[int]int
	for i := 1; i < 1000000; i++ {
		sum := CalculateFactorialSum(i)
		fmt.Printf("%d -> %d\n", i, sum)
	}
	return 0
}

func test() int64 {
	return int64(0)
}

// A dummy function to be called during testing.
func fortesting() int64 {
	return 0
}

func main() {
	flag.Parse()
	result, err := realMain(flag.Args())
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println(result)
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
		return 0, errors.New("Only 1 arg accepted from this list: " +
			strings.Join(keys, " "))
	}
	return functions[args[0]](), nil
}
