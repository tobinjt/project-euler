package main

import (
	"fmt"
	"math/big"
	"strings"
	"time"
)

/*
* The smallest number m such that 10 divides m! is m=5.
* The smallest number m such that 25 divides m! is m=10.
*
* Let s(n) be the smallest number m such that n divides m!.
* So s(10)=5 and s(25)=10.
* Let S(n) be ∑s(i) for 2 ≤ i ≤ n.
* S(100)=2012.
*
* Find S(10^8).
 */

/*
* I've had various thoughts about expressing i as a product of primes.
* - The highest prime must be present in the factorial, so that sets a lower bound.
* - I could use the primes factors as a bloom filter to whittle down the potential numbers to check against.
* - But every approach I think of feels expensive, so I'll start with trial division and see how far I get.  The distribution of factorials will help me figure out where to optimise.
 */

// factorialBigIntCached calculates n!, using the existing data in cache, and updates cache[n].
func factorialBigIntCached(n int64, cache []*big.Int) {
	if cache[n] != nil {
		return
	}
	if n == 0 {
		// 0 is the only number that the algorithm doesn't work for.
		cache[0] = big.NewInt(1)
		return
	}

	i := int(n - 1)
	r := big.NewInt(n)
	bn := big.NewInt(n)
	one := big.NewInt(1)
	for bn.Cmp(one) == 1 {
		if cache[i] != nil {
			r.Mul(r, cache[i])
			break
		}
		i--
		bn.Sub(bn, one)
		r.Mul(r, bn)
	}
	cache[n] = r
}

func projectEuler549actual(upper int64) int64 {
	sieve := SieveOfEratosthenes(int(upper))
	fCache := make([]*big.Int, upper)
	fCache[0] = big.NewInt(1)
	fCache[1] = big.NewInt(1)
	zero := big.NewInt(0)
	div := big.NewInt(0)
	mod := big.NewInt(0)
	var sum int64

Outer:
	for i := int64(2); i <= upper; i++ {
		// The lowest number we should even consider is the biggest prime factor of i.
		// This is inefficient for 97969 (313 * 313), we need to go to 626, i.e. 313 + 313.
		// At first I thought that I'd multiply the highest factor by the number of times it appears, but that doesn't work for 8.
		// 8 is 2 * 2 * 2.  4! is the answer we're looking for, but 2 + 2 + 2 = 6.
		// So if the second highest prime factor == the highest prime factor, start with 2 * highest prime factor.
		factors := PrimeFactors(int(i), sieve)
		k := len(factors) - 1
		j := factors[k]
		if k > 0 && factors[k] == factors[k-1] {
			j += factors[k]
		}

		div.SetInt64(i)
		for {
			if fCache[j] == nil {
				factorialBigIntCached(int64(j), fCache)
			}
			mod.Mod(fCache[j], div)
			if mod.Cmp(zero) == 0 {
				sum += int64(j)
				continue Outer
			}
			j++
		}
	}
	return sum
}

func projectEuler549test() int64 {
	return projectEuler549actual(100)
}

var logEveryNCounter int
var logEveryNPrintFunc = fmt.Printf
var logEveryNTimestamp = time.Now()

// logEveryN prints every nth call; it doesn't reset and it doesn't track call location or anything clever.
// The first
func logEveryN(n int, format string, a ...interface{}) {
	logEveryNCounter++
	if logEveryNCounter%n == 0 {
		t := time.Now()
		d := t.Sub(logEveryNTimestamp)
		logEveryNTimestamp = t
		str := fmt.Sprintf(format, a...)
		ps := strings.Split(fmt.Sprintf("%v", t), "+")
		logEveryNPrintFunc("%-30v%.5f   %v", ps[0], d.Seconds(), str)
	}
}

// PrimeFactors2 generates a list of prime factors for a number.  Factors are not deduplicated.
// slice is a list of prime numbers.
func PrimeFactors2(number int, sieve []int) []int {
	factors := []int{}
	remainder := number
	if number <= 1 {
		return factors
	}
	for _, divisor := range sieve {
		for remainder%divisor == 0 {
			factors = append(factors, divisor)
			remainder /= divisor
		}
		if remainder == 1 {
			break
		}
	}
	return factors
}

func sieveToPF2(sieve []bool) []int {
	c := 0
	for _, p := range sieve {
		if p {
			c++
		}
	}
	primes := make([]int, c)
	c = 0
	for d, p := range sieve {
		if p {
			primes[c] = d
			c++
		}
	}
	return primes
}

func projectEuler549_2actual(upper int) int64 {
	sieve := SieveOfEratosthenes(upper)
	primes := sieveToPF2(sieve)
	// factors tracks the lowest n where n! contains a given number of a certain prime factor.
	// E.g. factors[2][2] = 4, because 4! is the first factorial to contain 2 * 2.
	factors := make([][]int, upper)
	sum := int64(0)
	// cumF tracks the cumulative prime factors of n.
	cumF := make(map[int]int, len(primes))

Outer:
	for n := 2; n <= upper; n++ {
		logEveryN(10000, "%v / %v %.2f%%\n", n, upper, float64(n)*100/float64(upper))
		// Calculate prime factors if we n is not prime.
		var fn []int
		if sieve[n] {
			fn = []int{n}
		} else {
			fn = PrimeFactors2(n, primes)
		}
		// Fill factors for the current number.  This results in some
		// unnecessary work towards the end of the outer loop, but is
		// overall more efficient because PrimeFactors is only called
		// once for each number.
		for _, f := range fn {
			cumF[f]++
		}
		for _, f := range fn {
			for k := len(factors[f]); k <= cumF[f]; k++ {
				factors[f] = append(factors[f], n)
			}
		}

		// The lowest number we should even consider is the biggest
		// prime factor of n.  This is inefficient for 97969 (313 *
		// 313), we should start with 626 instead, i.e. 313 + 313.  At
		// first I thought that I'd multiply the highest factor by the
		// number of times it appears, but that doesn't work for 8.  8
		// is 2 * 2 * 2.  4! is the answer we're looking for, but 2 * 3
		// = 6.  So if the second highest prime factor == the highest
		// prime factor, start with 2 * highest prime factor.
		fnm := make(map[int]int)
		for _, f := range fn {
			fnm[f]++
		}
		j := fn[len(fn)-1]
		if fnm[j] > 1 {
			j *= 2
		}

	Inner:
		for {
			// Check if we've found a match.
			for f, v := range fnm {
				if len(factors[f]) < v+1 || factors[f][v] > j {
					j++
					continue Inner
				}
			}
			sum += int64(j)
			continue Outer
		}
	}
	return int64(sum)
}

func projectEuler549_2test() int64 {
	return projectEuler549_2actual(100)
}

func projectEuler549_3actual(upper int) int64 {
	// This is your basic prime sieve.
	sieve := SieveOfEratosthenes(upper)
	// This is a sieve for this problem, calculating future values of s(n).
	sSieve := make([]int, upper+1)
	sum := int64(0)

	for n := 2; n <= upper; n++ {
		logEveryN(10000, "%v / %v %.2f%%\n", n, upper, float64(n)*100/float64(upper))
		// http://mathworld.wolfram.com/SmarandacheFunction.html
		// explains this in detail.

		if sieve[n] {
			sum += int64(n)
			// n divides m! for every m=i*n for 1 <= i < n.
			nPow := 1
			for i := 1; i < n && i*n <= upper; i++ {
				sSieve[n*i] = n
				nPow *= n
				// nPow gets large enough to overflow and become negative.
				if nPow > 0 && nPow < upper {
					// s(p^x) == px
					sSieve[nPow] = n * i
				}
			}
			continue
		}
		if sSieve[n] != 0 {
			sum += int64(sSieve[n])
			continue
		}

		// Need to figure it out from factors.
		// We generate the prime factors for n.
		// We start with m=2, divide m by each factor, and zero out
		// factors that divide m, incrementing m until we have zero
		// factors left.
		facN := PrimeFactors(n, sieve)
		numFac, m := len(facN), 1
		for numFac > 0 {
			m++
			x := m
			for i, f := range facN {
				if f > x {
					break
				}
				if f != 0 && x%f == 0 {
					x /= f
					facN[i] = 0
					numFac--
					if numFac == 0 {
						break
					}
				}
			}
		}
		sum += int64(m)
		sSieve[n] = m

		// Ideally I'd fill up the sieve and save some calculations,
		// but it doesn't work, I get the wrong values for 32, 64, and
		// 96 :(
	}
	return sum
}

func projectEuler549_3test() int64 {
	return projectEuler549_3actual(100)
}
