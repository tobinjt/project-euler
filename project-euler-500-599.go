package main

import "math/big"

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

func projectEuler549_2actual(upper int) int64 {
	sieve := SieveOfEratosthenes(upper)
	// factors tracks the lowest n where n! contains a given number of a certain prime factor.
	// E.g. factors[2][2] = 4, because 4! is the first factorial to contain 2 * 2.
	factors := make([][]int, upper)
	sum := 0
	// cumF tracks the cumulative prime factors of lastF.
	cumF := make(map[int]int)

Outer:
	for n := 2; n <= upper; n++ {
		fn := PrimeFactors(n, sieve)
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
			sum += j
			continue Outer
		}
	}
	return int64(sum)
}

func projectEuler549_2test() int64 {
	return projectEuler549_2actual(100)
}
