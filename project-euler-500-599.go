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
		factors := PrimeFactors(int(i), sieve)
		j := factors[len(factors)-1]
		div.SetInt64(i)
		for {
			if fCache[j] == nil {
				fCache[j] = factorialBigInt(int64(j))
			}
			mod.Mod(fCache[j], div)
			if mod.Cmp(zero) == 0 {
				// fmt.Printf("%v => %v => %v\n", i, j, fCache[lastFC])
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
