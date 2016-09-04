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

func growBigIntSlice(s []*big.Int, i int) []*big.Int {
	sn := make([]*big.Int, len(s)+i)
	copy(sn, s)
	return sn
}

func projectEuler549actual(upper int64) int64 {
	fCache := []*big.Int{big.NewInt(1), big.NewInt(1), big.NewInt(2)}
	lastFC := 2
	var sum int64

Outer:
	for i := int64(2); i <= upper; i++ {
		zero := big.NewInt(0)
		div := big.NewInt(i)
		// Search the cache first.
		tmp := big.NewInt(0)
		for m := 0; m <= lastFC; m++ {
			tmp.Mod(fCache[m], div)
			if zero.Cmp(tmp) == 0 {
				// fmt.Printf("%v => %v => %v\n", i, m, fCache[m])
				sum += int64(m)
				continue Outer
			}
		}

		for {
			lastFC++
			if lastFC == len(fCache) {
				fCache = growBigIntSlice(fCache, 10)
			}
			fCache[lastFC] = factorialBigInt(int64(lastFC))
			mod := big.NewInt(0)
			mod.Mod(fCache[lastFC], div)
			if mod.Cmp(zero) == 0 {
				// fmt.Printf("%v => %v => %v\n", i, lastFC, fCache[lastFC])
				sum += int64(lastFC)
				continue Outer
			}
		}
	}
	return sum
}

func projectEuler549test() int64 {
	return projectEuler549actual(100)
}
