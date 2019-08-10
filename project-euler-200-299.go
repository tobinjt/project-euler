/*
* Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
* where each “_” is a single digit.
 */

package main

import (
	"math"
)

/*
* Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
* Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

* Peter and Colin roll their dice and compare totals: the highest total wins.
* The result is a draw if the totals are equal.

* What is the probability that Pyramidal Pete beats Cubic Colin? Give your
* answer rounded to seven decimal places in the form 0.abcdefg
 */

/*
* 4^9 = 262144
* 6^6 = 46656
* Idea:
*  - generate and count the totals of the permutations of 6^6
*  - iterate over the permutations of 4^9.
*    - 1) calculate the fraction of times the current 4^9 total comes up.
*    - 2) calculate the fraction of 6^6 rolls that are less and the fraction
*      that are equal.
*    - 3) multiply 1 by 2 and add it to a running total.
*
* It would be nice to calculate the number of ways to achieve each roll
* rather than brute forcing it.
 */

// incrementDice increments the values in dice, returning false when there
// aren't any more values to use.
func incrementDice(dice []int, sides int) bool {
	for i := len(dice) - 1; i >= 0; i-- {
		if dice[i] == sides {
			dice[i] = 1
		} else {
			dice[i]++
			return true
		}
	}
	return false
}

func sumIntSlice(s []int) int {
	sum := 0
	for _, v := range s {
		sum += v
	}
	return sum
}

// IntPair represents a key, value pair where both are ints.
type IntPair struct {
	key, value int
}

func projectEuler205() int64 {
	// Deliberately start off at 0 so that the first loop iteration brings
	// it to the first valid combination.
	cubicDice := []int{1, 1, 1, 1, 1, 0}
	cubicCounts := make([]int, 37)
	for incrementDice(cubicDice, 6) {
		sum := sumIntSlice(cubicDice)
		cubicCounts[sum]++
	}
	cubicNumRolls := sumIntSlice(cubicCounts)
	cubicIncCounts := make([]int, len(cubicCounts))
	for i, v := range cubicCounts {
		if i != 0 {
			cubicIncCounts[i] = cubicIncCounts[i-1] + v
		}
	}

	pyramidalDice := []int{1, 1, 1, 1, 1, 1, 1, 1, 0}
	pyramidalCounts := make([]int, 37)
	for incrementDice(pyramidalDice, 4) {
		sum := sumIntSlice(pyramidalDice)
		pyramidalCounts[sum]++
	}
	pyramidalNumRolls := sumIntSlice(pyramidalCounts)

	var pWinSum float64
	for roll, rollCount := range pyramidalCounts {
		if rollCount == 0 {
			continue
		}
		// Probability of pyramidal rolling roll.
		pRoll := float64(rollCount) / float64(pyramidalNumRolls)
		// Probability of cubic rolling less than roll.
		pLess := float64(cubicIncCounts[roll-1]) / float64(cubicNumRolls)
		// Probability of roll winning for pyramidal.
		pWin := pRoll * pLess
		pWinSum += pWin
	}

	// Scale up by many orders of magnitude so we can convert to int64.
	// Add 1 because the conversion rounds down.
	return int64(pWinSum*10*1000*1000) + 1
}

/*
* The answer lies between 1121314151617181910 and 1929394959697989990, so we
* have upper and lower bounds for the integers to square.
*
* After solving it I realised that the solution is much closer to the upper
* bound than the lower, so I start at the upper bound and work downwards to
* make testing much faster.
 */

func projectEuler206() int64 {
	digits := []int64{0, 9, 8, 7, 6, 5, 4, 3, 2}
	i := int64(math.Sqrt(1929394959697989990)) + 1

Loop:
	for {
		i--
		v := i * i
		for _, digit := range digits {
			if v%10 != digit {
				continue Loop
			}
			v = v / 100
		}
		return i
	}
}

/*
* A positive fraction whose numerator is less than its denominator is called a
* proper fraction.
* For any denominator, d, there will be d-1 proper fractions; for example, with
* d = 12: 1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12, 10/12, 11/12.
*
* We shall call a fraction that cannot be cancelled down a resilient fraction.
* Furthermore we shall define the resilience of a denominator, R(d), to be the
* ratio of its proper fractions that are resilient; for example, R(12) = 4/11.
* In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10.
*
* Find the smallest denominator d, having a resilience R(d) < 15499/94744.
 */

/*
* What makes a fraction n/d resilient?  It's resilient when n and d are
* relatively prime, viz gcd(n, d) == 1.  Euler's function phi(d) calculates the
* number of integers 0 < n <= d that are relatively prime to d.  So I can
* probably loop until I find phi(d-1)/d < 15499/94744.
*
* That approach works for the example, but it doesn't work for the real
* question because the lookup table is too large.
 */

// Phi returns the number of positive integers < n that are relatively prime
// to n; primes is a slice of prime numbers.
func Phi(n int, primes []int) int {
	phi := float64(n)
	for _, prime := range primes {
		if prime > n {
			return int(phi)
		}
		if n%prime == 0 {
			phi *= (float64(1) - (float64(1) / float64(prime)))
		}
	}
	return -1
}

func projectEuler243actual(n int, d int, m int) int64 {
	target := float64(n) / float64(d)
	for {
		phiTable := MakePhiLookupTable(d * m)
		for i, phi := range phiTable {
			if i < 3 {
				continue
			}
			ratio := float64(phi) / float64(i-1)
			if ratio < target {
				return int64(i)
			}
		}
		m *= 2
	}
}

func projectEuler243test() int64 {
	return projectEuler243actual(4, 10, 1)
}
