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
* Find the unique positive integer whose square has the form
* 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.
*
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
* Let f(N) be the number of points with integer coordinates that are on a circle
* passing through (0,0), (N,0),(0,N), and (N,N).
*
* It can be shown that f(10000) = 36.
*
* What is the sum of all positive integers N ≤ 10^11 such that f(N) = 420 ?
 */

/*
* Thoughts:
* - I can stop working on N as soon as I have more than 420 points.
* - I want to avoid math.Sqrt because of the expense.
* - Circles are symmetric, so I can check 1/4 or maybe 1/8 of it and multiply
*   the result by 4 or 8; need to be careful about doing 1/8 because all the
*   example numbers are multiples of 4 but not 8.
* - Formula for a circle: (x - c)(x - c) + (y - c)(y - c) = (r)(r), where:
*   - (x, y) is the point on the circle.
*   - c is the centre point (N/2, N/2).
*   - r is the radius: sqrt(2(N/2)(N/2)) = sqrt(NN/2) - Pythagoras theorem.
* - If I'm working from x=0 to x=N, I know that I'm interested in y=1, y=2, ...;
*   rather than calculate y can I calculate something smarter?
* - Consider the portion from x=0 to x=N/2; it covers y=0 to y=(about 0.415N/2).
*   Should I iterate over y rather than over x?  Does that result in more
*   expensive calculations that negate the benefit of fewer iterations?
* - Can I do something smarter than plain iteration?  Like binary search or
*   something?
* - I should figure out how to plot N=10,000 and look for patterns.  Done,
*   there's a clear pattern, need to look at more examples to figure out if
*   there's anything usable.
*   https://docs.google.com/spreadsheets/d/1Lb9OyM3AXTi4wTTPm3awIfwQDAZ6Ejv06L5PnIJmlVM/edit#gid=0
* - Will I get the same results if the circle centres are (0, 0) rather than
*   (N/2, N/2)?  Yes, see TestCompareOriginAndOffsetIntegerCoordinates().
*   Formula for a circle centred on the origin is simpler: xx + yy = rr.
 */

// Find both Y values for (x, Y).  (cx, cy) is the centre and r is the radius of
// the circle.
func circleYCoordinates(x, r, cx, cy float64) (float64, float64) {
	// Formula for a circle: (x - cx)(x - cx) + (y - cy)(y - cy) = (r)(r).
	// (x - cx)(x - cx) + (y - cy)(y - cy) = (r)(r)
	// (y - cy)(y - cy) + (x - cx)(x - cx) - (r)(r) = 0
	// (y)(y) - (cy)(y) - (cy)(y) + (cy)(cy) = (r)(r) - (x - cx)(x - cx)
	// (y)(y) - 2(cy)(y) + (cy)(cy) + (x - cx)(x - cx) - (r)(r) = 0
	// Calculate all the terms we know to get yy - by + c = 0, and solve
	// using Quadratic formula.
	b := -2 * cy
	c := cy * cy
	c += (x - cx) * (x - cx)
	c -= r * r
	return quadraticFormula(1, b, c)
}

// Find both Y coordinates for (x, Y) for a circle centred on the origin.
func originCircleYCoordinates(x, r float64) (float64, float64) {
	// xx + yy = rr
	// yy = rr - xx
	// y = sqrt(rr - xx)
	y := math.Sqrt((r * r) - (x * x))
	return -1 * y, y
}

type intPoint struct {
	x, y int
}

func allIntegerCircleCoordinates(cx, cy, n int) map[intPoint]bool {
	coords := make(map[intPoint]bool)
	halfN := n / 2
	r := math.Sqrt(float64(2 * halfN * halfN))
	for x := cx - int(r); x <= cx; x++ {
		var y1, y2 float64
		if cx == 0 && cy == 0 {
			y1, y2 = originCircleYCoordinates(float64(x), r)
		} else {
			y1, y2 = circleYCoordinates(float64(x), r, float64(cx), float64(cy))
		}
		if floatsAreClose(y1, math.Round(y1), 10) {
			iy1, iy2 := int(y1), int(y2)
			coords[intPoint{x, iy1}] = true
			coords[intPoint{x, iy2}] = true
			coords[intPoint{iy1, x}] = true
			coords[intPoint{iy2, x}] = true
			// Reflect the points across the centre.
			rx := cx + (cx - x)
			coords[intPoint{rx, iy1}] = true
			coords[intPoint{rx, iy2}] = true
			coords[intPoint{iy1, rx}] = true
			coords[intPoint{iy2, rx}] = true
		}
	}
	return coords
}

func projectEuler233actual() int64 {
	return 0
}

func projectEuler233test() int64 {
	return projectEuler233actual()
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
*
* Calculating R(n) for 2 <= n < 100,000 and tracking which n results in a lower
* R, I see a pattern: 4 6 12 18 24 30 60 90 120 150 180 - they're all a prime+1.
* Some primes are skipped, e.g. 7+1, 13+1, 19+1.
*
* Another pattern is revealed by expressing n as a product of primes: R(n) gets
* smaller by adding primes, and once a prime is present it is never removed -
* the number of times each prime is present increases and decreases, but
* they're always present at least once.  Why is this?  R(n) is low when there
* are few numbers that are relatively prime to n; the best way to reduce the
* count of numbers relatively prime to n is for n to be composed of many
* primes, because then all the other multiples of each prime are not relatively
* prime to n.
*
* How does that help?  We can figure out the set of primes that the answer must
* be a multiple of.  Multiply consecutive primes together (2*3, 2*3*5, ...) and
* calculate R(n) for each answer.  When R(n) is < 15499/94744 we've gone too
* far, so remove the last prime (a*...*m*m => a*...*m), and the answer must be a
* multiple of that product (thereafter called P).  I can calculate R(n) for all
* n=i*P for increasing i until I find the answer, which will be a lot fewer
* numbers to check than brute force.
*
* Calculating Phi(a big number) is going to be expensive because I need all the
* primes <= a big number.  Instead of calculating from scratch maybe I need to
* use the fact that Phi(n*m) = Phi(n)*Phi(m) when n and m are relatively prime?
* However the numbers aren't going to be relatively prime for the most part, so
* I don't know if that will help?
* That helps for calculating the Phi(upper bound prime), but doesn't help for
* calculating Phi(upper bound prime * m), so it still has to be done the
* expensive way.
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

func projectEuler243actual(target float64) int64 {
	primes := SieveToPrimes(SieveOfEratosthenes(100))
	n, d, upperProduct, upperPrime := 1, 1, 0, 0
	for _, prime := range primes {
		n *= Phi(prime, primes)
		d *= prime
		if float64(n)/float64(d-1) < target {
			break
		}
		upperPrime, upperProduct = prime, d
	}

	// 5 was determined by experimentation; upperPrime+1 would be safer but
	// massively increases the run time.
	primes = SieveToPrimes(SieveOfEratosthenes((upperProduct * 5)))
	answer := -1
	for m := 2; m < upperPrime; m++ {
		d = upperProduct * m
		n = Phi(d, primes)
		if float64(n)/float64(d-1) < target {
			answer = d
			break
		}
	}
	return int64(answer)
}

func projectEuler243test() int64 {
	return projectEuler243actual(4.0 / 10.0)
}
