/*
* Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
* where each “_” is a single digit.
 */

package main

import "math"

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

func projectEuler205actual() int64 {
	return 0
}

func projectEuler205test() int64 {
	return projectEuler205actual()
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
