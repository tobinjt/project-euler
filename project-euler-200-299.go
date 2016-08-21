/*
* Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
* where each “_” is a single digit.
 */

package main

import "math"

/*
* The answer lies between 1121314151617181910 and 1929394959697989990, so we
* have upper and lower bounds for the integers to square.
*
* After solving it I realised that the solution is much closer to the upper
* bound than the lower, so I start at the upper bound and work downwards to
* make testing much faster.
 */

func projectEuler206() int64 {
	lower := int64(math.Sqrt(1121314151617181910))
	upper := int64(math.Sqrt(1929394959697989990))
	digits := []int64{0, 9, 8, 7, 6, 5, 4, 3, 2}

Loop:
	for i := upper; i >= lower; i-- {
		v := i * i
		for _, digit := range digits {
			if v%10 != digit {
				continue Loop
			}
			v = v / 100
		}
		return i
	}
	return 0
}
