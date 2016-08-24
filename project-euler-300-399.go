package main

import "math"

/*
* Consider the divisors of 30: 1,2,3,5,6,10,15,30.
* It can be seen that for every divisor d of 30, d+30/d is prime.
*
* Find the sum of all positive integers n not exceeding 100 000 000
* such that for every divisor d of n, d+n/d is prime.
 */

func meetsPE357Criteria(n int, sieve []bool) bool {
	l := int(math.Ceil(math.Sqrt(float64(n))))
	for d1 := 1; d1 <= l; d1++ {
		if n%d1 == 0 {
			d2 := n / d1
			p1 := d1 + (n / d1)
			p2 := d2 + (n / d2)
			if !sieve[p1] || !sieve[p2] {
				return false
			}
		}
	}
	return true
}

func projectEuler357actual(upper int) int64 {
	sieve := SieveOfEratosthenes(upper + 1)
	sum := 0
	for i := 1; i <= upper; i++ {
		if meetsPE357Criteria(i, sieve) {
			sum += i
		}
	}
	return int64(sum)
}

func projectEuler357test() int64 {
	return projectEuler357actual(30)
}
