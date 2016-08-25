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

/*
* A Harshad or Niven number is a number that is divisible by the sum of its
* digits.
* 201 is a Harshad number because it is divisible by 3 (the sum of its digits.)
* When we truncate the last digit from 201, we get 20, which is a Harshad
* number.
* When we truncate the last digit from 20, we get 2, which is also a Harshad
* number.
* Let's call a Harshad number that, while recursively truncating the last digit,
* always results in a Harshad number a right truncatable Harshad number.
*
* Also:
* 201/3=67 which is prime.
* Let's call a Harshad number that, when divided by the sum of its digits,
* results in a prime a strong Harshad number.
*
* Now take the number 2011 which is prime.
* When we truncate the last digit from it we get 201, a strong Harshad number
* that is also right truncatable.
* Let's call such primes strong, right truncatable Harshad primes.
*
* You are given that the sum of the strong, right truncatable Harshad primes
* less than 10000 is 90619.
*
* Find the sum of the strong, right truncatable Harshad primes less than 1014.
 */

func projectEuler387actual() int64 {
	return 0
}

func projectEuler387test() int64 {
	return projectEuler387actual()
}
