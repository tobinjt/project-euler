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
* Find the sum of the strong, right truncatable Harshad primes less than 10^14.
 */

/*
* Thoughts:
* - I can't brute force this, 10^14 is huge.
* - I think generating Harshad numbers is the way to go.
*  - 1-9 are Harshad numbers, and those will be my initial numbers.
*  - For every Harshad number I have, append the digits 1-9 and check if it's a
*    Harshad number.
*  - If it's not a Harshad number, check if it's prime, and if
*    it/sum-of-digits is prime.
*  - I should store the sum of the digits alongside each Harshad number.
 */

func growInt64Slice(s []int64, i int) []int64 {
	sn := make([]int64, len(s)+i)
	copy(sn, s)
	return sn
}

// isInt64Prime checks is n is prime through trial division, returning true if it is.
func isInt64Prime(n int64) bool {
	// Special-case the numbers that the loop doesn't work for.
	if n < 2 {
		return false
	}
	if n == 2 {
		return true
	}
	if n%2 == 0 {
		return false
	}
	u := int64(math.Sqrt(float64(n))) + 1
	for i := int64(3); i <= u; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func projectEuler387actual(upper int64) int64 {
	// Every number in this slice is a right-truncatable Harshad number.
	harshads := []int64{1, 2, 3, 4, 5, 6, 7, 8, 9}
	sums := make([]int64, len(harshads))
	copy(sums, harshads)
	// Index of the next number to process.
	index := len(harshads) - 1
	// How much to grow slices by.  This turns out to be an unnecessary optimisation.
	inc := 10
	var result int64

Outer:
	for index >= 0 {
		h := harshads[index]
		s := sums[index]
		index--
		for i := int64(0); i <= 9; i++ {
			h1 := (h * 10) + i
			s1 := s + i
			if h1 > upper {
				continue Outer
			}
			if h1%s1 == 0 {
				index++
				if index == len(harshads) {
					// fmt.Printf("Growing harshads from %v to %v\n", len(harshads), len(harshads)+inc)
					harshads = growInt64Slice(harshads, inc)
					sums = growInt64Slice(sums, inc)
				}
				harshads[index] = h1
				sums[index] = s1
				continue
			}
			// Is the new number prime, and the Harshad number it came from a strong Harshad number?
			if isInt64Prime(h1) && isInt64Prime(h/s) {
				// fmt.Printf("%v is good\n", h1)
				result += h1
			}
		}
	}
	return result
}

func projectEuler387test() int64 {
	return projectEuler387actual(10000)
}
