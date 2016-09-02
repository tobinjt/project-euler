package main

import "math/big"

/*
* 70 colored balls are placed in an urn, 10 for each of the seven rainbow colors.
*
* What is the expected number of distinct colors in 20 randomly picked balls?
*
* Give your answer with nine digits after the decimal point (a.bcdefghij).
 */

/*
* See https://en.wikipedia.org/wiki/Expected_value
*
* Short version:
* - The probability of each colour being picked is P(i) for 1 <= i <= 7.
* - The expected value is the sum of the probabilities for each colour.
* - The probability for each colour is the same, so the expected value is 7*P(1).
* - P(1) == 1-P(1 is not chosen)
* - P(1 is not chosen) == (60 choose 20)/(70 choose 20), i.e. the number of
*   combinations without 1 divided by the number of combinations of all colours.
* - n choose k == n!/k!(n-k)!.
* - We can simplify the calculation some:
*   - (60! / 20! * (60-20)!) / (70! / 20! * (70-20)!)
*   - Change divide to multiply and invert the divisor.
*   - (60! * 20! * (70-20)!) / (70! * 20! * (60-20)!)
*   - (60! * 50!) / (70! * 40!)
*   - (50 * 49 * 48 * 47 * 46 * 45 * 44 * 43 * 42 * 41) / (70 * 69 * 68 * 67 * 66 * 65 * 64 * 63 * 62 * 61)
*   - P(1 is not chosen) => .0258940282
* - 7 * (1 - .0258940282)
* - 6.818741802
 */

func factorialInt64(n int64) int64 {
	r := int64(1)
	for n > 1 {
		r *= n
		n--
	}
	return r
}

func factorialBigInt(n int64) *big.Int {
	r := big.NewInt(1)
	bn := big.NewInt(n)
	one := big.NewInt(1)
	for bn.Cmp(one) == 1 {
		r.Mul(r, bn)
		bn.Sub(bn, one)
	}
	return r
}

func nCrInt64(n, r int64) int64 {
	d := factorialInt64(r) * factorialInt64(n-r)
	return factorialInt64(n) / d
}

func nCrBigInt(n, r int64) *big.Int {
	d := factorialBigInt(r)
	d.Mul(d, factorialBigInt(n-r))
	v := factorialBigInt(n)
	v.Div(v, d)
	return v
}

func projectEuler493() int64 {
	num := big.NewFloat(0).SetInt(nCrBigInt(60, 20))
	den := big.NewFloat(0).SetInt(nCrBigInt(70, 20))
	nc := big.NewFloat(0).Quo(num, den)
	res := big.NewFloat(1)
	res.Sub(res, nc).Mul(res, big.NewFloat(7))
	// Scale up by many orders of magnitude so we can convert to int64.
	res.Mul(res, big.NewFloat(1000*1000*1000))
	i, _ := res.Int64()
	return i
}
