package main

import (
	"io"
	"math"
	"strings"
)

/*
* Three distinct points are plotted at random on a Cartesian plane, for which
* -1000 ≤ x, y ≤ 1000, such that a triangle is formed.
*
* Consider the following two triangles:
*
*   A(-340,495), B(-153,-910), C(835,-947)
*
*   X(-175,41), Y(-421,-714), Z(574,-645)
*
* It can be verified that triangle ABC contains the origin, whereas triangle
* XYZ does not.
*
* Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text
* file containing the co-ordinates of one thousand "random" triangles, find the
* number of triangles for which the interior contains the origin.
*
* NOTE: The first two examples in the file represent the triangles in the
* example given above.
 */

// pointWithinTriangle determines if a point is within a triangle, as described in
// http://www.gamedev.net/topic/295943-is-this-a-better-point-in-triangle-test-2d/
func pointWithinTriangle(x, y, x1, y1, x2, y2, x3, y3 int64) bool {
	b1 := pointToLineCrossProduct(x, y, x1, y1, x2, y2) < 0
	b2 := pointToLineCrossProduct(x, y, x2, y2, x3, y3) < 0
	b3 := pointToLineCrossProduct(x, y, x3, y3, x1, y1) < 0
	return (b1 == b2) && (b2 == b3)
}

// pointToLineCrossProduct is called Sign in
// http://www.gamedev.net/topic/295943-is-this-a-better-point-in-triangle-test-2d/
func pointToLineCrossProduct(x, y, x1, y1, x2, y2 int64) int64 {
	return (x-x2)*(y1-y2) - (x1-x2)*(y-y2)
}

func projectEuler102actual(r io.Reader) int64 {
	triangles := readIntsFromCSVFile(r)
	res := 0
	for _, t := range triangles {
		if pointWithinTriangle(0, 0, t[0], t[1], t[2], t[3], t[4], t[5]) {
			res++
		}
	}
	return int64(res)
}

func projectEuler102test() int64 {
	data := `-340,495,-153,-910,835,-947
-175,41,-421,-714,574,-645`
	return projectEuler102actual(strings.NewReader(data))
}

/*
* Working from left-to-right if no digit is exceeded by the digit to its left it
* is called an increasing number; for example, 134468.
*
* Similarly if no digit is exceeded by the digit to its right it is called a
* decreasing number; for example, 66420.
*
* We shall call a positive integer that is neither increasing nor decreasing a
* "bouncy" number; for example, 155349.
*
* Clearly there cannot be any bouncy numbers below one-hundred, but just over
* half of the numbers below one-thousand (525) are bouncy. In fact, the least
* number for which the proportion of bouncy numbers first reaches 50% is 538.
*
* Surprisingly, bouncy numbers become more and more common and by the time we
* reach 21780 the proportion of bouncy numbers is equal to 90%.
*
* Find the least number for which the proportion of bouncy numbers is exactly
* 99%.
 */

func projectEuler112actual() int64 {
	return 0
}

func projectEuler112test() int64 {
	return projectEuler112actual()
}

/*
* See https://projecteuler.net/problem=144 - there's an image that really helps
* with understanding.
*
* Initial thoughts:
* - 1 Calculate the slope of the line from the start point and the impact point.
*     m = y1-y2/x1-x2.
* - 2 Calculate the slope of the tangent line from the intersection point.
*     m = −4x/y.
* - 3 Calculate the slope of the normal to the tangent line
*     pm = -1/m.
* - 4 Calculate the slope of the outgoing line given the slope of the incoming
*     line and the slope of the normal.  m1 = slope of incoming line; m2 = slope
*     of normal; m3 = slope of outgoing line (this is what I'm calculating).
*     - tan x = (m2-m1)/(1+m1m2) = (m3-m2)/(1+m2m3)
*     - (m2-m1)/(1+m1m2) = (m3-m2)/(1+m2m3)
*     - (m2-m1)(1+m2m3) = (m3-m2)(1+m1m2)
*     - m2 - m1 + m2m2m3 - m1m2m3 = m3 - m2 + m3m1m2 - m2m1m2
*     - Reorder m2m1 to m1m2 and so on for easier comparisons.
*     - m2 - m1 + m2m2m3 - m1m2m3 = m3 - m2 + m1m2m3 - m1m2m2
*     - Add (m2 + m1m2m3) to both sides.
*     - 2m2 - m1 + m2m2m3 = m3 + 2m1m2m3 - m1m2m2
*     - Move all terms containing m3 to one side, all other terms to the other.
*     - 2m2 - m1 + m1m2m2 = m3 + 2m1m2m3 - m2m2m3
*     - Isolate m3 from all terms on the right.
*     - 2m2 - m1 + m1m2m2 = m3(1 + 2m1m2 - m2m2)
*     - Divide both sides by (1 + 2m1m2 - m2m2) so the right side is just m3.
*     - (2m2 - m1 + m1m2m2)/(1 + 2m1m2 - m2m2) = m3
*     - Now I have a formula to calculate m3 from m1 and m2.
* - 5 The outgoing line now becomes the incoming line.  I need to express it as
*     y = mx + c, so I calculate c = y - mx.
* - 6 Calculate where the outgoing line intersects with the ellipse.
*     - Ellipse: 4xx + yy = 100
*     - Line: y = mx + c
*     - 4xx + (mx + c)(mx + c) = 100
*     - 4xx + mxmx + mxc + mxc + cc = 100
*     - 4xx + mmxx + 2mxc + cc = 100
*     - (4 + mm)xx + (2mc)x + (cc - 100) = 0
*     - Use https://en.wikipedia.org/wiki/Quadratic_formula
*     - One x will be the current intersection, the other x will be the new
*	intersection.
*     - Solve for y.
* - 7 Check if the intersection is within the gap at the top; break if it is,
*     GOTO 2 otherwise.
 */

func slopeOfLine(x1, y1, x2, y2 float64) float64 {
	return (y1 - y2) / (x1 - x2)
}

// Implements steps 3 and 4 of the plan above.
func slopeOfReflectedLine(incomingSlope, tangentSlope float64) float64 {
	normalSlope := float64(-1) / tangentSlope
	// (2m2 - m1 + m1m2m2)/(1 + 2m1m2 - m2m2) = m3
	n := (2 * normalSlope) - incomingSlope
	n += incomingSlope * normalSlope * normalSlope
	d := 1 + (2 * incomingSlope * normalSlope)
	d -= normalSlope * normalSlope
	return n / d
}

// https://en.wikipedia.org/wiki/Quadratic_formula
func quadraticFormula(a, b, c float64) (float64, float64) {
	root := math.Sqrt((b * b) - (4 * a * c))
	a1 := ((-1 * b) - root) / (2 * a)
	a2 := ((-1 * b) + root) / (2 * a)
	return a1, a2
}

// Calculate both points where the line given by y = mx + c and the ellipse
// given by 4xx + yy = 0 intersect.
// Implements step 6 above.
func intersectionOfLineAndEllipse(m, c float64) (x1, y1, x2, y2 float64) {
	a := 4 + (m * m)
	b := 2 * m * c
	d := (c * c) - 100
	x1, x2 = quadraticFormula(a, b, d)
	y1 = (m * x1) + c
	y2 = (m * x2) + c
	return
}

// floatsAreClose checks that the first precision digits after the decimal point
// of a and b are equal.  E.g. floatsAreClose(t, "", 0.1234567, 0.1234569, 6)
// will return true
// BEWARE: floating point precision is inaccurate enough that
// floatsAreClose(2.123, 2.124, 3) will return true because 0.1**3 ==
// 0.0010000000000000002 and 2.124-2.123 == 0.0009999999999998899.
func floatsAreClose(a, b float64, precision int) bool {
	threshold := math.Pow(0.1, float64(precision))
	difference := math.Abs(a - b)
	return difference < threshold
}

func projectEuler144actual() int64 {
	return 0
}

func projectEuler144test() int64 {
	return projectEuler144actual()
}
