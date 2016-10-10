package main

import (
	"io"
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
