package main

import (
	"fmt"
	"aoc/lib"
	"strconv"
	"strings"
)

func main() {
	in := lib.LinesOfFileOfDay(5)
	p1 := Day05Part1(in)
	p2 := Day05Part2(in)
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

func Day05Part1(in []string) int {
	lines := inputToLines(in, parseLineFromString)
	return countIntersectingLines(lines)
}

func Day05Part2(in []string) int {
	lines := inputToLines(in, parseLineFromStringWithDiagonals)
	return countIntersectingLines(lines)
}

type coord struct {
	x int
	y int
}

func inputToLines(in []string, parser func(string) []coord) [][]coord {
	lines := make([][]coord, len(in))
	for i, line := range in {
		lines[i] = parser(line)
	}
	return lines
}

func countIntersectingLines(lines [][]coord) int {
	occurrences := make(map[coord]int)
	for _, ln := range lines {
		for _, coord := range ln {
			occurrences[coord]++
		}
	}
	intersections := 0
	for _, v := range occurrences {
		if v >= 2 {
			intersections++
		}
	}
	return intersections
}

func parseLineFromString(s string) []coord {
	parts := strings.Split(s, " -> ")
	fromParts := strings.Split(parts[0], ",")
	toParts := strings.Split(parts[1], ",")
	fx, _ := strconv.Atoi(fromParts[0])
	fy, _ := strconv.Atoi(fromParts[1])
	tx, _ := strconv.Atoi(toParts[0])
	ty, _ := strconv.Atoi(toParts[1])
	return parseLine(coord{fx, fy}, coord{tx, ty})
}

func parseLineFromStringWithDiagonals(s string) []coord {
	parts := strings.Split(s, " -> ")
	fromParts := strings.Split(parts[0], ",")
	toParts := strings.Split(parts[1], ",")
	fx, _ := strconv.Atoi(fromParts[0])
	fy, _ := strconv.Atoi(fromParts[1])
	tx, _ := strconv.Atoi(toParts[0])
	ty, _ := strconv.Atoi(toParts[1])
	return parseLineWithDiag(coord{fx, fy}, coord{tx, ty})
}

func parseLine(from, to coord) []coord {
	if from.x == to.x {
		ys := lib.Seq(from.y, to.y)
		res := make([]coord, len(ys))
		for i := 0; i < len(ys); i++ {
			res[i] = coord{from.x, ys[i]}
		}
		return res
	}

	if from.y == to.y {
		xs := lib.Seq(from.x, to.x)
		res := make([]coord, len(xs))
		for i := 0; i < len(xs); i++ {
			res[i] = coord{xs[i], from.y}
		}
		return res
	}
	return []coord{}
}

func parseLineWithDiag(from, to coord) []coord {
	if from.x == to.x {
		ys := lib.Seq(from.y, to.y)
		res := make([]coord, len(ys))
		for i := 0; i < len(ys); i++ {
			res[i] = coord{from.x, ys[i]}
		}
		return res
	}
	if from.y == to.y {
		xs := lib.Seq(from.x, to.x)
		res := make([]coord, len(xs))
		for i := 0; i < len(xs); i++ {
			res[i] = coord{xs[i], from.y}
		}
		return res
	}
	xs := lib.Seq(from.x, to.x)
	ys := lib.Seq(from.y, to.y)
	res := make([]coord, len(xs))
	for i:= 0; i < len(xs); i++ {
		res[i] = coord{xs[i], ys[i]}
	}
	return res
}
