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
	occurrences := make(map[coord]int)
	for _, ln := range in {
		line := parseLineFromString(ln)
		for _, coord := range line {
			occurrences[coord]++
		}
	}
	atLeastTwo := 0
	for _, v := range occurrences {
		if v >= 2 {
			atLeastTwo++
		}
	}
	return atLeastTwo
}

func Day05Part2(in []string) int {
	return len(in) * 2
}

type coord struct {
	x int
	y int
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

// func parseLineFromStringWithDiagonals(s string) []coord {
// 	parts := strings.Split(s, " -> ")
// 	fromParts := strings.Split(parts[0], ",")
// 	toParts := strings.Split(parts[1], ",")
// 	fx, _ := strconv.Atoi(fromParts[0])
// 	fy, _ := strconv.Atoi(fromParts[1])
// 	tx, _ := strconv.Atoi(toParts[0])
// 	ty, _ := strconv.Atoi(toParts[1])
// }

func parseLine(from, to coord) []coord {
	res := make([]coord, 0)
	if from.x == to.x || from.y == to.y {
		if from.x > to.x {
			tmp := to.x
			to.x = from.x
			from.x = tmp
		}
		if from.y > to.y {
			tmp := to.y
			to.y = from.y
			from.y = tmp
		}
		for x := from.x; x <= to.x; x = towards(x, to.x) {
			for y := from.y; y <= to.y; y = towards(y, to.y) {
				res = append(res, coord{x, y})
			}
		}
	}
	return res
}

func towards(from, to int ) int {
	if from > to {
		return from - 1
	}
	return from + 1
}

func linesEqual(a, b []coord) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !in(a[i], b) {
			return false
		}
	}
	return true
}

func in(wanted coord, coords []coord) bool {
	for _, coord := range coords {
		if coord == wanted {
			return true
		}
	}
	return false
}
