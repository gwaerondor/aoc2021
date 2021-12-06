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
	occurrences := make(map[coord]int)
	for _, ln := range in {
		line := parseLineFromStringWithDiagonals(ln)
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
