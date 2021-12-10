package main

import (
	"aoc/lib"
	"fmt"
)

func main() {
	input := lib.DigitsOfLinesOfFileOfDay(9)
	p1 := P1(input)
	p2 := P2(input)
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

func P1(input [][]int) int {
	danger := 0
	for y, row := range input {
		for x, point := range row {
			if isLowPoint(x, y, input) {
				danger += point + 1
			}
		}
	}
	return danger
}

func P2(input [][]int) int {
	return len(input)
}

func isLowPoint(x, y int, m [][]int) bool {
	surrounding := surroundingTopology(x, y, m)
	for _, s := range surrounding {
		if s <= m[y][x] {
			return false
		}
	}
	return true
}

func surroundingTopology(x, y int, m [][]int) []int {
	res := make([]int, 0)
	// North
	if y - 1 >= 0 {
		res = append(res, m[y-1][x])
	}
	// South
	if y + 1 < len(m) {
		res = append(res, m[y+1][x])
	}
	// West
	if x - 1 >= 0 {
		res = append(res, m[y][x-1])
	}
	// East
	if x + 1 < len(m[0]) {
		res = append(res, m[y][x+1])
	}
	return res
}
