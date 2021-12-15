package main

import (
	"fmt"
	"aoc/lib"
)

func main() {
	input := lib.DigitsOfLinesOfFileOfDay(16)
	p1 := P1(input)
	fmt.Printf("Part 1: %d\n", p1)
	//fmt.Printf("Part 2: %d\n", p1)
}

func P1(input [][]int) int {
	// Add some sort of A* pathfinding algorithm
	return len(input)
}

