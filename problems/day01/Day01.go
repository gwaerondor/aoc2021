package main

import (
	"aoc/lib"
	"fmt"
)

func main() {
	in := lib.IntOfLinesOfFileOfDay(1)
	part1 := Day01Part1(in)
	fmt.Printf("Day 01 part 1: %d\n", part1)
	part2 := Day01Part2(in)
	fmt.Printf("Day 01 part 2: %d\n", part2)
}

func Day01Part1(measurements []int) int {
	increments := 0
	for i := 1; i < len(measurements); i++ {
		if measurements[i] > measurements[i-1] {
			increments++
		}
	}
	return increments
}

func Day01Part2(measurements []int) int {
	increments := 0
	low := 1
	high := 4
	for high < (len(measurements) + 1) {
		this := lib.Sum(measurements[low:high])
		last := lib.Sum(measurements[(low - 1):(high - 1)])
		if this > last {
			increments++
		}
		low++
		high++
	}
	return increments
}
