package main

import (
	"aoc/lib"
	"fmt"
)

func main() {
	in := lib.IntOfCsvOfFileOfDay(7)[0]
	p1 := Day07Part1(in)
	p2 := Day07Part2(in)
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

func Day07Part1(crabs []int) int {
	farLeft := min(crabs)
	farRight := max(crabs)
	fuelSpent := make(map[int]int)
	for i := farLeft; i <= farRight; i++ {
		for _, crab := range crabs {
			fuelSpent[i] += abs(crab - i)
		}
	}
	return smallestValue(fuelSpent)
}

func Day07Part2(in []int) int {
	return -1
}

func min(ns []int) int {
	smallest := ns[0]
	for _, n := range ns {
		if n < smallest {
			smallest = n
		}
	}
	return smallest
}

func max(ns []int) int {
	largest := ns[0]
	for _, n := range ns {
		if n > largest {
			largest = n
		}
	}
	return largest
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func smallestValue(m map[int]int) int {
	smallest := -1
	for _, v := range m {
		if v < smallest || smallest == -1 {
			smallest = v
		}
	}
	return smallest
}
