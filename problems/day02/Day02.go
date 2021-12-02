package main

import (
	"aoc/lib"
	"fmt"
)

type sub struct {
	X     int
	Depth int
}

type aimedSub struct {
	Sub sub
	Aim int
}

func main() {
	in := lib.SIPairsOfLinesOfFileOfDay(2, " ")
	part1 := Day02Part1(in)
	fmt.Printf("Day 02 part 1: %d\n", part1)
	part2 := Day02Part2(in)
	fmt.Printf("Day 02 part 2: %d\n", part2)
}

func Day02Part1(pairs []lib.SIPair) int {
	sub := &sub{}
	for _, instruction := range pairs {
		sub.execute(instruction)
	}
	return sub.Depth * sub.X
}

func Day02Part2(pairs []lib.SIPair) int {
	aimedSub := &aimedSub{}
	for _, instruction := range pairs {
		aimedSub.execute(instruction)
	}
	return aimedSub.Sub.Depth * aimedSub.Sub.X
}

func (sub *sub) execute(instruction lib.SIPair) {
	switch instruction.String {
	case "up":
		sub.Depth -= instruction.Int
	case "down":
		sub.Depth += instruction.Int
	case "forward":
		sub.X += instruction.Int
	}
}

func (sub *aimedSub) execute(instruction lib.SIPair) {
	switch instruction.String {
	case "up":
		sub.Aim -= instruction.Int
	case "down":
		sub.Aim += instruction.Int
	case "forward":
		sub.Sub.X += instruction.Int
		sub.Sub.Depth += sub.Aim * instruction.Int
	}
}
