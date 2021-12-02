package main

import (
	"aoc/lib"
	"fmt"
	"strconv"
	"strings"
)

type sub struct {
	X     int
	Depth int
}

type aimedSub struct {
	Sub sub
	Aim int
}

type instruction struct {
	Direction Direction
	Steps     int
}

type Direction int

const (
	Forward Direction = iota
	Down
	Up
)

func main() {
	in := lib.LinesOfFileOfDay(2)
	part1 := Day02Part1(in)
	fmt.Printf("Day 02 part 1: %d\n", part1)
	part2 := Day02Part2(in)
	fmt.Printf("Day 02 part 2: %d\n", part2)
}

func Day02Part1(lines []string) int {
	sub := &sub{}
	instructions := parseInstructions(lines)
	for _, instruction := range instructions {
		sub.execute(instruction)
	}
	return sub.Depth * sub.X
}

func Day02Part2(lines []string) int {
	aimedSub := &aimedSub{}
	instructions := parseInstructions(lines)
	for _, instruction := range instructions {
		aimedSub.execute(instruction)
	}
	return aimedSub.Sub.Depth * aimedSub.Sub.X
}

func (sub *sub) execute(instruction instruction) {
	switch instruction.Direction {
	case Up:
		sub.Depth -= instruction.Steps
	case Down:
		sub.Depth += instruction.Steps
	case Forward:
		sub.X += instruction.Steps
	}
}

func (sub *aimedSub) execute(instruction instruction) {
	switch instruction.Direction {
	case Up:
		sub.Aim -= instruction.Steps
	case Down:
		sub.Aim += instruction.Steps
	case Forward:
		sub.Sub.X += instruction.Steps
		sub.Sub.Depth += sub.Aim * instruction.Steps
	}
}

func parseInstructions(lines []string) []instruction {
	res := make([]instruction, len(lines))
	for i, ln := range lines {
		parts := strings.Split(ln, " ")
		direction := parseDirection(parts[0])
		steps, _ := strconv.Atoi(parts[1])
		res[i] = instruction{direction, steps}
	}
	return res
}

func parseDirection(dir string) Direction {
	switch dir {
	case "up":
		return Up
	case "down":
		return Down
	}
	return Forward
}
