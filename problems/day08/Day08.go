package main

import (
	"aoc/lib"
	"fmt"
	"strings"
)

func main() {
	in := lib.LinesOfFileOfDay(8)
	p1 := Day08Part1(in)
	p2 := Day08Part2(in)
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

func Day08Part1(input []string) int {
	rhs := make([]string, len(input))
	for i, ln := range input {
		rhs[i] = strings.Trim(strings.Split(ln, "|")[1], " ")
	}
	easyDigits := 0
	for _, output := range rhs {
		tokens := strings.Split(output, " ")
		for _, token := range tokens {
			segments := len(token)
			if in(segments, []int{2, 4, 3, 7}) {
				easyDigits++
			}
		}
	}
	return easyDigits
}

func Day08Part2(input []string) int {
	return 2 * len(input)
}

func in(e int, ls []int) bool {
	for _, x := range ls {
		if e == x {
			return true
		}
	}
	return false
}

func solve(input string) map[int]rune {
	res := map[int]rune{
		1: 'a',
		2: 'b',
		3: 'c',
		4: 'd',
		5: 'e',
		6: 'f',
		7: 'g',
	}
	return res
}
