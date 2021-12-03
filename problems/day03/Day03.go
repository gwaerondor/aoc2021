package main

import (
	"aoc/lib"
	"fmt"
)

func main() {
	in := parse(lib.LinesOfFileOfDay(3))
	p1 := Day03Part1(in)
	p2 := Day03Part2(in)
	fmt.Printf("Part 1: %v\nPart 2: %v\n", p1, p2)
}

func parse(input []string) [][]bool {
	parsed := make([][]bool, len(input))
	for i, row := range input {
		parsed[i] = lib.ParseBits(row)
	}
	return parsed
}

func Day03Part1(in [][]bool) int {
	gamma := Gamma(in)
	epsilon := Epsilon(in)
	return gamma * epsilon
}

func Day03Part2(in [][]bool) int {
	o2 := Oxygen(in)
	co2 := CO2Scrubber(in)
	return o2 * co2
}

func Gamma(in [][]bool) int {
	transposed := lib.TransposeBits(in)
	res := make([]bool, 0)
	for _, row := range transposed {
		res = append(res, mostCommon(row, true))
	}
	return lib.BinaryToInt(res)
}

func Epsilon(in [][]bool) int {
	transposed := lib.TransposeBits(in)
	res := make([]bool, 0)
	for _, row := range transposed {
		res = append(res, leastCommon(row, true))
	}
	return lib.BinaryToInt(res)
}

func Oxygen(in [][]bool) int {
	i := 0
	remainder := in
	for len(remainder) > 1 {
		transposed := lib.TransposeBits(remainder)
		mostCommon := mostCommon(transposed[i], true)
		remainder = filterOnBit(mostCommon, i, remainder)
		i++
	}
	return lib.BinaryToInt(remainder[0])
}

func CO2Scrubber(in [][]bool) int {
	i := 0
	remainder := in
	for len(remainder) > 1 {
		transposed := lib.TransposeBits(in)
		leastCommon := leastCommon(transposed[i], false)
		remainder = filterOnBit(leastCommon, i, remainder)
		i++
	}
	return lib.BinaryToInt(remainder[0])
}

func filterOnBit(keep bool, pos int, in [][]bool) [][]bool {
	res := make([][]bool, 0)
	for _, bin := range in {
		if bin[pos] == keep {
			res = append(res, bin)
		}
	}
	return res
}

func mostCommon(bits []bool, bias bool) bool {
	zeros := 0
	ones := 0
	for _, bit := range bits {
		if bit {
			ones++
		} else {
			zeros++
		}
	}
	if zeros == ones {
		return bias
	}
	return ones > zeros
}

func leastCommon(bits []bool, bias bool) bool {
	return !mostCommon(bits, !bias)
}

func pp(b [][]bool) {
	for _, r := range b {
		fmt.Printf("[")
		for _, r2 := range r {
			if r2 {
				fmt.Printf("1")
			} else {
				fmt.Printf("0")
			}
		}
		fmt.Printf("]")
	}
	fmt.Printf("\n")
}
