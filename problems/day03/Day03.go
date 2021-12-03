package main

import (
	"aoc/bitstring"
	"aoc/lib"
	"fmt"
)

func main() {
	in := parse(lib.LinesOfFileOfDay(3))
	p1 := Day03Part1(in)
	p2 := Day03Part2(in)
	fmt.Printf("Part 1: %v\nPart 2: %v\n", p1, p2)
}

func parse(input []string) bitstring.Bitmatrix {
	parsed := make(bitstring.Bitmatrix, len(input))
	for i, row := range input {
		parsed[i] = bitstring.New(row)
	}
	return parsed
}

func Day03Part1(in bitstring.Bitmatrix) int {
	gamma := Gamma(in)
	epsilon := Epsilon(in)
	return gamma * epsilon
}

func Day03Part2(in bitstring.Bitmatrix) int {
	o2 := Oxygen(in)
	co2 := CO2Scrubber(in)
	return o2 * co2
}

func Gamma(in bitstring.Bitmatrix) int {
	transposed := in.Transpose()
	res := make(bitstring.Bitstring, 0)
	for _, row := range transposed {
		res = append(res, row.MostCommon(bitstring.ToBit(1)))
	}
	return res.ToInt()
}

func Epsilon(in bitstring.Bitmatrix) int {
	transposed := in.Transpose()
	res := make(bitstring.Bitstring, 0)
	for _, row := range transposed {
		res = append(res, row.LeastCommon(bitstring.ToBit(1)))
	}
	return res.ToInt()
}

func Oxygen(in bitstring.Bitmatrix) int {
	i := 0
	remainder := in
	for len(remainder) > 1 {
		transposed := remainder.Transpose()
		mostCommon := transposed[i].MostCommon(bitstring.ToBit(1))
		remainder = remainder.FilterOnBit(mostCommon, i)
		i++
	}
	return remainder[0].ToInt()
}

func CO2Scrubber(in bitstring.Bitmatrix) int {
	i := 0
	remainder := in
	for len(remainder) > 1 {
		transposed := in.Transpose()
		leastCommon := transposed[i].LeastCommon(bitstring.ToBit(0))
		remainder = remainder.FilterOnBit(leastCommon, i)
		i++
	}
	return remainder[0].ToInt()
}
