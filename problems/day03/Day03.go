package main

import (
	"aoc/bitstring"
	"aoc/lib"
	"fmt"
)

func main() {
	in := lib.BitmatrixOfFileOfDay(3)
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
	gamma := getMostCommonByColumn(in)
	return gamma.ToInt()
}

func Epsilon(in bitstring.Bitmatrix) int {
	epsilon := getMostCommonByColumn(in).Flip()
	return epsilon.ToInt()
}

func getMostCommonByColumn(m bitstring.Bitmatrix) bitstring.Bitstring {
	transposed := m.Transpose()
	res := make(bitstring.Bitstring, len(transposed))
	for i, row := range transposed {
		res[i] = row.MostCommon(bitstring.ToBit(1))
	}
	return res
}

func Oxygen(in bitstring.Bitmatrix) int {
	for i := 0; len(in) > 1; i++ {
		transposed := in.Transpose()
		mostCommon := transposed[i].MostCommon(bitstring.ToBit(1))
		in = in.FilterOnBit(mostCommon, i)
	}
	return in[0].ToInt()
}

func CO2Scrubber(in bitstring.Bitmatrix) int {
	for i := 0; len(in) > 1; i++ {
		transposed := in.Transpose()
		leastCommon := transposed[i].LeastCommon(bitstring.ToBit(0))
		in = in.FilterOnBit(leastCommon, i)
	}
	return in[0].ToInt()
}
