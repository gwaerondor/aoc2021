package main

import (
	"aoc/lib"
	"fmt"
	"strconv"
)

func main() {
	in := lib.LinesOfFileOfDay(3)
	p1 := Day03Part1(in)
	p2 := Day03Part2(in)
	fmt.Printf("Part 1: %v\nPart 2: %v\n", p1, p2)
}

func Day03Part1(in []string) int64 {
	gamma, _ := strconv.ParseInt(Gamma(in), 2, 32)
	epsilon, _ := strconv.ParseInt(Epsilon(in), 2, 32)
	return gamma * epsilon
}

func Day03Part2(in []string) int64 {
	o2, _ := strconv.ParseInt(Oxygen(in), 2, 32)
	co2, _ := strconv.ParseInt(CO2Scrubber(in), 2, 32)
	return o2 * co2
}

func Gamma(in []string) string {
	gamma := make([]rune, len(in[0]))
	for i, _ := range in[0] {
		thisPos := allFromPos(i, in)
		gamma[i] = mostCommon(thisPos, '0')
	}
	return string(gamma)
}

func Epsilon(in []string) string {
	gamma := Gamma(in)
	epsilon := make([]rune, len(gamma))
	for i, e := range gamma {
		switch e {
		case '1':
			epsilon[i] = '0'
		default:
			epsilon[i] = '1'
		}
	}
	return string(epsilon)
}

func Oxygen(in []string) string {
	i := 0
	remainder := in
	for len(remainder) > 1 {
		mostCommon := mostCommon(allFromPos(i, remainder), '1')
		remainder = filterOnRune(mostCommon, i, remainder)
		i++
	}
	return remainder[0]
}

func CO2Scrubber(in []string) string {
	i := 0
	remainder := in
	for len(remainder) > 1 {
		leastCommon := leastCommon(allFromPos(i, remainder), '0')
		remainder = filterOnRune(leastCommon, i, remainder)
		i++
	}
	return remainder[0]
}

func filterOnRune(keep rune, pos int, in []string) []string {
	res := make([]string, 0)
	for _, s := range in {
		if rune(s[pos]) == keep {
			res = append(res, s)
		}
	}
	return res
}

func allFromPos(pos int, strs []string) []rune {
	res := make([]rune, len(strs))
	for i, str := range strs {
		res[i] = rune(str[pos])
	}
	return res
}

func mostCommon(runes []rune, bias rune) rune {
	zeros := 0
	ones := 0
	for _, r := range runes {
		switch r {
		case '0':
			zeros++
		case '1':
			ones++
		}
	}
	if zeros == ones {
		return bias
	}
	if zeros > ones {
		return '0'
	}
	return '1'
}

func leastCommon(runes []rune, bias rune) rune {
	nonBias := '0'
	if bias == '0' {
		nonBias = '1'
	}
	if mostCommon(runes, nonBias) == '0' {
		return '1'
	}
	return '0'
}
