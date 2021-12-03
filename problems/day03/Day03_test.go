package main

import (
	"aoc/bitstring"
	"aoc/lib"
	"testing"
)

var testData []string = []string{
	"00100",
	"11110",
	"10110",
	"10111",
	"10101",
	"01111",
	"00111",
	"11100",
	"10000",
	"11001",
	"00010",
	"01010",
}

var parsed bitstring.Bitmatrix = parse(testData)

func TestDay03Part1(t *testing.T) {
	exp := 198
	if res := Day03Part1(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestDay03Part2(t *testing.T) {
	exp := 230
	if res := Day03Part2(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestGamma(t *testing.T) {
	exp := 22
	if res := Gamma(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestEpsilon(t *testing.T) {
	exp := 9
	if res := Epsilon(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestOxygen(t *testing.T) {
	exp := 23
	if res := Oxygen(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestCO2Scrubber(t *testing.T) {
	exp := 10
	if res := CO2Scrubber(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestActualSolution(t *testing.T) {
	in := parse(lib.LinesOfFileOfDay(3))
	t.Run("Part 1", func(t *testing.T) {
		exp := 1458194
		in := parse(lib.LinesOfFileOfDay(3))
		if res := Day03Part1(in); res != exp {
			t.Fatalf("Expected %d, got %d", exp, res)
		}
	})
	t.Run("Part 2", func(t *testing.T) {
		exp := 2829354
		if res := Day03Part2(in); res != exp {
			t.Fatalf("Expected %d, got %d", exp, res)
		}
	})
}
