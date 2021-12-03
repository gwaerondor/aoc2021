package main

import (
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

func TestDay03Part1(t *testing.T) {
	var exp int64 = 198
	if res := Day03Part1(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestDay03Part2(t *testing.T) {
	var exp int64 = 230
	if res := Day03Part2(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestGamma(t *testing.T) {
	exp := "10110"
	if res := Gamma(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestEpsilon(t *testing.T) {
	exp := "01001"
	if res := Epsilon(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestOxygen(t *testing.T) {
	exp := "10111"
	if res := Oxygen(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestCO2Scrubber(t *testing.T) {
	exp := "01010"
	if res := CO2Scrubber(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}
