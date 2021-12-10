package main

import (
	"testing"
	"aoc/lib"
)

var testData [][]int = lib.DigitsOfLines([]string{
	"2199943210",
	"3987894921",
	"9856789892",
	"8767896789",
	"9899965678",
})

func TestP1(t *testing.T) {
	expected := 15
	actual := P1(testData)
	if expected != actual {
		t.Fatalf("Expected %d, got %d", expected, actual)
	}
}

func TestP2(t *testing.T) {
	expected := 5
	actual := P2(testData)
	if expected != actual {
		t.Fatalf("Expected %d, got %d", expected, actual)
	}
}
