package main

import (
	"testing"
	"aoc/lib"
)

var testData [][]int = lib.DigitsOfLines([]string{
	"1163751742",
	"1381373672",
	"2136511328",
	"3694931569",
	"7463417111",
	"1319128137",
	"1359912421",
	"3125421639",
	"1293138521",
	"2311944581",
})

func TestP1(t *testing.T) {
	expected := 40
	if r := P1(testData); r != expected {
		t.Fatalf("Expected %d, got %d", expected, r)
	}
}
