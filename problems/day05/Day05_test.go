package main

import (
	"testing"
)

var testData []string = []string{
	"0,9 -> 5,9",
	"8,0 -> 0,8",
	"9,4 -> 3,4",
	"2,2 -> 2,1",
	"7,0 -> 7,4",
	"6,4 -> 2,0",
	"0,9 -> 2,9",
	"3,4 -> 1,4",
	"0,0 -> 8,8",
	"5,5 -> 8,2",
}

func TestDay05Part1(t *testing.T) {
	exp := 5
	if res := Day05Part1(testData); res != exp {
		t.Fatalf("Expected %d, got %d", exp, res)
	}
}

func TestDay05Part2(t *testing.T) {
	exp := 12
	if res := Day05Part2(testData); res != exp {
		t.Fatalf("Expected %d, got %d", exp, res)
	}
}

func TestParseHorizontalLine(t *testing.T) {
	from := coord{0, 0}
	to := coord{0, 5}
	expected := []coord{
		{0, 0}, {0, 1}, {0, 2}, {0, 3}, {0, 4}, {0, 5},
	}
	actual := parseLine(from, to)
	if !linesEqual(expected, actual) {
		t.Fatalf("Lines were not equal: %v and %v", expected, actual)
	}
}

func TestParseVerticalLine(t *testing.T) {
	from := coord{2, 3}
	to := coord{4, 3}
	expected := []coord{
		{2, 3}, {3, 3}, {4, 3},
	}
	actual := parseLine(from, to)
	if !linesEqual(expected, actual) {
		t.Fatalf("Lines were not equal: %v and %v", expected, actual)
	}
}

func TestParseDiagonalLine(t *testing.T) {
	from := coord{0, 0}
	to := coord{2, 2}
	expected := []coord{
		{0, 0}, {1, 1}, {2, 2},
	}
	actual := parseLineWithDiag(from, to)
	if !linesEqual(expected, actual) {
		t.Fatalf("Lines were not equal: %v and %v", expected, actual)
	}
}
