package main

import (
	"testing"
)

var testData []int = []int{
	199,
	200,
	208,
	210,
	200,
	207,
	240,
	269,
	260,
	263,
}

func TestDay01Part1(t *testing.T) {
	if n := Day01Part1(testData); n != 7 {
		t.Fatalf("Should have 7 increases, had %d", n)
	}
}

func TestDay01Part2(t *testing.T) {
	if n := Day01Part2(testData); n != 5 {
		t.Fatalf("Should have 5 increases, had %d", n)
	}
}
