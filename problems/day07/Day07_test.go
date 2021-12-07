package main

import (
	"testing"
)

var testData []int = []int{16, 1, 2, 0, 4, 2, 7, 1, 2, 14}

func TestDay07Part1(t *testing.T) {
	exp := 37
	actual := Day07Part1(testData)
	if exp != actual {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}

func TestDay07Part2(t *testing.T) {
	exp := 168
	actual := Day07Part2(testData)
	if exp != actual {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}
