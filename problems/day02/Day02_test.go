package main

import (
	"aoc/lib"
	"testing"
)

var testData []string = []string{
	"forward 5",
	"down 5",
	"forward 8",
	"up 3",
	"down 8",
	"forward 2",
}

var parsed []lib.SIPair = lib.SIPairsOfLines(testData, " ")

func TestDay02Part1(t *testing.T) {
	res := Day02Part1(parsed)
	exp := 150
	if res != exp {
		t.Fatalf("Expected %v but got %v", exp, res)
	}
}

func TestDay02Part2(t *testing.T) {
	res := Day02Part2(parsed)
	exp := 900
	if res != exp {
		t.Fatalf("Expected %v but got %v", exp, res)
	}
}
