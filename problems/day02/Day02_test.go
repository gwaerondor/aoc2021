package main

import (
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

func TestDay02Part1(t *testing.T) {
	res := Day02Part1(testData)
	exp := 150
	if res != exp {
		t.Fatalf("Expected %v but got %v", exp, res)
	}
}

func TestDay02Part2(t *testing.T) {
	res := Day02Part2(testData)
	exp := 900
	if res != exp {
		t.Fatalf("Expected %v but got %v", exp, res)
	}
}
