package main

import (
	"testing"
)

func TestDay02Part1(t *testing.T) {
	in := false
	res := Day02Part1(in)
	exp := true
	if res != exp {
		t.Fatalf("Expected %v but got %v", exp, res)
	}
}

func TestDay02Part2(t *testing.T) {
	in := false
	res := Day02Part2(in)
	exp := true
	if res != exp {
		t.Fatalf("Expected %v but got %v", exp, res)
	}
}
