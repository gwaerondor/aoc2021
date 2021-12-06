package main

import (
	"testing"
)

var testData []int = []int{3,4,3,1,2}

func TestDay06Part1(t *testing.T) {
	exp := 5934
	actual := Day06Part1(testData)
	if exp != actual {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}

func TestDay06Part2(t *testing.T) {
	exp := 26984457539
	actual := Day06Part2(testData)
	if exp != actual {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}

func TestParseInitState(t *testing.T) {
	exp := map[int]int{0: 0, 1: 1, 2: 1, 3: 2, 4: 1, 5: 0, 6: 0, 7: 0, 8: 0}
	actual := parseInitState(testData)
	if !mapEq(exp, actual) {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}

func mapEq(a, b map[int]int) bool {
	for key, value := range a {
		if bVal, ok := b[key]; ok {
			if bVal != value {
				return false
			}
		} else {
			return false
		}
	}
	return true
}
