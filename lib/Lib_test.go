package lib

import (
	"testing"
)

func TestFilePathOfDay(t *testing.T) {
	if filePathOfDay(1) != "../../input/day01.txt" {
		t.Fatal("Wrong path with single digit")
	}
	if filePathOfDay(12) != "../../input/day12.txt" {
		t.Fatal("Wrong path with double digits")
	}
}

func TestSum(t *testing.T) {
	if Sum([]int{0}) != 0 || Sum([]int{1, 2, 3}) != 6 {
		t.Fatal("Sum is not working correctly")
	}
}
