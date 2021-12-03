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

type exp2test struct {
	exponent int
	expected int
}

func (test exp2test) run(t *testing.T) {
	if res := exp2(test.exponent); res != test.expected {
		t.Fatalf("Expected %d, got %d", res, test.expected)
	}
}

func TestExp2(t *testing.T) {
	tests := []exp2test{
		{0, 1},
		{1, 2},
		{2, 4},
		{3, 8},
		{4, 16},
		{5, 32},
	}
	for _, test := range tests {
		test.run(t)
	}
}

func TestTransposeBits(t *testing.T) {
	in := [][]bool{
		{true, true, true, true},
		{false, false, true, false},
	}
	exp := [][]bool{
		{true, false},
		{true, false},
		{true, true},
		{true, false},
	}
	transposed := TransposeBits(in)
	for i, row := range transposed {
		for j, _ := range row {
			if exp[i][j] != transposed[i][j] {
				t.Fatal("Transposed incorrectly")
			}
		}
	}
}

type bin2intTest struct {
	bin      string
	expected int
}

func (test bin2intTest) run(t *testing.T) {
	bits := ParseBits(test.bin)
	if res := BinaryToInt(bits); res != test.expected {
		t.Fatalf("Expected %d, got %d", res, test.expected)
	}
}

func TestBinaryToInt(t *testing.T) {
	tests := []bin2intTest{
		{"1", 1},
		{"10", 2},
		{"00", 0},
		{"101", 5},
		{"1111", 15},
		{"1010110010110000101010", 2829354},
	}
	for _, test := range tests {
		test.run(t)
	}
}
