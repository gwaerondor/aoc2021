package main

import (
	"testing"
)

var testData string = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n" +
	"\n" +
	"22 13 17 11  0\n" +
	" 8  2 23  4 24\n" +
	"21  9 14 16  7\n" +
	" 6 10  3 18  5\n" +
	" 1 12 20 15 19\n" +
	"\n" +
	" 3 15  0  2 22\n" +
	" 9 18 13 17  5\n" +
	"19  8  7 25 23\n" +
	"20 11 10 24  4\n" +
	"14 21 16 12  6\n" +
	"\n" +
	"14 21 17 24  4\n" +
	"10 16 15  9 19\n" +
	"18  8 23 26 20\n" +
	"22 11 13  6  5\n" +
	" 2  0 12  3  7\n"

func TestNewBoard(t *testing.T) {
	in := []string{
		"22 13 17 11  0",
		" 8  2 23 4  24",
		"21  9 14 16  7",
		" 6 10  3 18  5",
		" 1 12 20 15 19",
	}
	expected := [][]*BingoNumber{
		{{22, false}, {13, false}, {17, false}, {11, false}, {0, false}},
		{{8, false}, {2, false}, {23, false}, {4, false}, {24, false}},
		{{21, false}, {9, false}, {14, false}, {16, false}, {7, false}},
		{{6, false}, {10, false}, {3, false}, {18, false}, {5, false}},
		{{1, false}, {12, false}, {20, false}, {15, false}, {19, false}},
	}
	actual := NewBoard(in)
	if !Equal(expected, actual) {
		t.Fatalf("NewBoard is wrong:\nEXP:\n%s\n\nACTUAL:\n%s\n", pp(expected), pp(actual))
	}
}

func TestWonHorizontally(t *testing.T) {
	board := [][]*BingoNumber{
		{{22, true}, {13, true}, {17, true}},
		{{8, false}, {2, false}, {23, false}},
		{{21, false}, {9, false}, {14, false}},
	}
	if !Won(board) {
		t.Fatalf("Board should have been a win but wasn't:\n%s", board)
	}
}

func TestWonVertically(t *testing.T) {
	board := [][]*BingoNumber{
		{{22, false}, {13, true}, {17, true}},
		{{8, false}, {2, true}, {23, false}},
		{{21, false}, {9, true}, {14, false}},
	}
	if !Won(board) {
		t.Fatalf("Board should have been a win but wasn't:\n%s", board)
	}
}

func TestDay04Part1(t *testing.T) {
	exp := 4512
	if res := Day04Part1(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestDay04Part2(t *testing.T) {
	exp := 1924
	if res := Day04Part2(testData); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}
