package main

import (
	"aoc/lib"
	"fmt"
	"strings"
)

func Day04Part1(in string) int {
	fields := strings.Split(strings.Trim(in, "\n"), "\n\n")
	drawings := lib.StringsToInts(strings.Split(fields[0], ","))
	boards := make([][][]*BingoNumber, 0)
	for _, board := range fields[1:] {
		boards = append(boards, NewBoard(strings.Split(board, "\n")))
	}
	for _, draw := range drawings {
		for _, board := range boards {
			Mark(board, draw)
			if Won(board) {
				return SumUnmarked(board) * draw
			}
		}
	}
	return -1
}

func Day04Part2(in string) int {
	fields := strings.Split(strings.Trim(in, "\n"), "\n\n")
	drawings := lib.StringsToInts(strings.Split(fields[0], ","))
	boards := make([][][]*BingoNumber, 0)
	for _, board := range fields[1:] {
		boards = append(boards, NewBoard(strings.Split(board, "\n")))
	}
	for _, draw := range drawings {
		for _, board := range boards {
			Mark(board, draw)
		}
		if len(boards) == 1 && Won(boards[0]) {
			return SumUnmarked(boards[0]) * draw
		}
		boards = FilterWon(boards)
	}
	return -1
}

type BingoNumber struct {
	number int
	marked bool
}

func Mark(board [][]*BingoNumber, n int) {
	for _, row := range board {
		for _, number := range row {
			if n == number.number {
				number.Mark()
			}
		}
	}
}

func Won(board [][]*BingoNumber) bool {
	return anyRowFullyMarked(board) || anyRowFullyMarked(Transpose(board))
}

func Transpose(board [][]*BingoNumber) [][]*BingoNumber {
	xl := len(board[0])
	yl := len(board)
	result := make([][]*BingoNumber, xl)
	for i := range result {
		result[i] = make([]*BingoNumber, yl)
	}
	for i := range board[0] {
		for j := range board {
			result[i][j] = board[j][i]
		}
	}
	return result
}

func NewBoard(rows []string) [][]*BingoNumber {
	matrix := lib.StringsToIntMatrix(rows, " ")
	res := make([][]*BingoNumber, len(matrix))
	for i, ns := range matrix {
		res[i] = make([]*BingoNumber, len(ns))
		for j, n := range ns {
			res[i][j] = &BingoNumber{n, false}
		}
	}
	return res
}

func Equal(a, b [][]*BingoNumber) bool {
	if len(a) != len(b) {
		return false
	}
	for i, row := range a {
		if len(a[i]) != len(b[i]) {
			return false
		}
		for j, e := range row {
			if e.number != b[i][j].number || e.marked != b[i][j].marked {
				fmt.Printf("%s != %s", e, b[i][j])
				return false
			}
		}
	}
	return true
}

func anyRowFullyMarked(board [][]*BingoNumber) bool {
	for _, row := range board {
		allMarked := true
		for _, n := range row {
			if !n.marked {
				allMarked = false
			}
		}
		if allMarked {
			return true
		}
	}
	return false
}

func SumUnmarked(board [][]*BingoNumber) int {
	sum := 0
	for _, row := range board {
		for _, bn := range row {
			if !bn.marked {
				sum += bn.number
			}
		}
	}
	return sum
}

func (b *BingoNumber) Mark() {
	b.marked = true
}

func FilterWon(boards [][][]*BingoNumber) [][][]*BingoNumber {
	res := make([][][]*BingoNumber, 0)
	for _, board := range boards {
		if !Won(board) {
			res = append(res, board)
		}
	}
	return res
}

func main() {
	in := lib.StringOfFileOfDay(4)
	p1 := Day04Part1(in)
	p2 := Day04Part2(in)
	fmt.Printf("Part 1: %v\nPart 2: %v\n", p1, p2)
}

func (b *BingoNumber) String() string {
	state := "f"
	if b.marked {
		state = "t"
	}
	return fmt.Sprintf("%d%s", b.number, state)
}

func pp(board [][]*BingoNumber) string {
	s := ""
	for _, row := range board {
		for _, n := range row {
			s = fmt.Sprintf("%s %s", s, n)
		}
		s = s + "\n"
	}
	return s
}
