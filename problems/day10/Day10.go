package main

import (
	"fmt"
	"aoc/lib"
	"strings"
	"sort"
)

type ParenthesisError struct {
	r rune
}

func (pe ParenthesisError) Error() string {
	return fmt.Sprintf("unexpected closing parenthesis %s", string(pe.r))
}

func main() {
	input := lib.LinesOfFileOfDay(10)
	p1 := P1(input)
	p2 := P2(input)
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

func P1(input []string) int {
	score := 0
	for _, ln := range input {
		_, err := consume(ln)
		if err != nil {
			score += corruptionScores[err.r]
		}
	}
	return score
}

func P2(input []string) int {
	incomplete := make([]string, 0)
	for _, ln := range input {
		if _, err := consume(ln); err == nil {
			incomplete = append(incomplete, ln)
		}
	}
	scores := make([]int, len(incomplete))
	for i, ln := range incomplete {
		scores[i] = autocompleteScore(ln)
	}
	sort.Ints(scores)
	return middle(scores)
}

func isOpener(c rune) bool {
	return strings.ContainsRune("<{([", c)
}

func canClose(r rune, s string) bool {
	if s == "" {
		return false
	}
	mate, _ := closers[last(s)]
	return r == mate
}

var corruptionScores map[rune]int = map[rune]int{
	')': 3,
	']': 57,
	'}': 1197,
	'>': 25137,
}

var autocompleteScores map[rune]int = map[rune]int{
	')': 1,
	']': 2,
	'}': 3,
	'>': 4,
}

var closers map[rune]rune = map[rune]rune{
	'(': ')',
	'[': ']',
	'{': '}',
	'<': '>',
}

func autocompleteScore(s string) int {
	s, _ = consume(s)
	score := 0
	for len(s) > 0 {
		closer := findNextCloser(s)
		score = (5 * score) + autocompleteScores[closer]
		s, _ = consume(s + string(closer))
	}
	return score
}

func consume(s string) (string, *ParenthesisError) {
	curr := ""
	for _, r := range s {
		if isOpener(r) {
			curr = curr + string(r)
		} else {
			if canClose(r, curr) {
				curr = curr[:len(curr) - 1]
			} else {
				return "", &ParenthesisError{r}
			}
		}
	}
	return curr, nil
}

func middle(scores []int) int {
	return scores[len(scores) / 2]
}

func findNextCloser(s string) rune {
	closer, _ := closers[last(s)]
	return closer
}

func last(s string) rune {
	return []rune(s[len(s)-1:len(s)])[0]
}
