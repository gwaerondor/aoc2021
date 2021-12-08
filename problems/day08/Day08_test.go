package main

import (
	"testing"
)

var testData []string = []string{
	"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
	"edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
	"fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
	"fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
	"aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
	"fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
	"dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
	"bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
	"egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
	"gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
}

func TestDay08Part1(t *testing.T) {
	exp := 26
	actual := Day08Part1(testData)
	if exp != actual {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}

func TestSolve(t *testing.T) {
	//   1111     dddd
	//  2    3   e    a
	//  2    3   e    a
	//   4444     ffff
	//  5    6   g    b
	//  5    6   g    b
	//   7777     cccc
	input := "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
	expected := map[int]rune{1: 'd', 2: 'e', 3: 'a', 4: 'f', 5: 'g', 6: 'b', 7: 'c'}
	actual := solve(input)
	if !mapEq(expected, actual) {
		t.Fatalf("Expected %d, got %d", expected, actual)
	}
}

func TestDay08Part2(t *testing.T) {
	exp := 61229
	actual := Day08Part1(testData)
	if exp != actual {
		t.Fatalf("Expected %d, got %d", exp, actual)
	}
}

func mapEq(a, b map[int]rune) bool {
	if len(a) != len(b) {
		return false
	}
	for ak, av := range a {
		if bv, ok := b[ak]; !ok || bv != av {
			return false
		}
	}
	return true
}
