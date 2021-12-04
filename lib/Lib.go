package lib

import (
	"aoc/bitstring"
	"encoding/csv"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Reads a file as a string
func StringOfFile(path string) string {
	bytes, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return string(bytes)
}

// String of a file for a specified day
func StringOfFileOfDay(day int) string {
	return StringOfFile(filePathOfDay(day))
}

// Lines of a file for a specified day
func LinesOfFileOfDay(day int) []string {
	return LinesOfFile(filePathOfDay(day))
}

func SeparatedFileOfDay(day int, separator string) []string {
	return SeparatedFile(filePathOfDay(day), separator)
}

// Fields of a CSV file for a specified day
func CsvOfFileOfDay(day int) [][]string {
	return CsvOfFile(filePathOfDay(day))
}

// Lines of a file for a specified day, converted to integers
func IntOfLinesOfFileOfDay(day int) []int {
	lines := LinesOfFileOfDay(day)
	return StringsToInts(lines)
}

func BitmatrixOfFileOfDay(day int) bitstring.Bitmatrix {
	lines := LinesOfFileOfDay(day)
	return bitstring.NewMatrix(lines)
}

// Lines of a file for a specific day, each character converted to a digit
func DigitsOfLinesOfFileOfDay(day int) [][]int {
	lines := LinesOfFileOfDay(day)
	res := make([][]int, len(lines))
	for i, ln := range lines {
		res[i] = ParseDigits(ln)
	}
	return res
}

type SIPair struct {
	String string
	Int    int
}

func ParseSIPair(str, separator string) SIPair {
	parts := strings.Split(str, separator)
	n, err := strconv.Atoi(parts[1])
	if err != nil {
		panic(err)
	}
	return SIPair{parts[0], n}
}

func ParseDigits(str string) []int {
	res := make([]int, len(str))
	for i, e := range str {
		digit, err := strconv.Atoi(string(e))
		if err != nil {
			panic(err)
		}
		res[i] = digit
	}
	return res
}

func SIPairsOfLinesOfFileOfDay(day int, separator string) []SIPair {
	lines := LinesOfFileOfDay(day)
	return SIPairsOfLines(lines, separator)
}

func SIPairsOfLines(lines []string, separator string) []SIPair {
	res := make([]SIPair, len(lines))
	for i, ln := range lines {
		res[i] = ParseSIPair(ln, separator)
	}
	return res
}

// Fields of a CSV file for a specified day, converted to integers
func IntOfCsvOfFileOfDay(day int) [][]int {
	rows := CsvOfFileOfDay(day)
	res := make([][]int, 0)
	for _, row := range rows {
		res = append(res, StringsToInts(row))
	}
	return res
}

// Lines of a file
func LinesOfFile(path string) []string {
	return SeparatedFile(path, "\n")
}

func CsvOfFile(path string) [][]string {
	fileReader, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	csvReader := csv.NewReader(fileReader)
	records, err := csvReader.ReadAll()
	if err != nil {
		panic(err)
	}
	return records
}

// The contents of a file, separated on a specified separator string
func SeparatedFile(path, separator string) []string {
	str := strings.Trim(StringOfFile(path), "\n")
	return strings.Split(str, separator)
}

// Sum of integers in a list
func Sum(numbers []int) int {
	sum := 0
	for _, n := range numbers {
		sum += n
	}
	return sum
}

func StringsToIntMatrix(matrix []string, separator string) [][]int {
	res := make([][]int, len(matrix))
	for i, ln := range matrix {
		res[i] = StringToInts(ln, separator)
	}
	return res
}

func StringToInts(ns, separator string) []int {
	ns = regexp.MustCompile("\\s+").ReplaceAllLiteralString(ns, " ")
	ns = strings.Trim(ns, " \n\t")
	split := strings.Split(ns, separator)
	return StringsToInts(split)
}

// Converts a list of numeric strings to a list of integers
func StringsToInts(ns []string) []int {
	res := make([]int, len(ns))
	for i, n := range ns {
		r, err := strconv.Atoi(n)
		if err != nil {
			panic(err)
		}
		res[i] = r
	}
	return res
}

func filePathOfDay(day int) string {
	return fmt.Sprintf("../../input/day%02d.txt", day)
}
