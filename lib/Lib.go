package lib

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"encoding/csv"
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

// Fields of a CSV file for a specified day
func CsvOfFileOfDay(day int) [][]string {
	return CsvOfFile(filePathOfDay(day))
}

// Lines of a file for a specified day, converted to integers
func IntOfLinesOfFileOfDay(day int) []int {
	lines := LinesOfFileOfDay(day)
	return StringsToInts(lines)
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

// Converts a list of numeric strings to a list of integers
func StringsToInts(ns []string) []int {
	res := make([]int, len(ns))
	for i, n := range ns {
		r, err := strconv.Atoi(n)
		if err == nil {
			res[i] = r
		}
	}
	return res
}

func filePathOfDay(day int) string {
	return fmt.Sprintf("../../input/day%02d.txt", day)
}
