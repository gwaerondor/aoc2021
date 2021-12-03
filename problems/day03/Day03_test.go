package main

import (
	"aoc/lib"
	"testing"
)

var testData []string = []string{
	"00100",
	"11110",
	"10110",
	"10111",
	"10101",
	"01111",
	"00111",
	"11100",
	"10000",
	"11001",
	"00010",
	"01010",
}

var parsed [][]bool = parse(testData)

func TestDay03Part1(t *testing.T) {
	exp := 198
	if res := Day03Part1(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestDay03Part2(t *testing.T) {
	exp := 230
	if res := Day03Part2(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestGamma(t *testing.T) {
	exp := 22
	if res := Gamma(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestEpsilon(t *testing.T) {
	exp := 9
	if res := Epsilon(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestOxygen(t *testing.T) {
	exp := 23
	if res := Oxygen(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestCO2Scrubber(t *testing.T) {
	exp := 10
	if res := CO2Scrubber(parsed); res != exp {
		t.Fatalf("Expected %v, got %v", exp, res)
	}
}

func TestLeastCommon(t *testing.T) {
	if leastCommon(lib.ParseBits("11"), false) != false {
		t.Fatalf("11 bias 0 should be 0")
	}
	if leastCommon(lib.ParseBits("10"), false) != false {
		t.Fatalf("10 bias 0 should be 0")
	}
	if leastCommon(lib.ParseBits("10"), true) != true {
		t.Fatalf("10 bias 1 should be 1")
	}
	if leastCommon(lib.ParseBits("01"), false) != false {
		t.Fatalf("01 bias 0 should be 0")
	}
	if leastCommon(lib.ParseBits("00"), false) != true {
		t.Fatalf("00 bias 0 should be 1")
	}
	if leastCommon(lib.ParseBits("1010101010101010101010101"), false) != false {
		t.Fatal("long binary failed")
	}
	if leastCommon(lib.ParseBits("1010101010101010101010101"), true) != false {
		t.Fatal("second long binary failed")
	}
}

func TestMostCommon(t *testing.T) {
	if mostCommon(lib.ParseBits("11"), false) != true {
		t.Fatalf("11 bias 0 should be 1")
	}
	if mostCommon(lib.ParseBits("10"), false) != false {
		t.Fatalf("10 bias 0 should be 0")
	}
	if mostCommon(lib.ParseBits("10"), true) != true {
		t.Fatalf("10 bias 1 should be 1")
	}
	if mostCommon(lib.ParseBits("01"), false) != false {
		t.Fatalf("01 bias 0 should be 0")
	}
	if mostCommon(lib.ParseBits("00"), false) != false {
		t.Fatalf("00 bias 0 should be 0")
	}
	if mostCommon(lib.ParseBits("1010101010101010101010101"), false) != true {
		t.Fatal("long binary failed")
	}
	if mostCommon(lib.ParseBits("1010101010101010101010101"), true) != true {
		t.Fatal("second long binary failed")
	}

}

type filterOnBitTest struct {
	binaries []string
	pos      int
	on       bool
	expected []string
}

func (test filterOnBitTest) run(t *testing.T) {
	input := toBinMatrix(test.binaries)
	exp := toBinMatrix(test.expected)
	actual := filterOnBit(test.on, test.pos, input)
	if !boolMatrixEq(exp, actual) {
		t.Fatalf("Expected %v, got %v", exp, actual)
	}
}

func TestFilterOnBit(t *testing.T) {
	tests := []filterOnBitTest{
		{
			[]string{"011", "111"},
			0,
			false,
			[]string{"011"},
		},
		{
			[]string{"000", "110"},
			2,
			true,
			[]string{},
		},
		{
			[]string{"10101", "10001"},
			3,
			false,
			[]string{"10101", "10001"},
		},
		{
			[]string{"10110", "10111", "10101", "10000"},
			2,
			true,
			[]string{"10110", "10111", "10101"},
		},
		{
			[]string{"10110", "10111"},
			4,
			true,
			[]string{"10111"},
		},
		{
			[]string{"10110", "10111"},
			4,
			false,
			[]string{"10110"},
		},
	}
	for _, test := range tests {
		test.run(t)
	}
}

func toBinMatrix(bins []string) [][]bool {
	res := make([][]bool, 0)
	for _, s := range bins {
		res = append(res, lib.ParseBits(s))
	}
	return res
}

func boolMatrixEq(a, b [][]bool) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		for j := range b {
			if len(a[i]) != len(b[i]) {
				return false
			}
			if a[i][j] != b[i][j] {
				return false
			}
		}
	}
	return true
}

func TestActualSolution(t *testing.T) {
	in := parse(lib.LinesOfFileOfDay(3))
	t.Run("Part 1", func(t *testing.T) {
		exp := 1458194
		in := parse(lib.LinesOfFileOfDay(3))
		if res := Day03Part1(in); res != exp {
			t.Fatalf("Expected %d, got %d", exp, res)
		}
	})
	t.Run("Part 2", func(t *testing.T) {
		exp := 2829354
		if res := Day03Part2(in); res != exp {
			t.Fatalf("Expected %d, got %d", exp, res)
		}
	})
}
