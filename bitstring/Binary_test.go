package bitstring

import (
	"fmt"
	"testing"
)

func TestLeastCommon(t *testing.T) {
	if New("11").LeastCommon(Zero) != Zero {
		t.Fatalf("11 bias 0 should be 0")
	}
	if New("10").LeastCommon(Zero) != Zero {
		t.Fatalf("10 bias 0 should be 0")
	}
	if New("10").LeastCommon(One) != One {
		t.Fatalf("10 bias 1 should be 1")
	}
	if New("01").LeastCommon(Zero) != Zero {
		t.Fatalf("01 bias 0 should be 0")
	}
	if New("00").LeastCommon(Zero) != One {
		t.Fatalf("00 bias 0 should be 1")
	}
	if New("1010101010101010101010101").LeastCommon(Zero) != Zero {
		t.Fatal("long binary failed")
	}
	if New("1010101010101010101010101").LeastCommon(One) != Zero {
		t.Fatal("second long binary failed")
	}
}

func TestMostCommon(t *testing.T) {
	if New("11").MostCommon(Zero) != One {
		t.Fatalf("11 bias 0 should be 1")
	}
	if New("10").MostCommon(Zero) != Zero {
		t.Fatalf("10 bias 0 should be 0")
	}
	if New("10").MostCommon(One) != One {
		t.Fatalf("10 bias 1 should be 1")
	}
	if New("01").MostCommon(Zero) != Zero {
		t.Fatalf("01 bias 0 should be 0")
	}
	if New("00").MostCommon(Zero) != Zero {
		t.Fatalf("00 bias 0 should be 0")
	}
	if New("1010101010101010101010101").MostCommon(Zero) != One {
		t.Fatal("long binary failed")
	}
	if New("1010101010101010101010101").MostCommon(One) != One {
		t.Fatal("second long binary failed")
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

type bin2intTest struct {
	bin      string
	expected int
}

func (test bin2intTest) run(t *testing.T) {
	bits := New(test.bin)
	if res := bits.ToInt(); res != test.expected {
		t.Fatalf("Expected %d, got %d", res, test.expected)
	}
}

func TestToInt(t *testing.T) {
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

func TestTransposeBits(t *testing.T) {
	in := Bitmatrix{
		{true, true, true, true},
		{false, false, true, false},
	}
	exp := Bitmatrix{
		{true, false},
		{true, false},
		{true, true},
		{true, false},
	}
	transposed := in.Transpose()
	for i, row := range transposed {
		for j, _ := range row {
			if exp[i][j] != transposed[i][j] {
				t.Fatal("Transposed incorrectly")
			}
		}
	}
}

type filterOnBitTest struct {
	binaries []string
	pos      int
	on       Bit
	expected []string
}

func (test filterOnBitTest) run(t *testing.T) {
	input := toBinMatrix(test.binaries)
	exp := toBinMatrix(test.expected)
	actual := input.FilterOnBit(test.on, test.pos)
	if !exp.Equals(actual) {
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

func TestBitString(t *testing.T) {
	if Zero.String() != "0" || One.String() != "1" {
		t.Fatal("String() of Bit is wrong")
	}
}

func TestBitstringString(t *testing.T) {
	in := "10101"
	if New(in).String() != fmt.Sprintf("[%s]", in) {
		t.Fatalf("Bitstring to string didn't work")
	}
}

func TestBitFlip(t *testing.T) {
	if Zero.Flip() != One || One.Flip() != Zero {
		t.Fatal("Bit.Flip() isn't working")
	}
}

func TestBitstringFlip(t *testing.T) {
	in := New("11001010")
	expected := New("00110101")
	actual := in.Flip()
	if !expected.Equals(actual) {
		t.Fatal("Bitstring.Flip() is not working")
	}
}

func toBinMatrix(bins []string) Bitmatrix {
	res := make(Bitmatrix, 0)
	for _, s := range bins {
		res = append(res, New(s))
	}
	return res
}
