package bitstring

import (
	"fmt"
)

type Bit bool
type Bitstring []Bit
type Bitmatrix []Bitstring

const (
	Zero Bit = false
	One  Bit = true
)

func New(init string) Bitstring {
	res := make(Bitstring, len(init))
	for i, e := range []rune(init) {
		if e == '0' {
			res[i] = Zero
		} else if e == '1' {
			res[i] = One
		} else {
			panic(fmt.Errorf("%s is not a valid bit", string(e)))
		}
	}
	return res
}

func NewMatrix(init []string) Bitmatrix {
	res := make(Bitmatrix, len(init))
	for i, ln := range init {
		res[i] = New(ln)
	}
	return res
}

func (b Bitstring) ToInt() int {
	sum := 0
	pow := len(b) - 1
	for _, bit := range b {
		if bit {
			sum += exp2(pow)
		}
		pow--
	}
	return sum
}

func (b Bit) Flip() Bit {
	return !b
}

func (b Bitstring) Flip() Bitstring {
	res := make(Bitstring, len(b))
	for i, e := range b {
		res[i] = e.Flip()
	}
	return res
}

func (b Bitstring) MostCommon(bias Bit) Bit {
	zeros := 0
	ones := 0
	for _, bit := range b {
		if bit {
			ones++
		} else {
			zeros++
		}
	}
	if zeros == ones {
		return bias
	}
	return ones > zeros
}

func (b Bitstring) LeastCommon(bias Bit) Bit {
	return !b.MostCommon(!bias)
}

func (b Bit) String() string {
	if b {
		return "1"
	}
	return "0"
}

func (b Bitstring) String() string {
	r := "["
	for _, b := range b {
		r = r + b.String()
	}
	r = r + "]"
	return r
}

func exp2(x int) int {
	if x == 0 {
		return 1
	}
	prod := 1
	for x > 0 {
		prod = prod * 2
		x--
	}
	return prod
}

func (bm Bitmatrix) Transpose() Bitmatrix {
	xl := len(bm[0])
	yl := len(bm)
	result := make(Bitmatrix, xl)
	for i := range result {
		result[i] = make(Bitstring, yl)
	}
	for i := range bm[0] {
		for j := range bm {
			result[i][j] = bm[j][i]
		}
	}
	return result
}

func ToBit(n int) Bit {
	if n == 0 {
		return Zero
	} else if n == 1 {
		return One
	}
	panic(fmt.Errorf("%d is not a valid bit", n))
}

func (b Bitstring) Equals(other Bitstring) bool {
	if len(b) != len(other) {
		return false
	}
	for i := range b {
		if b[i] != other[i] {
			return false
		}
	}
	return true
}

func (b Bitmatrix) Equals(other Bitmatrix) bool {
	if len(b) != len(other) {
		return false
	}
	for i := range b {
		if !b[i].Equals(other[i]) {
			return false
		}
	}
	return true
}

func (b Bitmatrix) AddRow(row Bitstring) Bitmatrix {
	return append(b, row)
}

func (b Bitmatrix) FilterOnBit(keep Bit, pos int) Bitmatrix {
	res := make(Bitmatrix, 0)
	for _, bin := range b {
		if bin[pos] == keep {
			res = append(res, bin)
		}
	}
	return res
}
