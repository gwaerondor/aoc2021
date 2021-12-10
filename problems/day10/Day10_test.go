package main

import "testing"

var testData []string = []string{
	"[({(<(())[]>[[{[]{<()<>>",
	"[(()[<>])]({[<{<<[]>>(",
	"{([(<{}[<>[]}>{[]{[(<()>",
	"(((({<>}<{<{<>}{[]{[]{}",
	"[[<[([]))<([[{}[[()]]]",
	"[{[{({}]{}}([{[{{{}}([]",
	"{<[[]]>}<{[{[{[]{()[[[]",
	"[<(<(<(<{}))><([]([]()",
	"<{([([[(<>()){}]>(<<{{",
	"<{([{{}}[<[[[<>{}]]]>[]]",
}

func TestMiddle(t *testing.T) {
	data := []int{1, 2, 3}
	if r := middle(data); r != 2 {
		t.Fatalf("got %d, expected 2", r)
	}
}

func TestAutocompleteScoreShort(t *testing.T) {
	if r := autocompleteScore("<{([{{}}[<[[[<>{}]]]>[]]"); r != 294 {
		t.Fatalf("Wrong autocomplete score: %d, expected 294", r)
	}
}

func TestAutocompleteScore(t *testing.T) {
	if r:= autocompleteScore("[({(<(())[]>[[{[]{<()<>>"); r != 288957 {
		t.Fatalf("Wrong autocomplete score: %d", r)
	}
}

func TestP1(t *testing.T) {
	r := P1(testData)
	if P1(testData) != 26397 {
		t.Fatalf("got %d, expected 26397", r)
	}
}

func TestP2(t *testing.T) {
	r := P2(testData)
	if P2(testData) != 288957 {
		t.Fatalf("got %d, expected 288957", r)
	}
}


func TestConsume(t *testing.T) {
	_, err := consume("{([(<{}[<>[]}>{[]{[(<()>")
	if err == nil {
		t.Fatal("Supposed to be invalid but wasn't")
	}
	if err.r != '}' {
		t.Fatalf("Expected erroneous rune to be } but it was %s", string(err.r))
	}
}
