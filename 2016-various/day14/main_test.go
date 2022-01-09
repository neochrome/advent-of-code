package main

import (
	"testing"
)

func Test_first_3(t *testing.T) {
	verify := func(str string, expected byte) {
		actual := first_3(str)
		if actual != expected {
			t.Error("For", str, "expected", expected, "got", actual)
		}
	}
	verify("", 0)
	verify("abcdefg", 0)
	verify("aaadefg", 'a')
	verify("abbbefg", 'b')
	verify("abcdddg", 'd')
	verify("abcdeee", 'e')
	verify("aaabbb", 'a')
	verify("bbbaaa", 'b')
}

func Test_hash(t *testing.T) {
	actual := hash("abc18")
	expected := "0034e0923cc38887a57bd7b1d4f953df"
	if actual != expected {
		t.Error("For", "hash", "expected", expected, "got", actual)
	}
}

func Test_example(t *testing.T) {
	ch := first_3(hash("abc39"))
	if !has_5_of(hash("abc816"), ch) {
		t.Fail()
	}
}

func Test_hash_rounds(t *testing.T) {
	actual := hash_rounds(2016)("abc0")
	expected := "a107ff634856bb300138cac6568c0f24"
	if actual != expected {
		t.Error("For", "hash", "expected", expected, "got", actual)
	}
}
