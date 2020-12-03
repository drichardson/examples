package pkg2

import "testing"

func TestOne(t *testing.T) {
	if !Testing() {
		t.Fail()
	}
}

func TestTwo(t *testing.T) {
	if !Testing() {
		t.Fail()
	}
}
