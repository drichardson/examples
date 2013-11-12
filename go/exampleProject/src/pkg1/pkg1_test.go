package pkg1

import "fmt"
import "testing"
import "log"

func TestIt(t *testing.T) {
	fmt.Println("hihi")
	log.Fatalf("Howdy log message\n")
	t.Fail()
}
