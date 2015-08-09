package main

import (
	"fmt"
	"os"
	"os/user"
)

func main() {
	dir, err := os.Getwd()
	u, err2 := user.Current()
	fmt.Println("My pid is:", os.Getpid(), "dir:", dir, "err:", err, "MY_VAR1="+os.Getenv("MY_VAR1"), "User:", u.Username, "err2:", err2)
}
