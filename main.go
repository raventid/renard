package main

import (
	"fmt"
	"github.com/raventid/clojurium/repl"
	"os"
	"os/user"
)

func main() {
	user, err := user.Current()

	if err != nil {
		panic(err)
	}

	fmt.Printf("Hello %s! Welcome to Clojurium language interactive environment.\n", user.Username)
	fmt.Printf("You can start by reading documentation at Clojurium's web-site. https://raventid.github.io/clojurium/\n")
	repl.Start(os.Stdin, os.Stdout)
}
