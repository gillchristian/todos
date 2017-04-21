package todos

import (
	"fmt"
	"io/ioutil"
	"log"
	"os/user"
)

// PrintTodos prints today's to-do file
func PrintTodos() {
	path := homeDir() + "/.todos/data/2017/04-20.txt"
	b, err := ioutil.ReadFile(path) // just pass the file name
	if err != nil {
		fmt.Print(err)
	}

	str := string(b) // convert content to a 'string'

	fmt.Println(str) // print the content as a 'string'
}

// homeDir retrieves the home directory for the current user.
func homeDir() string {
	usr, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return usr.HomeDir
}
