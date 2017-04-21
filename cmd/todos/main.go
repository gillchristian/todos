package main

import (
	"os"

	"github.com/gillchristian/todos"
	"github.com/urfave/cli"
)

var version = "master"

func main() {
	app := cli.NewApp()
	app.Name = "todos"
	app.Version = version
	app.Author = "Christian Gill (gillchristiang@gmail.com)"
	app.Usage = "This is an todos app written in Go"
	app.Action = func(c *cli.Context) error {
		todos.PrintTodos()
		return nil
	}
	_ = app.Run(os.Args)
}
