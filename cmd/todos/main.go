package main

import (
	"fmt"
	"os"

	"github.com/gillchristian/todos"
	"github.com/urfave/cli"
)

var version = "master"

func main() {
	var path string

	app := cli.NewApp()

	app.Name = "todos"

	app.Version = version

	app.Author = "Christian Gill (gillchristiang@gmail.com)"

	app.Usage = "This is a TO-DO's app written in Go"

	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:        "dir, d",
			Value:       "$HOME/.todos",
			Usage:       "todos directory `PATH`.",
			Destination: &path,
		},
	}

	app.Commands = []cli.Command{
		{
			Name:    "add",
			Aliases: []string{"a"},
			Usage:   "add a new to-do to today's list",
			Action: func(c *cli.Context) error {
				if c.NArg() > 0 {
					todos.AddTodo(path, c.Args()[0])
				} else {
					fmt.Println("Can't add something if it's nothing ¯\\_(ツ)_/¯")
				}

				return nil
			},
		},
		{
			Name:  "init",
			Usage: "initializes the TODO's directory and creates the first to-do file",
			Action: func(c *cli.Context) error {
				return todos.Init()
			},
		},
	}

	app.Action = func(c *cli.Context) error {
		todos.PrintTodos(path)
		return nil
	}
	_ = app.Run(os.Args)
}
