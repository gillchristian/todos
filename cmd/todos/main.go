package main

import (
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
			Usage:       "todos directory `PATH`",
			Destination: &path,
		},
	}

	app.Action = func(c *cli.Context) error {
		todos.PrintTodos(path)
		return nil
	}
	_ = app.Run(os.Args)
}
