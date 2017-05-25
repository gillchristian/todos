package main

import (
	"fmt"
	"os"

	"github.com/fatih/color"
	"github.com/gillchristian/todos"
	"github.com/urfave/cli"
)

var version = "0.0.1"

var red *color.Color = color.New(color.FgRed, color.Bold)
var green *color.Color = color.New(color.FgGreen, color.Bold)

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
			Value:       "~/.todos",
			Usage:       "todos directory `PATH`.",
			Destination: &path,
		},
	}

	app.Commands = []cli.Command{
		{
			Name:  "init",
			Usage: "initializes the TODO's directory and creates the first to-do file",
			Action: func(c *cli.Context) error {
				var dir string

				if c.NArg() > 0 {
					dir = c.Args()[0]
					fmt.Printf("Initializing on: %v.\n", red.Sprint(dir))
				} else {
					dir = c.GlobalString("dir")
					fmt.Printf("Initializing on default directory: %v.\n", red.Sprint(dir))
				}

				path, err := todos.Init(dir)
				if err != nil {
					fmt.Println(err)
					os.Exit(0)
				}

				fmt.Printf("Created today's to-do file: %s\n", green.Sprint(path))
				return nil
			},
		},
	}

	app.Action = func(c *cli.Context) error {
		return nil
	}
	_ = app.Run(os.Args)
}
