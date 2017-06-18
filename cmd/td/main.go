package main

import (
	"fmt"
	"os"

	"github.com/fatih/color"
	"github.com/gillchristian/todos"
	"github.com/urfave/cli"
)

var version = "0.0.2"

var red = color.New(color.FgRed, color.Bold)
var green = color.New(color.FgGreen, color.Bold)

// TODO: create today's file if not present, maybe todo.TodoFile.Parse could do that
func list(c *cli.Context) error {
	t := todos.New(c.GlobalString("dir"))

	err := t.Parse()

	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nMaybe you forgot to initialize TD:\n   td init [dir]")

		return nil
	}

	t.Print()

	return nil
}

func main() {
	var path string

	app := cli.NewApp()

	app.Name = "td"

	app.Version = version

	app.Author = "Christian Gill (gillchristiang@gmail.com)"

	app.Usage = "A to-do's app written in Go. Inspired on https://goo.gl/j1dQ4M"

	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:        "dir, d",
			Value:       "~/.todos",
			Usage:       "td directory `PATH`.",
			Destination: &path,
		},
	}

	app.Commands = []cli.Command{
		{
			Name:  "init",
			Usage: "initializes the TD's directory and creates the first to-do file",
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
		{
			Name:   "list",
			Usage:  "lists today's TODOs and Accomplished items",
			Action: list,
		},
	}

	app.Action = list

	_ = app.Run(os.Args)
}
