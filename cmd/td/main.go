package main

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/gillchristian/todos"
	"github.com/urfave/cli"
)

var red = color.New(color.FgRed, color.Bold)
var green = color.New(color.FgGreen, color.Bold)

func initAction(c *cli.Context) error {
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
}

func listAction(c *cli.Context) error {
	t := todos.New(c.GlobalString("dir"))

	_, err := t.Read()

	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nMaybe you forgot to initialize TD:\n   $ td init [dir]")

		return nil
	}

	t.Print()

	return nil
}

func lastAction(c *cli.Context) error {
	t := todos.New(c.GlobalString("dir"))

	t, err := t.FindPrev()

	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nCould not find a TD file, maybe you forgot to initialize TD:\n   $ td init [dir]")

		return nil
	}

	if path, err := t.Path(); err == nil {
		if date, err := time.Parse(t.BasePath+"/2006/01-02.txt", path); err == nil {
			fmt.Printf("TD file: %v\n\n", date.Format("2006/01/02"))
		}
	}

	t.Print()

	return nil
}

func addTodoAction(c *cli.Context) error {
	t := todos.New(c.GlobalString("dir"))

	_, err := t.Read()

	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nMaybe you forgot to initialize TD:\n   $ td init [dir]")

		return nil
	}

	msg := strings.Join(c.Args(), " ")

	for msg == "" {
		fmt.Print("Enter TODO text: ")
		fmt.Scanln(&msg)
	}

	_ = t.Add(msg) // TODO: handle this error

	t.Print()

	return nil
}

func main() {
	var path string

	app := cli.NewApp()

	app.Name = "td"

	app.Version = "0.0.5"

	app.Author = "Christian Gill (gillchristiang@gmail.com)"

	app.Usage = "A to-do's app written in Go. Inspired on https://goo.gl/j1dQ4M"

	app.Action = listAction

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
			Name:   "init",
			Usage:  "initializes the TD's directory and creates the first to-do file",
			Action: initAction,
		},
		{
			Name:   "list",
			Usage:  "lists today's TODOs and Accomplished items",
			Action: listAction,
		},
		{
			Name:   "last",
			Usage:  "lists last day TODOs and Accomplished items",
			Action: lastAction,
		},
		{
			Name:   "add",
			Usage:  "adds a TODO to today's list",
			Action: addTodoAction,
		},
	}

	_ = app.Run(os.Args)
}
