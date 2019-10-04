package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

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

	if d, ok := t.Date(); ok {
		fmt.Printf("TD file: %v\n\n", d)
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

func accomplishTodoAction(c *cli.Context) error {
	t := todos.New(c.GlobalString("dir"))

	_, err := t.Read()

	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nMaybe you forgot to initialize TD:\n   $ td init [dir]")

		return nil
	}

	id := c.Args().First()

	count := len(t.Todos)

	i, err := strconv.Atoi(id)

	for err != nil || i < 1 || i > count {

		fmt.Printf("Enter TODO number between 1 and %v: ", count)
		fmt.Scanln(&id)

		i, err = strconv.Atoi(id)
	}

	_ = t.Accomplish(i) // TODO: handle this error

	t.Print()

	return nil
}

func reportAction(c *cli.Context) error {
	today := todos.New(c.GlobalString("dir"))
	_, err := today.Read()
	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nMaybe you forgot to initialize TD:\n   $ td init [dir]")
		return nil
	}

	prev, err := today.FindPrev()
	if err != nil {
		fmt.Println(err.Error())
		fmt.Println("\nCould not find a TD file, maybe you forgot to initialize TD:\n   $ td init [dir]")
		return nil
	}

	d := "*Yesterday*"
	if prevDate, ok := prev.Day(); ok {
		d = prevDate
	}

	todos.PrintList(
		prev.Accomps,
		green.Sprint(d),
		"Looks like you did not work yesterday...",
	)

	todos.PrintList(
		today.Todos,
		 red.Sprint("\n*Today*"),
		"Nothing to do today, really?",
	)

	return nil
}

func main() {
	var path string

	app := cli.NewApp()

	app.Name = "td"
	app.Version = "0.0.8"
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
		{
			Name:   "done",
			Usage:  "sets a TODO as accomplished",
			Action: accomplishTodoAction,
		},
		{
			Name:   "standup",
			Usage:  "report for standup (prev day accomplishments and today's todos)",
			Action: reportAction,
		},
	}

	_ = app.Run(os.Args)
}
