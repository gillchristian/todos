package main

import (
	"fmt"
	"os"

	"github.com/caarlos0/spin"
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
		spin := spin.New("\033[36m %s Working...\033[m")
		spin.Start()
		err := todos.Foo()
		spin.Stop()
		if err != nil {
			return err
		}
		fmt.Println("Done!")
		return nil
	}
	_ = app.Run(os.Args)
}
