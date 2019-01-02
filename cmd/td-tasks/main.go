// Package main provides tasks related to td.
package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	// "strconv"
	"strings"
	// "time"
	"github.com/urfave/cli"
)

var empty interface{}

func fileExists(f string) bool {
	_, err := os.Stat(f)
	if os.IsNotExist(err) {
		return false
	}
	return err == nil
}

func toMonthDirs(c *cli.Context) error {
	base := c.GlobalString("dir")

	yearDirs, err := ioutil.ReadDir(base)

	if err != nil {
		return err
	}

	for _, yearDir := range yearDirs {
		if yearDir.Name() == ".git" {
			continue
		}

		tds, err := ioutil.ReadDir(filepath.Join(base, yearDir.Name()))

		if err != nil {
			return err
		}

		for _, td := range tds {
			month := strings.Split(td.Name(), "-")[0]
			dir := filepath.Join(base, yearDir.Name(), month)

			if !fileExists(dir) {
				fmt.Printf("\nCreating dir: %v\n", dir)
				if err := os.Mkdir(dir, 0755); err != nil && !os.IsExist(err) {
					return err
				}
			}

			if !td.IsDir() {
				old := filepath.Join(base, yearDir.Name(), td.Name())
				new := filepath.Join(base, yearDir.Name(), strings.Replace(td.Name(), "-", "/", 1))
				fmt.Printf("%v -> %v\n", old, new)
				if err := os.Rename(old, new); err != nil && !os.IsExist(err) {
					return err
				}
			}
		}
	}

	return nil
}

func main() {
	var path string

	app := cli.NewApp()

	app.Name = "td-tasks"

	app.Version = "0.0.1"

	app.Author = "Christian Gill (gillchristiang@gmail.com)"

	app.Usage = "Support tasks for the td program"

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
			Name:   "to-month-dirs",
			Usage:  "changes the format from <year>/<month>-<day>.txt to <year>/<month>/<day>.txt",
			Action: toMonthDirs,
		},
	}

	_ = app.Run(os.Args)
}
