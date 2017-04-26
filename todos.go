package todos

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/user"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
)

type files []os.FileInfo

func (fs files) Len() int {
	return len(fs)
}

func (fs files) Swap(i, j int) {
	fs[i], fs[j] = fs[j], fs[i]
}

func (fs files) Less(i, j int) bool {
	return fs[i].ModTime().Before(fs[j].ModTime())
}

// PrintTodos prints today's to-do file
func PrintTodos(basePath string) {
	path := todaysPath(basePath)
	if !exists(path) {
		createTodayFile(basePath)
	}
	printFile(path)
}

// createTodayFile creates today's to-do file.
// TODO section is copied from the last to-do file present.
func createTodayFile(basePath string) {

	// TODO: make it work for last year files.

	currentYear := strconv.Itoa(time.Now().Year())
	dirPath := sanitizePath(basePath + "/" + currentYear)
	fs, err := readDir(dirPath)
	if err != nil {
		log.Fatal(err)
	}

	lastTodo := dirPath + "/" + fs[len(fs)-1].Name()
	b, err := ioutil.ReadFile(lastTodo)
	if err != nil {
		log.Fatal(err)
	}

	content := strings.SplitAfter(string(b), "Accomplished")[0]

	f, err := os.Create(todaysPath(basePath))
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	_, err = f.Write([]byte(content))
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Created today's to-do file: %s\n\n", todaysFilename())
}

// printFile prints a file content to the console.
func printFile(path string) {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(b))
}

// getTodaysFilename generates todo's filename for today.
// Format: "<year>/<month>-<day>.txt"
func todaysFilename() string {
	return time.Now().Format("2006/01-02.txt")
}

// sanitizePath makes sure a path is absolute and clean.
// It also replaces $HOME & ~ with the home directory for the current user.
func sanitizePath(path string) string {
	if !filepath.IsAbs(path) {
		home := homeDir()
		if strings.HasPrefix(path, "$HOME") {
			path = strings.Replace(path, "$HOME", home, 1)
		}
		if strings.HasPrefix(path, "~") {
			path = strings.Replace(path, "~", home, 1)
		}
	}

	return filepath.Clean(path)
}

// todaysPath generates a sanitized path to what should be today's to-do file.
func todaysPath(basePath string) string {
	return sanitizePath(basePath + "/" + todaysFilename())
}

// exists checks whether a file exists or not.
func exists(name string) bool {
	_, err := os.Stat(name)
	return !os.IsNotExist(err)
}

// homeDir retrieves the home directory path for the current user.
func homeDir() string {
	usr, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return usr.HomeDir
}

// readDir walks a directory and returns a list of all files sorted DESC by ModTime.
func readDir(path string) (files, error) {
	var todoFs files

	fs, err := ioutil.ReadDir(path)

	if err != nil {
		return files{}, err
	}

	for _, f := range fs {
		if !f.IsDir() {
			todoFs = append(todoFs, f)
		}
	}

	sort.Sort(todoFs)

	return todoFs, nil
}
