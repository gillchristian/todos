package todos

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/user"
	"sort"
	"strconv"
	"strings"
	"time"
)

type file struct {
	name    string
	modTime time.Time
}

type files []file

func (s files) Len() int {
	return len(s)
}

func (s files) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s files) Less(i, j int) bool {
	return s[i].modTime.Before(s[j].modTime)
}

func readDir(path string) (files, error) {
	var todoFs files

	fs, err := ioutil.ReadDir(path)

	if err != nil {
		return files{}, err
	}

	for _, f := range fs {
		if !f.IsDir() {
			todoFs = append(todoFs, file{f.Name(), f.ModTime()})
		}
	}

	sort.Sort(todoFs)

	return todoFs, nil
}

// PrintTodos prints today's to-do file
func PrintTodos() {
	path := homeDir() + "/.todos/data/" + todaysFilename()
	if !exists(path) {
		createTodayFile(path)
	}
	printFile(path)
}

func createTodayFile(path string) {
	dirPath := homeDir() + "/.todos/data/" + currentYear()
	fs, err := readDir(dirPath)
	if err != nil {
		log.Fatal(err)
	}

	lastFilePath := dirPath + "/" + fs[len(fs)-1].name
	b, err := ioutil.ReadFile(lastFilePath)
	if err != nil {
		log.Fatal(err)
	}

	content := strings.SplitAfter(string(b), "Accomplished")[0]
	content += "\n-"

	f, err := os.Create(path)
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

func currentYear() string {
	return strconv.Itoa(time.Now().Year())
}

// exists checks whether a file exists or not.
func exists(name string) bool {
	_, err := os.Stat(name)
	return !os.IsNotExist(err)
}

// homeDir retrieves the home directory for the current user.
func homeDir() string {
	usr, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return usr.HomeDir
}
