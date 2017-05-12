package todos

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/user"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/fatih/color"
)

type TodoFile struct {
	todos   []string
	accomps []string
	year    string
	name    string
	content string
}

func (t *TodoFile) Parse() error {
	if t.content == "" || !strings.Contains(t.content, "TODO") || !strings.Contains(t.content, "Accomplished") {
		return fmt.Errorf("%T does not have a valid content", t)
	}

	chunks := strings.Split(t.content, "Accomplished")

	todo := strings.TrimSpace(strings.Replace(chunks[0], "TODO", "", 1))
	accomp := strings.TrimSpace(chunks[1])

	t.todos = list(todo)
	t.accomps = list(accomp)

	return nil
}

func (t TodoFile) Path(base string) string {
	return sanitizePath(base + "/" + t.year + "/" + t.name)
}

func (t TodoFile) Print() {
	red := color.New(color.FgRed, color.Bold)
	green := color.New(color.FgGreen, color.Bold)
	if len(t.todos) > 0 {
		red.Println("TODO")

		for i, t := range t.todos {
			fmt.Printf("%2d - %v\n", i, t)
		}
	}

	if len(t.accomps) > 0 {
		green.Println("\nAccomplished")

		for i, t := range t.accomps {
			fmt.Printf("%2d - %v\n", i, t)
		}
	}

}

func (t TodoFile) Create() error {

	if err := os.Mkdir("~/.todos/"+t.year, os.ModeDir); err != nil {
		log.Fatal(err)
	}

}

func list(s string) []string {
	l := strings.Split(s, "\n")
	r := []string{}

	for _, item := range l {
		if t := strings.Replace(item, "- ", "", 1); t != "" {
			r = append(r, t)
		}
	}

	return r
}

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

func Init() error {
	now := time.Now()

	if err := os.Mkdir("~/.todos", os.ModeDir); err != nil {
		log.Fatal(err)
	}

	return TodoFile{
		year: now.Format("2006"),
		name: now.Format("01-02.txt"),
	}.Create()
}

// AddTodo adds a new to-do to today's file.
func AddTodo(basePath, todo string) error {
	path := todaysPath(basePath)

	fmt.Printf("Adding: %v - to: %v\n", todo, path)

	return nil
}

// PrintTodos prints today's to-do file
func PrintTodos(basePath string) {
	path := todaysPath(basePath)
	if !exists(path) {
		createTodayFile(basePath)
	}

	now := time.Now()

	t := TodoFile{
		content: parseTodoFile(path),
		year:    now.Format("2006"),
		name:    now.Format("01-02.txt"),
	}

	if err := t.Parse(); err != nil {
		log.Fatal(err)
	}

	t.Print()
}

func parseTodoFile(path string) string {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}

	content := string(b)
	return content
}

// createTodayFile creates today's to-do file.
// TODO section is copied from the last to-do file present.
func createTodayFile(basePath string) {

	// TODO: make it work for last year files.

	currentYear := time.Now().Format("2006")
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
	return time.Now().Format("01-02.txt")
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
	return sanitizePath(basePath + "/" + time.Now().Format("2006") + "/" + todaysFilename())
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
