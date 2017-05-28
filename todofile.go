package todos

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/pkg/errors"
)

// TodoFile stores the data of a to-do file and has the relevant methods to interact with it.
type TodoFile struct {
	Todos    []string
	Accomps  []string
	Year     string
	Name     string
	Content  string
	BasePath string
}

// New creates a new TodoFile with path as the TD's base path.
func New(path string) TodoFile {
	return TodoFile{BasePath: path}
}

// Dir returns the TodoFile directory path ("<base-path>/<year>").
// If t.year is missing, it will assigend first to the current year.
func (t *TodoFile) Dir() (string, error) {
	if t.BasePath == "" {
		return "", errors.New("basePath missing")
	}
	if t.Year == "" {
		t.Year = time.Now().Format("2006")
	}
	return sanitizePath(t.BasePath + "/" + t.Year), nil
}

// Path returns the TodoFile path ("<base-path>/<year>/<month>-<day>.txt").
// If t.year and t.name are missing, they will assigend first to the current date.
func (t *TodoFile) Path() (string, error) {
	dir, err := t.Dir()
	if err != nil {
		return "", err
	}
	if t.Name == "" {
		t.Name = time.Now().Format("01-02.txt")
	}
	return sanitizePath(dir + "/" + t.Name), nil
}

// Create creates a todo file at "<t.basePath>/<t.year>/<t.name>".
// Asumes TD's base path has already been created.
func (t TodoFile) Create() (string, error) {
	dir, err := t.Dir()

	if err != nil {
		return "", err
	}

	if _, err := os.Stat(dir); os.IsNotExist(err) {
		if err := os.Mkdir(dir, 0755); err != nil {
			return "", errors.Wrap(err, "Cannot create directory:")
		}
	}

	filePath, err := t.Path()
	if err != nil {
		return "", err
	}

	f, err := os.Create(filePath)
	if err != nil {
		return "", errors.Wrap(err, "Cannot create file:")
	}
	defer f.Close()

	_, err = f.WriteString("TODO\n\nAccomplished")
	if err != nil {
		return "", errors.Wrap(err, "Cannot write to file:")
	}

	return filePath, nil
}

// Parse parses t to list todos and accomplished items.
func (t *TodoFile) Parse() error {
	path, err := t.Path()

	if err != nil {
		return err
	}

	// TODO: create file if not existent (?)
	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		return err
	}

	t.Content = strings.Replace(string(bytes), "TODO\n", "", -1)

	sides := strings.Split(t.Content, "Accomplished")

	t.Todos = parseList(sides[0])[1:]
	t.Accomps = parseList(sides[1])[1:]

	return nil
}

// Print prints the TodoFile TODO & Accomplished lists.
func (t *TodoFile) Print() {
	// TODO: print message if no TODOs or Accomplished items are present.
	format := formatStr(len(t.Todos), len(t.Accomps))

	red.Println("TODO")

	for i, item := range t.Todos {
		fmt.Printf(format, i+1, item)
	}

	green.Println("\nAccomplished")

	for i, item := range t.Accomps {
		fmt.Printf(format, i+1, item)
	}
}

func formatStr(a, b int) string {
	// TODO: improve this func, maybe make it more generic
	pad := a
	if a < b {
		pad = b
	}

	s := fmt.Sprintf("%d", len(fmt.Sprintf("%d", pad)))

	return "- %" + s + "d: %v\n"
}

func parseList(text string) []string {
	text = strings.Replace(text, "\n", "", -1)
	return strings.Split(text, "- ")
}
