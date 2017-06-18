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
	return TodoFile{
		BasePath: sanitizePath(path),
		Content:  "TODO\n\nAccomplished",
	}
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
			return "", errors.Errorf("Cannot create directory %v", red.Sprint(dir))
		}
	}

	filePath, err := t.Path()
	if err != nil {
		return "", err
	}

	f, err := os.Create(filePath)
	if err != nil {
		return "", errors.Errorf("Cannot create file %v", red.Sprint(filePath))
	}
	defer f.Close()

	_, err = f.WriteString(t.Content)
	if err != nil {
		return "", errors.Errorf("Cannot write to file %v", red.Sprint(filePath))
	}

	return filePath, nil
}

// Parse parses t to list todos and accomplished items.
// When parsing today's TodoFile it will create it if it does not exist.
func (t *TodoFile) Parse() error {
	path, err := t.Path()

	if err != nil {
		return err
	}

	if _, err := os.Stat(path); os.IsNotExist(err) {
		if today := time.Now().Format("01-02.txt"); today == t.Name {
			if _, errC := t.Create(); errC != nil {
				return errC
			}
		} else {
			return errors.Errorf("%v does not exist", red.Sprint(path))
		}
	}

	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		return err
	}

	t.Content = strings.Replace(string(bytes), "TODO\n", "", -1)

	lists := strings.Split(t.Content, "Accomplished")

	t.Todos = parseList(lists[0])[1:]   // first item is an empty string
	t.Accomps = parseList(lists[1])[1:] // first item is an empty string

	return nil
}

// Print prints the TodoFile TODO & Accomplished lists.
func (t *TodoFile) Print() {
	// TODO: print message if no TODOs or Accomplished items are present.

	red.Println("TODO")

	printList(t.Todos)

	green.Println("\nAccomplished")

	printList(t.Accomps)
}

func printList(list []string) {
	format := listItemFormater(len(list))

	for i, item := range list {
		fmt.Printf(format, i+1, item)
	}
}

func listItemFormater(count int) string {
	s := fmt.Sprintf("%d", len(fmt.Sprintf("%d", count)))
	return "- %" + s + "d: %v\n"
}

func parseList(text string) []string {
	text = strings.Replace(text, "\n", "", -1)
	return strings.Split(text, "- ")
}
