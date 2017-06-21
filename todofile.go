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

// Read parses t to list todos and accomplished items.
// When parsing today's TodoFile it will create it if it does not exist.
func (t *TodoFile) Read() (int, error) {
	path, err := t.Path()

	if err != nil {
		return 0, err
	}

	if _, err := os.Stat(path); os.IsNotExist(err) {
		// TODO:  Read should not be responsible for creating the file nor it
		// should be responsible for adding the content from the previous file
		if today := time.Now().Format("01-02.txt"); today == t.Name {

			if _, errC := t.Create(); errC != nil {
				return 0, errC
			}

			pt, errF := t.FindPrev()
			if errF != nil {
				return 0, errF
			}

			t.Todos = pt.Todos

			_, errW := t.Write()

			if errW != nil {
				return 0, errW
			}
		} else {
			return 0, errors.Errorf("%v does not exist", red.Sprint(path))
		}
	}

	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		return 0, err
	}

	t.Content = string(bytes)

	lists := strings.Split(strings.Replace(t.Content, "TODO\n", "", 1), "Accomplished")

	t.Todos = parseList(lists[0])[1:]   // first item is an empty string
	t.Accomps = parseList(lists[1])[1:] // first item is an empty string

	return len(bytes), nil
}

// Print prints the TodoFile TODO & Accomplished lists.
func (t *TodoFile) Print() {

	red.Println("TODO")
	if len(t.Todos) > 0 {
		printList(t.Todos)
	} else {
		fmt.Println("\nWhoops nothing to do, try adding some tasks: \n  $ td add [task]")
	}

	green.Println("\nAccomplished")
	if len(t.Accomps) > 0 {
		printList(t.Accomps)
	} else {
		fmt.Println("\nIf you finished something don't forget to mark it as done: \n  $ td done [# number]")
	}
}

// FindPrev finds the previous TD file, perses and returns it.
func (t *TodoFile) FindPrev() (TodoFile, error) {
	format := t.BasePath + "/2006/01-02.txt"

	path, err := t.Path()

	if err != nil {
		return TodoFile{}, err
	}

	date, err := time.Parse(format, path)

	if err != nil {
		return TodoFile{}, err
	}

	// TODO: find from previous year
	// TODO: test / double check edge cases
	// TODO: check if file is a directory
	// TODO: stop when no more files (i.e. year's directory does not exist / is empty)
	for {
		date = date.Add(time.Hour * -24)
		path := date.Format(format)

		if _, err := os.Stat(path); err == nil {
			prev := New(t.BasePath)
			prev.Year = date.Format("2006")
			prev.Name = date.Format("01-02.txt")

			if _, errP := prev.Read(); err != nil {
				return TodoFile{}, errP
			}

			return prev, nil
		}
	}
}

// Write writes the lists into the file.
func (t *TodoFile) Write() (int, error) {
	path, err := t.Path()
	if err != nil {
		return 0, err
	}

	todos := ""

	for _, item := range t.Todos {
		todos = fmt.Sprintf("%v- %v\n", todos, item)
	}

	accomps := ""

	for _, item := range t.Accomps {
		accomps = fmt.Sprintf("%v- %v\n", accomps, item)
	}

	if len(t.Accomps) > 0 {
		accomps = fmt.Sprintf("\n\n%v", accomps)
	}

	content := fmt.Sprintf("TODOS\n\n%v\nAccomplished%v", todos, accomps)

	bs := []byte(content)
	err = ioutil.WriteFile(path, bs, os.ModePerm)

	return len(bs), err
}

func (t *TodoFile) Add(text string) error {

	t.Todos = append(t.Todos, text)
	_, err := t.Write()

	return err
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
