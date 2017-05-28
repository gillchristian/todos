package todos

import (
	"os"
	"time"

	"github.com/pkg/errors"
)

// TODO: export properties too (?)

// TodoFile stores the data of a to-do file and has the relevant methods to interact with it.
type TodoFile struct {
	todos    []string
	accomps  []string
	year     string
	name     string
	content  string
	basePath string
}

// New creates a new TodoFile with path as the TD's base path.
func New(path string) TodoFile {
	return TodoFile{basePath: path}
}

// Dir returns the TodoFile directory path ("<base-path>/<year>").
// If t.year is missing, it will assigend first to the current year.
func (t *TodoFile) Dir() (string, error) {
	if t.basePath == "" {
		return "", errors.New("basePath missing")
	}
	if t.year == "" {
		t.year = time.Now().Format("2006")
	}
	return sanitizePath(t.basePath + "/" + t.year), nil
}

// Path returns the TodoFile path ("<base-path>/<year>/<month>-<day>.txt").
// If t.year and t.name are missing, they will assigend first to the current date.
func (t *TodoFile) Path() (string, error) {
	dir, err := t.Dir()
	if err != nil {
		return "", err
	}
	if t.name == "" {
		t.name = time.Now().Format("01-02.txt")
	}
	return sanitizePath(dir + "/" + t.name), nil
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
