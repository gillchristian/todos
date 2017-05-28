// Package todos handles creation of TODO files and adding and completting todos.
package todos

import (
	"log"
	"os"
	"os/user"
	"path/filepath"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/pkg/errors"
)

var red *color.Color = color.New(color.FgRed, color.Bold)
var green *color.Color = color.New(color.FgGreen, color.Bold)

// TODO: export properties too (?)

// TodoFile stores the data of a TODO file.
type TodoFile struct {
	todos    []string
	accomps  []string
	year     string
	name     string
	content  string
	basePath string
}

// New creates a new TodoFile with path as the TODO's base path.
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
// Asumes TODO's base path has already been created.
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

// Init initializes the TODO's directory and creates a TodoFile at today's path.
func Init(dirPath string) (string, error) {
	t := TodoFile{
		basePath: sanitizePath(dirPath),
	}

	if err := os.Mkdir(t.basePath, 0755); err != nil {
		if os.IsExist(err) {
			return "", errors.New("Cannot initialize TODO at " + red.Sprint(dirPath) + ", already exists.")
		}
		return "", err
	}

	return t.Create()
}

// homeDir retrieves the home directory path for the current user.
func homeDir() string {
	usr, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return usr.HomeDir
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
