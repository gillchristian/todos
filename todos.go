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

type TodoFile struct {
	todos   []string
	accomps []string
	year    string
	name    string
	content string
}

var red *color.Color = color.New(color.FgRed, color.Bold)
var green *color.Color = color.New(color.FgGreen, color.Bold)

func Init(dirPath string) (string, error) {
	dir := sanitizePath(dirPath)

	if err := os.Mkdir(dir, 0755); err != nil {
		if os.IsExist(err) {
			return "", errors.New("Cannot initialize TODO at " + red.Sprint(dirPath) + ", already exists.")
		}
		return "", err
	}

	now := time.Now()

	return TodoFile{
		year: now.Format("2006"),
		name: now.Format("01-02.txt"),
	}.Create(dir)
}

func (t TodoFile) Create(dirPath string) (string, error) {
	dir := sanitizePath(dirPath + "/" + t.year)

	if _, err := os.Stat(dir); os.IsNotExist(err) {
		if err := os.Mkdir(dir, 0755); err != nil {
			return "", errors.Wrap(err, "Cannot create directory:")
		}
	}

	filePath := sanitizePath(dir + "/" + t.name)

	f, err := os.Create(filePath)
	if err != nil {
		return "", errors.Wrap(err, "Cannot create file:")
	}
	defer f.Close()

	_, err = f.Write([]byte("TODO\n\nAccomplished"))
	if err != nil {
		return "", errors.Wrap(err, "Cannot write to file:")
	}

	return filePath, nil
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
