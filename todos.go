// Package todos handles creation of TODO files and adding and completting todos.
package todos

import (
	"log"
	"os"
	"os/user"
	"path/filepath"
	"strings"

	"github.com/fatih/color"
	"github.com/pkg/errors"
)

var red *color.Color = color.New(color.FgRed, color.Bold)
var green *color.Color = color.New(color.FgGreen, color.Bold)

// Init initializes the TD's directory and creates a TodoFile at today's path.
func Init(dirPath string) (string, error) {
	t := New(dirPath)

	if err := os.Mkdir(t.BasePath, 0755); err != nil {
		if os.IsExist(err) {
			return "", errors.Errorf("Cannot initialize TD at %v, already exists.", red.Sprint(dirPath))
		}
		return "", err
	}

	return t.Create()
}

// homeDir retrieves the home directory path for the current user.
func homeDir() string {
	usr, err := user.Current()
	if err != nil {
		// no intention of handling user lookup errors
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
