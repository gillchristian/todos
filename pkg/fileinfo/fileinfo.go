// Package fileinfo ...
// TODO: rename this package
package fileinfo

import (
	"io/ioutil"
	"log"
	"os"
	"os/user"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// FileInfo contains information about a file and, if it's a directory,
// methods to traverse it's content.
type FileInfo struct {
	Path      string
	Name      string
	IsDir     bool
	ModTime   time.Time
	Content   []FileInfo
	traversed bool
}

func (f FileInfo) Len() int {
	return len(f.Content)
}

func (f FileInfo) Swap(i, j int) {
	f.Content[i], f.Content[j] = f.Content[j], f.Content[i]
}

func (f FileInfo) Less(i, j int) bool {
	// TODO: > or < ???
	return f.Content[i].Name < f.Content[j].Name
}

// New creates a new FileInfo with the dir path and the file's os.FileInfo
func New(dir string, f os.FileInfo) FileInfo {
	path := filepath.Join(dir, f.Name())

	return FileInfo{
		Path:    SanitizePath(path),
		Name:    f.Name(),
		ModTime: f.ModTime(),
		IsDir:   f.IsDir(),
	}
}

// Files returns all files contained by f.
func (f FileInfo) Files() ([]FileInfo, error) {
	return f.getFilesBy(func(fi FileInfo) bool { return !fi.IsDir })
}

// Dirs returns all directories contained by f.
func (f FileInfo) Dirs() ([]FileInfo, error) {
	return f.getFilesBy(func(fi FileInfo) bool { return fi.IsDir })
}

type fileInfoPred func(FileInfo) bool

func (f *FileInfo) getFilesBy(pred fileInfoPred) ([]FileInfo, error) {
	files := []FileInfo{}
	if !f.IsDir {
		return files, nil
	}

	if err := f.traverse(); err != nil {
		return files, nil
	}

	for _, fi := range f.Content {
		if pred(fi) {
			files = append(files, fi)
		}
	}

	return files, nil
}

func (f *FileInfo) traverse() error {
	if f.traversed || !f.IsDir {
		return nil
	}

	f.traversed = true

	fs, err := ioutil.ReadDir(f.Path)
	if err != nil {
		return err
	}

	f.Content = []FileInfo{}

	for _, fi := range fs {
		// TODO: add ignored files here
		if fi.Name() != ".git" {
			f.Content = append(f.Content, New(f.Path, fi))
		}
	}

	sort.Sort(f)

	return nil
}

// SanitizePath makes sure a path is absolute and clean.
// It also replaces $HOME & ~ with the home directory for the current user.
func SanitizePath(elem ...string) string {
	path := filepath.Join(elem...)

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

// homeDir retrieves the home directory path for the current user.
func homeDir() string {
	usr, err := user.Current()
	if err != nil {
		// no intention of handling user lookup errors
		log.Fatal(err)
	}
	return usr.HomeDir
}
