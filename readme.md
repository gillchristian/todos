# td [![Godoc](https://godoc.org/github.com/gillchristian/todos?status.svg&style=flat-square)](http://godoc.org/github.com/gillchristian/todos)

A to-do's CLI app written in Go. Inspired on [The Power of the TODO List](https://goo.gl/j1dQ4M) by [James Hood](http://jlhood.com/) (:bird: [@jlhcoder](https://twitter.com/jlhcoder)).

## Installation


```bash
$ go get -u github.com/gillchristian/todos/cmd/td
```

## Usage

```bash
# Initialization

$ td init ~/Desktop/todos # initialize to-do's directory and create today's to-do file
$ td init                 # use default directory ~/.todos

# List TODOs and Acommplished items 

$ td list      # defaults to today's file (*)
$ td           # just an alias for ^
$ to list last # lists the last found (not today's)

# Add a TODO

$ td add <TODO item> # adds the item to today's list (*)

# Set a TODO as completed
# If I is not provided TD will list today's TODOs and ask for I

$ td done [I] # sets TODO #I as accomplised, if it exists (*)

# Remove a TODO
# If I is not provided TD will list today's TODOs and ask for I

$ td rm [I] # removes TODO #I, if it exists (*)

# Generate a report
# 

$ td report <week|month|year>
$ td report <week|month|year> -o <FILE> # write the report to <FILE>
```

`(*)` creates today's file if not yet created.


**NOTE**: if you use a custom directory you will have to specify it on every command with the `--dir` flag. A solution fo that is to use an alias:

```bash
alias td="td --dir ~/Desktop/todos"
```

## TO-DO

- [x] `$ td`
- [x] `$ td init`
- [x] `$ td list`
- [ ] `$ td list last`
- [ ] `$ td list <date>`
- [ ] `$ td add <todo item>`
- [ ] `$ td done [I]`
- [ ] `$ td rm`
- [ ] `$ td report <week|month|year>`
- [ ] Create today's file when not found (using yesterday's TODOs).
- [ ] Feed back messages:
  - [x] `$ td init` was not run, i.e. `--dir` does not exists.
  - [ ] No TODOs (well done! & suggest to use `$ td add <todo item>`).
  - [ ] No Accomplished (mmm! & suggest to use `$ td done`).
  - [ ] No items (both TODOs & Accomplished suggestions).
- [ ] Add tests.

