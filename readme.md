<h1 align="center">TODO (td)</h1>

<p align="center">

A to-do's CLI app. Inspired on
[The Power of the TODO List](https://goo.gl/j1dQ4M) by
[James Hood](http://jlhood.com/) (:bird:
[@jlhcoder](https://twitter.com/jlhcoder)).

</p>

## Installation

**WIP**

_I'm migrating it from Go to Haskell ~because reasons~, still gotta get the installation part sorted out._

## Usage

```
TODO (td), a command line tool to handle your daily tasks

TODO is inspired by "The Power of the TODO List"
(https://dev.to/jlhcoder/the-power-of-the-todo-list), a simple, yet powerful,
approach to keep track of your daily tasks.

TODO saves your items (on ~/.todos) in a readable format that you can
edit/read/grep yourself. Make sure to checkout the article to learn more.

TODO takes care of the files for your, so you don't have to. It creates a new
one every day when you run any of the commands. All the stuff you did the
previous day stays there and the pending items are copied to today (yup you gotta
finish what you started, eh?)

Author: gillchristian (https://gillchristian.xyz)

Version: 0.0.9

Usage:
  $ td [command] [arguments]

Commands:
  list:
    Show today's pending and done items.

    Usage:
      $ td list
      $ td      # no command defualts to list

  last:
    Show previous day pending and done items.

    Usage:
      $ td last

  add:
    Add a new pending item to today's list and show the updated list.
    Use quotes when you want to use a symbol that isn't supported by the shell.

    Usage:
      $ td add Do something awesome today
      $ td add 'Do stuff (not that stuff)'

  done:
    Mark a pending item as done (Accomplished) and show the updated list.
    If no number is provided you will be prompted to input one.

    Usage:
      $ td done
      $ td done [x]

  standup:
    List previous day done items and today's pending ones.
    This serves as a report for your (you guessed it) standup.

    Usage:
      $ td standup

  version:
    Show the version (just in case you'd like to know.)

    Usage:
      $ td version
      $ td --version

  help:
    Show this message. Duh!

    Usage:
      $ td help
      $ td --help
```

## TO-DO

- [x] `$ td` (lists today's todo/done)
- [x] `$ td last` (list previous's day todo/done)
- [x] `$ td add <todo item>`
- [x] `$ td standup` (list previous's day done & today's todo)
- [x] `$ td done` /  `$ td done [x]`
- [ ] `$ td rm` / `$ td rm [x]`
- [x] `$ td init` (instead any command just creates dir and first file if missing)
- [x] `$ td help`
- [x] Create today's file when not found (from previous one).
- [ ] Tests :tm:

