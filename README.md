## Introduction

Second Climacs is an Emacs-like editor that is focused on editing
Common Lisp code and that is written entirely in Common Lisp.  The
project is called Second Climacs because it is a complete rewrite of
the [Climacs](https://github.com/robert-strandh/Climacs) text editor.

The (First) Climacs editor gave us some significant experience with
writing a text editor, and we think we can improve on a number of
aspects of it.  As a result, there are some major differences between
(First) Climacs and Second Climacs:

* We implemented a better buffer representation, and extracted it from
  the editor code into a separate library named
  [Cluffer](https://github.com/robert-strandh/Cluffer).  The new
  buffer representation will have better performance, especially on
  large buffers, and it will make it easier to write sophisticated
  parsers for buffer contents.

* The incremental parser for Common Lisp syntax of (First) Climacs is
  very hard to maintain, and while it is better than that of Emacs, it
  is still not good enough.  Second Climacs uses an extensible
  implementation of the Common Lisp reader named
  [Eclector](https://github.com/s-expressionists/Eclector) together
  with an incremental parsing library named
  [Incrementalist](https://github.com/s-expressionists/incrementalist)
  to parse buffer contents, making it extremely close to the way a
  Common Lisp compiler reads its input.

* (First) Climacs implements text editing commands as CLIM commands
  which assume a single current cursor.  This design prevents reuse of
  text editing commands across different editors and makes it
  difficult to apply commands in other ways such as applying a given
  command for each of multiple cursors.  Second Climacs uses the
  [text.editing](https://github.com/scymtym/text.editing) library to
  address both issues.

## Quick Start

1. Clone the
   [Second Climacs repository](https://github.com/robert-strandh/Second-Climacs)
   with `git`:

   ```
   $ git clone https://github.com/robert-strandh/Second-Climacs
   ```

1. Make sure the directories of the cloned repository can be found by
   ASDF.

1. Make sure you have installed the dependencies (dependencies that
   are readily available via Quicklisp are not listed):

   * A recent 64-bit version of SBCL
   * the [Cluffer library](https://github.com/robert-strandh/Cluffer)
   * the [text.editing library](https://github.com/scymtym/text.editing)
   * the [Eclector library](https://github.com/s-expressionists/Eclector)
   * the [Incrementalist library](https://github.com/s-expressionists/incrementalist)

   The bash script `get-dependencies.sh` from this repository can
   clone the above repositories into
   `$HOME/quicklisp/local-projects/`.

1. Compile the editor system as follows:

   ```lisp
   (ql:quickload "second-climacs-clim")
   ```

1. To start Second Climacs, execute this form:

   ```lisp
   (second-climacs-clim-base:climacs)
   ```

## Documentation

Check the `Documentation` directory for more information.

## Commands

At the moment, all you can do is type some text, and you can use `C-x i`
to insert and existing file.  Some basic Emacs commands also work, like
`C-f`, `C-b`, `C-p`, `C-n`, `M-<`, `M->`, and `C-x` `C-c`.  The
visible window does not automatically follow the cursor yet.

## Contributing

We are not accepting contributions at this time.
