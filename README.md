# Longboye

![Longboye](https://github.com/lgastako/longboye/blob/master/longboye.jpg?raw=true "Longboye")

> "Dogs are better than human beings because they know but do not tell."
> - Emily Dickinson

Longboye is a Haskell source prettifier.

Currently it only touches `import` statements but we plan to give the module
statement and language pragmas a similar treatment as well adding as some
whole-file touch-ups like hygenic whitespace management.

Pass the `longboye imports` command one or more paths to files or directories
on the command line and it will prettify each file specified and each `.hs`
file found recursively in each directory specified.

## The Format

It's better seen than described, as in the next section ("Examples"), but the
gist so far is:

- Any imports with a `Prelude` component are moved to the top.
- A blank line is added between any `Prelude` imports and the rest.
- Any extra blank lines between the first import and the top of the file are
  removed
- Any comments between the first import and the last import are removed
- Imports are sorted
- All imports are aligned in columns for `qualified`, the module name, `as`
  clause, and `hiding` clauses.
- Any imports with 0-1 members with 0-1 operators is left on the same line
  (eg. `import Foo ( Bar( baz ) )`).
- Any imports with 2 or more members or 2 or more operators are split across
  lines.
- A blank line is added between the last import and the rest of the file.
- Any extra blank lines between the last import and rest of the file are removed

## Examples

For real examples of Goodboye™ format, see the Longboye source code (Longboye
eats its own dog food):

- [src/Longboye/ImportsParser.hs](/../../tree/master/src/Longboye/ImportsParser.hs) - a
  fairly typical example
- [src/Longboye/Imports.hs](/../../tree/master/src/Longboye/Imports.hs) - a
  file with a lot of imports and a `hiding` statement
- [test/Spec.hs](/../../tree/master/test/Spec.hs) - a simple (mostly collapsed)
  file

It may take some developers a little time to get used to the exotic styling of
the Goodboye™ format but those who have the fortitude to see it through will be
rewarded with the bountiful gifts of clean code undreamed of by most of
mankind.

## Features

- [X] Aligns all imports in normalized Goodboye™ format with patented
      insta-collapse
  - [X] Sorts import statements
  - [X] Sorts members within import statements
  - [X] Removes duplicate import statements
  - [X] Normalizes whitespace between module statement and import statements
  - [X] Normalizes import statements and definitions
  - [X] Normalizes whitespace between import statements and module body
  - [X] Longboye will find and read `.cabal` files to load default extensions
- [X] Emacs integration
- [X] Is a good boy.
  - [X] Yes he is.

## Coming Soon

- [#9](/../../issues/9) - Handle `TypeNamespace` and `PatternNamespace`
- [#10](/../../issues/10) - Add `-q` option to prevent output other than errors
- [#11](/../../issues/11) - Automatic removal of per-line trailing whitespace
- [#12](/../../issues/12) - Automatic removal of per-file trailing whitespace
- [#13](/../../issues/13) - `modules` sub-command for cleaning up module declarations
- [#14](/../../issues/14) - `pragmas` sub-command for cleaning up LANGUAGE pragmas
- [#15](/../../issues/15) - CLI combinators for `imports` / `modules` / `pragmas` commands.
- [#5](/../../issues/5) - Better treatment of comments
- [#6](/../../issues/6) - Better error handling in editor integration
- Possibly further bonus collapsing as/hiding/etc.

We love feedback, so feel free to chime in on any of the issues.

## Caveats and Emptors<sup>[*](#emptors)</sup>

- Any comments that appear between the first uncommented import statement and
  the last uncommented import statement will be removed.

- Using the current emacs integration function will cause your cursor to jump
  around on save in some situations.

<a name="emptors">*</a> Emptors are Functors over expectations.

## Installation

    $ make build install
    ...
    $ longboye --help

## Installation

    $ make build install
    $ longboye --help

## Emacs Integration

Put something like this in the appropriate spot in your
[Emacs Init](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)
file.  We suggest you use [use-package](https://github.com/jwiegley/use-package)
and put it in the `:init` section of your `haskell-mode` package declaration.

```elisp
  (defun longboyeee ()
    (interactive "r")
    (when (eq major-mode 'haskell-mode)
      (let ((start    1)
            (end      (+ 1 (buffer-size)))
            (program "longboye"))
        (let ((saved-cursor-position (point)))
            (call-process-region start
                                 end
                                 program
                                 t                   ;; delete
                                 t                   ;; destination
                                 nil                 ;; display
                                 "imports" "-")      ;; additional args
            (goto-char saved-cursor-position)))))

  (add-hook 'before-save-hook #'longboyeee)
```

See [issue 20](https://github.com/SuperpowersCorp/longboye/issues/20) for
details of the issue of the cursor jumping around.

## Data Integrity

The operations are performed by:

- making a temporary backup of the file in the same directory as the original
- writing the new content to a temporary file in the same directory
- atomically swapping the temporary file into place
- deleting the backup file

This should provide a relativel high degree of safety.  If Longboye does detect
an error it will abort all operations leaving the backup file in the same
directory with a `.longboye.bak` extension.

## License

Longboye is licensed under the BSD3 open source license.  See LICENSE file.

Copyright Superpowers Corp © 2017.
