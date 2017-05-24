# Longboye

![Longboye](https://github.com/lgastako/longboye/blob/master/longboye.jpg?raw=true "Longboye")

> "Dogs are better than human beings because they know but do not tell."
> - Emily Dickinson

Longboye is a Haskell source prettifier.

Currently it only touches `import` statements but I plan to give the module
statement and language pragmas a similar treatment.

Pass it one or more paths to filenames or directories on the command line and
it will process each file (when given files), or each `.hs` file in each
directory recursively (when given directories).

The [examples/](examples/) directory shows an example of many different imports
pulled from open source projects.  For examples of real-life layout in context
in one project, see the Longboye source code.  Longboye eats it's own dog
food. 🐶

## Features

- [X] Aligns all imports in normalized Goodboye™ format with patented
  insta-collapse feature
- [X] Sorts import statements
- [X] Removes duplicate import statements
- [X] Normalizes whitespace between module statement and import statements
- [X] Normalizes import statements and definitions
- [X] Emacs integration
- [X] Is a good boy.
  - [X] Yes he is.

## Coming Soon

- [ ] Handle `TypeNamespace` and `PatternNamespace`
- [ ] Sorting of members within parens
- [ ] Longboye will find and read `.cabal` files to load default extensions
- [ ] Better treatment of comments
  - [ ] (?) `--preserve-comments` flag that lumps them all together at the top
- [ ] Better error handling in editor integration
  - [ ] (?) `--error-comments` flag that puts errors in comments at ... top of file?
- [ ] Bonus as/hiding/other collapses
- [ ] `modules` command for cleaning up module declarations
- [ ] `pragmas` command for cleaning up LANGUAGE pragmas
- [ ] `-q` option to prevent output other than errors
- [ ] Automatic and/or option for removal of trailing whitespace (per line)
- [ ] Clear documention of any unfixed known issues
  - [ ] Comment removal
  - [ ] Cursor jumping around in emacs

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
