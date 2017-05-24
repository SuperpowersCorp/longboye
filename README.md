# Longboye

![Longboye](https://github.com/lgastako/longboye/blob/master/longboye.jpg?raw=true "Longboye")

> "Dogs are better than human beings because they know but do not tell."
> - Emily Dickinson

Longboye is a Haskell source prettifier.

Currently it only touches `import` statements but we plan to give the module
statement and language pragmas a similar treatment.

Pass it one or more paths to filenames or directories on the command line and
it will process each file (when given files), or each `.hs` file in each
directory recursively (when given directories).

The [examples/](examples/) directory shows an example of many different imports
pulled from open source projects.  For examples see the Longboye source code,
[eg. Longboye/Imports.hs](/../tree/master/src/Longboye/Imports.hs).  Longboye
eats it's own dog food. 🐶

## Features

- [X] Aligns all imports in normalized Goodboye™ format with patented
  insta-collapse feature
- [X] Sorts import statements
  - [X] Sorts members within import statements
- [X] Removes duplicate import statements
- [X] Normalizes whitespace between module statement and import statements
- [X] Normalizes import statements and definitions
- [X] Emacs integration
- [X] Is a good boy.
  - [X] Yes he is.

## Coming Soon

- [ ] [#7](/../../issues/7) - Longboye will find and read `.cabal` files to load default extensions
- [ ] [#5](/../../issues/5) - Better treatment of comments
- [ ] [#6](/../../issues/6) - Better error handling in editor integration
- [ ] [#9](/../../issues/9) - Handle `TypeNamespace` and `PatternNamespace`
- [ ] [#10](/../../issues/10) - Add `-q` option to prevent output other than errors
- [ ] Bonus as/hiding/other collapses
- [ ] `modules` sub-command for cleaning up module declarations
- [ ] `pragmas` sub-command for cleaning up LANGUAGE pragmas
- [ ] Automatic and/or option for removal of trailing whitespace (per line)
- [ ] Clear documention of any unfixed known issues
  - [ ] Comment removal
  - [ ] Cursor jumping around in emacs

## Installation

    $ make build install
    ...
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
