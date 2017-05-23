# Longboye

![Longboye](https://github.com/lgastako/longboye/blob/master/longboye.jpg?raw=true "Longboye")

> "Dogs are better than human beings because they know but do not tell."
> - Emily Dickinson

Longboye is a Haskell source prettifier.

Currently it only touches `import` statements.

Pass it one or more paths to filenames or directories on the command line and
it will process each file (when given files), or each `.hs` file in each
directory (recursively, when given directories).

The [examples/](examples/) directory shows an example of many different imports
pulled from open source projects.  For examples of real-life layout in context
in one project, see the Longboye source code.  Longboye eats it's own dog
food. 🐶

## Emacs Integration

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
                                 t         ;; delete
                                 t         ;; destination
                                 nil       ;; display
                                 "imports"
                                 "-"
                                 )
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

## TODOs

- [ ] Bonus as/hiding/other collapses
- [ ] Add 'modules' command for cleaning up module declarations
- [ ] Add automatic and/or option for removal of trailing whitespace (per line)
- [ ] Solicit Feedback
- [ ] Clean up UX/ergonomics/polish.  eg. something better than "Processing file: ..."
  - [ ] add -q to prevent output other than errors

## License

Longboye is licensed under the BSD3 open source license.  See LICENSE file.

Copyright Superpowers Corp © 2017.
