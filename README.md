# Longboye

![Longboye](https://github.com/lgastako/longboye/blob/master/longboye.jpg?raw=true "Longboye")

> "Dogs are better than human beings because they know but do not tell."
> - Emily Dickinson

Longboye is a Haskell import statement prettifier.

You pass it one or more paths to filenames or directories on the command line
and it will process each file (when given paths to files), or each `.hs` file
in each directory (recursively, when given paths to directories).

## Data Integrity

The operations are performed by:

- making a temporary backup of the file in the same directory as the original
- writing the new content to a temporary file in the same directory
- verifying the new content in the temporary file
- atomically swapping the temporary file into place
- verifying the new content in place
- deleting the backup file

This provides a high degree of safety.  Of course cosmic rays do flip bits
occasionally, so if somehow Longboye detects any problems or crashes at any
point, you can find the original contents of the file in the temporary `.lbak`
file in the same directory.

## TODOs

- [x] Basic Formatting
- [ ] Atomic file swapping
- [ ] Sort Imports (with Prelude at top)
- [ ] Handle extra indenting for global hiding resolution

## Examples

    TODO: Add examples.

## License

    TODO: License.
