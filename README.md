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

- [X] Basic Formatting
- [X] Sort Imports (with Prelude at top)
- [X] Prelude to the top (also maybe Overture?)
  - [X] space between them and the rest if either is present
- [X] BUG: in current version Prelude HIDING (some, shit) wraps across lines
      wrong (mayb be fixed by above)
- [X] Extra spacing at end of hiding

- [ ] Condensing
  - [ ] BUG: Handle extra indenting for global hiding resolution
  - [ ] Even as is, hiding should be in the as column, but let's just figure out
        condensing rules instead and let that sort out the rest.
    - [X] Condense qualified if there are no qualified
    - [X] Condense as column if there are no as
    - [ ] Line up ops at end up asCol (as is) plus any extra space any hiding's
          need (if any)

- [ ] Atomic file swapping
- [ ] Sub-directory traversal
- [ ] Examples
- [ ] Solicit Feedback
- [ ] License
- [ ] BUG: If you have a module with imports and no body, eg. app/Main.hs
      it will fail to parse the file

## Examples

    TODO: Add examples.

## License

    TODO: License.
