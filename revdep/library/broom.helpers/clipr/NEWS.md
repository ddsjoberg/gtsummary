## clipr 0.7.1

- Call xsel with the `--output` flag, which prevents RStudio from hanging when calling clipr functions on a system running certain Linux window managers. Thank you to @cgillespie and @kevinushey for identifying the bug and the solution, and to @hannahcgunderman for help in testing.

## clipr 0.7.0

Thank you to @jennybc for prompting these changes:

- Before attempting to read/write form the clipboard, `clipr_available()` will first explicitly check if it is being run non-interactively, and if so, if the `CLIPR_ALLOW` environment variable has been set. This will hopefully prevent starting spurious Linux processes during CRAN tests.
- Out of an abundance of caution, `read_clip()` now does the same interactive/envvar check that `write_clip()` does.
- Some documentation clarifications

## clipr 0.6.0

Thank you to @wangyuchen for making the following suggestions:

- To make clipr more pipe-friendly, `write_clip()` now defaults to `return_new = FALSE`, and will instead return the initial object that was passed in. To get the old behavior, pass `return_new = TRUE` 

- In an effort to make `write_clip()` and `read_clip_tbl()` more symmetrical, `write_clip()` now defaults to writing out row and column names when they exist.

- Introduces `write_last_clip()`, a wrapper function for `write_clip(.Last.value)`

## clipr 0.5.0

- To comply with CRAN policy, `write_clip()` will now error by default if run in
a non-interactive session. Non-interactive use must be explicitly enabled by
setting an environment variable `CLIPR_ALLOW=TRUE`.

- Documented that the default behavior when writing matrices to `write_clip()`
is `col.names = FALSE`

## clipr 0.4.1

- Correct a formatting error by adding and separation character to tables when
they are being written with rownames.

## clipr 0.4.0

- Introduces `dr_clipr()`, which gives informative suggestions for software and
configuration requirements when accessing the clipboard on X11-based systems.

## clipr 0.3.3

- Due to poor testing and configuration options, clipr was not delivering on its
promised support for xsel :( This has now been fixed, with more complete Travis 
tests, and some core fixes by @milesmcbain.

## clipr 0.3.2

- Suppress an erroneous warning on OS X / X11 systems when trying to write an 
empty string to the clipboard.

- Fix error when `NA` is passed to `write_clip()`. This will now write `"NA"` to
the clipboard.

- Fix error when passing `NULL` or an empty vector (e.g. `character(0)`). This 
will now write `""` to the clipboard.

## clipr 0.3.1

- Fixes a breaking bug that caused `clipr_available` to erroneously return 
`FALSE`. Thank you to @krivit for catching this.

- Introduces better testing of `clipr_available` to properly evaluate it on 
Travis CI.

## clipr 0.3.0

- Introduces `clipr_available` which checks to see if the system clipboard is 
writeable/readable. This may be useful if you are developing a package that 
relies on clipr and need to ensure that it will skip tests on machines (e.g. 
CRAN, Travis) where the system clipboard may not be available. Thank you to
@jennybc for this suggestion.
  
  - Implements genuine testing of clipr functionality with thanks to some deft 
  environment variable settings added by @jennybc.
  
  - Two Rstudio addins: one to copy the _value_ returned when a highlighted 
  expression is evaluated, and another that copies the _console output_.
  
  ## clipr 0.2.1
  
  - Introduces `read_clip_tbl`, a convenience function that takes tab-delimited 
  text from `read_clip` (such as that copied from a spreadsheet) and parses it 
  with `read.table`. Thank you to Steve Simpson (@data-steve) for the original 
  PR.
  
  - `write_clip(object_type = "table")` has a new internal implementation 
  (writing to a temporary file rather than using `capture.output`) which should 
  dramatically shorten the time it takes to write very large tables to the 
  clipboard. Thank you to @r2evans for this suggestion.
  
  ## clipr 0.2.0
  
  - Several changes to `write_clip` - The separator to be used when writing a 
  character vector can now be explicitly declared using `breaks`. `breaks=NULL` 
  will default to system-specific line breaks for both vectors and tables. - 
  `write_clip` will default to formatting data.frames and matrices with 
  `write.table`, allowing easy pasting of tabular objects into programs like 
  Excel. Option `object_type="auto"` will check the object type to decide on the
  correct formatting, or the user may explicitly state `object_type="table"` or 
  `object_type="character"`. - clipr will default to sane system-specific 
  options for `write.table()`, however you may pass any custom desired options 
  via `write_clip` - `return_new=TRUE` (the default behavior) will return the 
  formatted character string that was passed to the system clipboard, while 
  `write_clip(return_new=FALSE)` will return the original object.
  
  - Introduces `clear_clip`, a wrapper function for `write_clip("")` for easy 
  clearing of the system clipboard.
  
  ## clipr 0.1.1
  
  - Bug fix that removes the explicit test for "Linux" in favor of a check for 
  "xclip" or "xsel"
