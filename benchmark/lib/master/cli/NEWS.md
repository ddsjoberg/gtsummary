
# cli 2.5.0

* New `style_no_*()` functions to locally undo styling.
  New `col_none()` and `bg_none()` functions to locally undo text color
  and background color.

* It is now possible to undo text and background color in a theme, by
  setting them to `NULL` or `"none"`.

* `cli_memo()` was renamed to `cli_bullets()`, as it is by default
  formatted as a bullet list (#250).

* New `ansi_toupper()`, `ansi_tolower` and `ansi_chartr()` functions,
  the ANSI styling aware variants of `toupper()`, `tolower()` and
  `chartr()` (#248).

* New `test_that_cli()` helper function to write testthat tests for
  cli output.

* `tree()` now does not produce warnings for tibbles (#238).

* New inline style: `.cls` to format class names, e.g.
  `"{.var fit} must be an {.cls lm} object"`.

# cli 2.4.0

* New `cli_memo()` function to create a list of items or tasks.

* New `cli::cli()` function to create a single cli message from multiple
  cli calls (#170).

* cli now highlights weird names, e.g. path names with leading or
  trailing space (#227).

* Styling is fixed at several places. In particular, nested lists should
  be now formatted better (#221).

* New `spark_bar()` and `spark_line()` funcions to draw small bar or
  line charts.

# cli 2.3.1

* ANSI color support detection works correctly now in older RStudio,
  and also on older R versions.

* `cli_h1()`, `cli_h2()` and `cli_h3()` now work with multiple glue
  substitutions (#218).

# cli 2.3.0

* `boxx()` now correctly calculates the width of the box for non-ASCII
  characters.

* New `ansi_trimws()` and `ansi_strwrap()` functions, they are similar
  to `trimws()` and `strwrap()` but work on ANSI strings.

* New `ansi_columns()` function to format ANSI strings in multiple columns.

* `ansi_substr()`, `ansi_substring()`, `ansi_strsplit()`, `ansi_align()`
  now always return `ansi_string` objects.

* `ansi_nchar()`, `ansi_align()`, `ansi_strtrim()` and the new
  `ansi_strwrap()` as well handle wide Unicode correctly, according to
  their display width.

* `boxx()` can now add headers and footers to boxes.

# cli 2.2.0

* New `style_hyperlink()` function to add hyperlinks, on terminals that
  support them.

* `cli_format_method()` now works properly in knitr, and other environments
  that catch message conditions (#159).

* ANSI strings created by `col_*`, `bg_*` and `style_*` now also add the
  `character` class to the result. This fixes issues with code that
  expect `character` objects.

* New functions to manipulate ANSI strings: `ansi_aling()`,
  `ansi_has_any()`, `ansi_nchar()`, `ansi_regex()`, `ansi_strip()`,
  `ansi_strsplit()`, `ansi_substr()`, `ansi_substring()`.

# cli 2.1.0

* New `cli_vec()` function to allow easier formatting of collapsed
  vectors. It is now also possible to use styling to set the collapsing
  parameters (#129).

* New `pluralize()` function to perform pluralization without generating
  cli output (#155).

* `console_width()` works better now in RStudio, and also in terminals.

* Styling of verbatim text work properly now (#147, @tzakharko).

* Messages (ie. `message` conditions) coming from cli now have the
  `cliMessage` class, so you can easily suppress them without suppressing
  other messages (#156).

* cli prints the output to `stderr()` now, if there is an output or
  message sink. This is to make interactive and non-interactive sessions
  consistent (#153).

* Pluralization works corrently now if the last alternative is the
  empty string (#158).

* cli now caches the result of the dark background detection in iTerm on
  macOS. Reload cli to delete the cache (#131).

* The `is_dynamic_tty()`, `is_ansi_tty()` and `ansi_hide_cursor()` and
  releted functions now default to the `"auto"` stream, which is
  automatically selected to be either `stdout()` or `stderr()`.
  See the manual for details (#144).

* The default theme now quotes file names, paths, email addresses if they
  don't start or end with an alphanumeric character or a slash. This is
  to make it easier to spot names that start or end with a space (#167).

* `make_spinner()` clears the line properly now (@tzakharko, #164).

* Semantic cli functions now automatically replace Unicode non-breaking
  space characters (`\u00a0`) with regular space characters, right before
  output. They are still used to calculate the line breaks, but not
  outputted (#161).
* Progress bars now respect `is_dynamic_tty()` and do not output `\r` when this
  is false (@jimhester, #177)

# cli 2.0.2

* The status bar now does not simplify multiple spaces by a single space.

* cli now does not crash if it fails to detect whether the RStudio theme
  is a dark theme (#138).

* cli now works better with wide Unicode characters, for example emojis.
  In particular, a status bar containing emojis is cleared properly (#133).

* The status bar now does not flicker when updated, in terminals (#135).

# cli 2.0.1

* Symbols (`symbol$*`) are now correctly printed in RStudio on Windows (#124).

* The default theme for `cli_code()` output looks better now, especially
  in RStudio (#123).

* Remove spurious newline after a `cli_process_start()` was cleared
  manually, and also at the end of the function.

* Use Oxford comma when listing 3 or more items (@jonocarroll, #128).

# cli 2.0.0

## Semantic command line interface tools

cli 2.0.0 has a new set of functions that help creating a CLI using a set
of higher level elements: headings, paragraphs, lists, alerts, code blocks,
etc. The formatting of all elements can be customized via themes.
See the "Building a semantic CLI" article on the package web site:
https://cli.r-lib.org

## Bug fixes:

* Fix a bug in `is_dynamic_tty()`, setting `R_CLI_DYNAMIC="FALSE"` now
  properly turns dynamic tty off (#70).

# cli 1.1.0

* cli has now functions to add ANSI styles to text. These use the crayon
  package internally, and provide a simpler interface. See the `col_*`,
  `bg_*`, `style_*` and also the `make_ansi_style()` and
  `combine_ansi_styles()` functions (#51).

* New `is_dynamic_tty()` function detects if `\r` should be used for a
  stream (#62).

* New `is_ansi_tty()` function detects if ANSI control sequences can be
  used for a stream.

* New `ansi_hide_cursor()`, `ansi_show_cursor()` and
  `ansi_with_hidden_cursor()` functions to hide and show the cursor in
  terminals.

* New `make_spinner()` function helps integrating spinners into your
  functions.

* Now `symbol` always uses ASCII symbols when the `cli.unicode` option is
  set to `FALSE`.

# 1.0.1

* New `cli_sitrep()` function, situation report about UTF-8 and ANSI
  color support (#53).

* Fall back to ASCII only characters on non-Windows platforms without
  UTF-8 support, and also in LaTeX when running knitr (#34).

# cli 1.0.0

First public release.
