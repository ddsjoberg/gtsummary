NEWS for the htmlTable package

Changes fro 2.2.1
-----------------
* The `txtInt` handles `NA` more gracefully.

Changes fro 2.2.0
-----------------
* Added `htmlTable_args` attribute for making modifications to the final table easier

Changes fro 2.1.0
-----------------
* Added options for how the empty spacer cell appears (see `addHtmlTableStyle()` spacer.* options)
* Fixed ignored `align.header`
* Fix for wrapped styles (issue #80)

Changes for 2.0.1
-----------------
* Fix for txtRound not handling negative numbers (issue #76)
* Fix bug for `hidden.rgroup` & `hidden_tspanner` in `tidyHtmlTable`
* Documentation improvements & switched to markdown docs

Changes for 2.0.0
------------------
* Added theming and styling with `addHtmlTableStyle` and `setHtmlTableTheme` to reduce the cognitive burden of finding the right
  option within the docs. Note: this may impact your current tables and hence the major version (2.0.0).
* Changed so that `css.cell` is properly applied to rownames, cell fillers and the actual cells of interest (may impact the final layout!)
* Breaking change `tidyHtmlTable`: Moved to a fully tidyverse compatible system with tidyHtmlTable. This is a breaking change to the API as we switch from
  columns as strings to `tidyselect` syntax and as `gather`/`spread` have been replaced by `pivot_longer`/`pivot_wider`
  the default values have been updated in accordance with their defaults, e.g. `rnames = "name"` and `value = "value"`.
* Breaking change `tidyHtmlTable`: Sorting of rows is skipped as we may have situations with repeating inputs and this can easily
  be performed pre-function by calling `dplyr::arrange`. This has furthermore the desirable feature that any custom sorting
  is retained.
* Added *mso-number-format* to help (Issue #63) - thanks Rasmus Hertzum
* txtRound can now add txtInt when formatting the integer section for easier readability
* Added htmlTable css options - they should all start with `htmlTable.`
* `pos.caption` now uses match.arg as expected
* Fixed proper S3 function definition for `htmlTable` with all the arguments
* Added `htmlTable.css.border` style option for allowing to choose border style. Also fixed bug with cgroup empty cells and vertical border.
* Added `htmlTable.pretty_indentation` option for skipping the stripping of all the tabs that was required due to old Pandoc bug.
* Added `attr(x, "html") <- TRUE` by default and UTF-8 encoding on all outputted strings to mimic the `htmltools::HTML` function behavior.
* For simple tibble output the `tidyHtmlTable` can now be used to choose a column for the rnames argument
* The print statement now respects the `chunk_output_type` in Rmd files in RStudio
* `tidyHtmlTable` now accepts table function that allows switching to other table functions
* Added `css.header` style as using `css.cell` wasn't entirely intuitive (fixes issue #73)

Changes for 1.13.3
------------------
* Prepared for R 4.0

Changes for 1.13.1
------------------
* Bug fix for cgroup as list argument
* The `n.tspanner` now also accepts number of `rgroup`s

Changes for 1.13
------------------
* Added the ability to have `cgroup` arguments as a list
* Fixed `n.rgoup` bug with css
* Improved the general vignette
* Added `vector2string` - a convenience function when you want to output a vector into a string
* Added `digits.nonzero` to `txtRound` function that allows more digits with values close to zero
* Force encoding for `print.htmlTable` when outputting a table using the viewer (Issue #61)

Changes for 1.12
----------------
* Added scientific notation to `txtRound` (Issue #35)

Changes for 1.11.4
------------------
* Fix $ MathJax bug (Issue #57)

Changes for 1.11.3
------------------
* Fix single-row `css.cell` bug (Issue #54)

Changes for 1.11.2
------------------
* Set `htmlEscape` to default to `FALSE` as some features depend on the ability to be able to
  send html formatted strings.


Changes for 1.11.1
------------------
* Removed tidyr and dplyr from dependencies (issue #47)

Changes for 1.11.0
------------------
* Strings are now escaped using `htmltools::htmlEscape` - issue #40 (thanks Peter Konings)
* Tidy data interface - issue #42 (thanks Stephen Gragg)

Changes for 1.10.1
-----------------
* Fixed bug with rownames styling (thanks Shira Mitchell)

Changes for 1.10
-----------------
* Added conversion of dimnames into row/column labels
* Added detection of sum row/colum when using `base::table`
* fixed `cgroup` bug with automated `n.cgroup` calculations
* fixed output to viewport when not in RStudio notebook (thanks Peter Konings)
* fixed vector input for `txtRound` warning

Changes for 1.9
-----------------
* `txtInt` handles nsmall warning when working with non-atomic numbers (issue #23)
* fixed output for RStudio notebook (issue #26)

Changes for 1.8
-----------------
* `txtRound` now throws an error when provided a too short vector of digits (thanks Peter Konings)
* `css.cell` has improved docs and added checkmate to verify format (thanks maverickg)
* Added `concatHtmlTables` for merging multiple tables into one string element of class `htmlTable`
* Fixed CRAN bugs in dev version

Changes for 1.7
-----------------
* Added ability to print `matrix` & `data.frame` without any rows, i.e. empty (Thanks Peter Konings)
* Added table border flexibility via the `ctable` argument (Thanks raredd)
* Added option of having row-group separators for no-named row groups (Thanks, prof. Harrell)
* Fixed bug with outputting dates (issue #14)

Changes for 1.6
-----------------
* The `txtRound` now properly handles vector digits argument
* The `txtRound` is now a S3-function and handles `data.frame` objects in a cleaner way

Changes for 1.5
-----------------
* Added better description for how to use the add attribute for `rgroup`s
* Extended the add attribute for `rgroup` to accept matrices
* The `n.rgroup`/`rgroup` are automaticaly completed with the last rows if sum(`n.rgroup`) is less than the total number of rows
* Similar applies to `n.cgroup`/`cgroup`
* Fixed the line-merge so that all new lines get an `<br>`-tag
* Added an `interactiveTable` for allowing tables with cells that have resizeable content
* Added `css.table` for table element css styling

Changes for 1.4
---------------
* Handles `data.frames` with factors - thanks Sergio Oller #4

Changes for 1.3
---------------
* Prepared for API-changes with stringr 1.0
* The txtRound can now handle vectors and single values

Changes for 1.2
-----------------
* Fixed table counter update
* The `htmlTable` can now also accept vectors
* Removed the `format.df` from Hmisc as it converted & to \& with unexpected results. This functionality
  has also been superseeded by the txtRound function.

Changes for 1.1
-----------------
* Added the option of having an attribute on the `rgroup` in case there is an interest
  of adding more data to that particular row
* Added a fix for the pandoc tab bug
* `knit_print` implemented removing the need for results='asis' except for within for-loops
* Removed the capitalize tspanner css as this may cause confusion with limited word processor compatibility
* Added `htmlTable` tests
* `txtRound` now also rounds character matrices
* Added a detailed vignette with the primary features of `htmlTable`
* Added the option of having a total row
* The `pos.caption` can now also be "below"
* Fixed minor bug with numbering not beeing turned off with options(table_counter = FALSE)
* Zebra striping now works for `rgroup`s mixed with ""
* `txtRound` returns "" by default if value missing. This can also be specified with the
  `txt.NA` option

Changes for 1.0
-----------------
* The `htmlTable` and associated txt-functions are now separated from Gmisc
* Argument name changes for `htmlTable` for better consistency and logic:
    `rowname` -> `rnames`
    `headings` -> `header`
    `halign` -> `align.header`
    `cgroup.just` -> `align.cgroup`
    `rgroupCSSstyle` -> `css.rgroup`
    `rgroupCSSseparator` -> `css.rgroup.sep`
    `tspannerCSSstyle` -> `css.tspanner`
    `tspannerCSSseparator` -> `css.tspanner.sep`
    `tableCSSclass` -> `css.table.class`
    `rowlabel.pos` -> `pos.rowlabel`
    `caption.loc` -> `pos.caption`
    `altcol` -> `col.rgroup`
* `htmlTable` can now handle `rnames = FALSE` in order to surpress rownames
* `htmlTable` now defaults to the layout of `ctable` as this is the more commonly found layout among medical papers
* `htmlTable` `rgroup` has the additional `padding.rgroup` for those that want to change the no-breaking space padding
* `htmlTable` `tfoot` is automatically run through `txtMergeLines` in order to retain wrapped text
* Renamed `splitLines4Table` to `txtMergeLines`, `outputInt` to `txtInt`, `pvalueFormatter` to `txtPval` and these follow now the argument style of `htmlTable`
* Added `txtRound` for rounding matrices. The problem with `round()` is that 1.01 rounds to 1
  instead of "1.0" that is wanted for output.
* Multiple bug-fixes
