# development

## Fixes

* `write.xlsx()` now successfully passes `withFilter` ([#151](https://github.com/ycphs/openxlsx/issues/151))
* code clean up PR [#168](https://github.com/ycphs/openxlsx/pull/168)
* removal of unused variables PR [#168](https://github.com/ycphs/openxlsx/pull/168)

## New features

* adds `buildWorkbook()` to generate a `Workbook` object from a (named) list or a data.frame ([#192](https://github.com/ycphs/openxlsx/issues/192), [#187](https://github.com/ycphs/openxlsx/issues/187))
  * this is now recommended rather than the `write.xlsx(x, file) ; wb <- read.xlsx(file)` functionality before
  * `write.xlsx()` is now a wrapper for `wb <- buildWorkbook(x); saveWorkbook(x, file)`
  * parameter checking from `write.xlsx()` >> `buildWorkbook()` are now held off until passed to `writeData()`, `writeDataTable()`, etc
  * `row.names` is now deprecated for `writeData()` and `writeDataTable()`; please use `rowNames` instead
* `read.xlsx()` now checks for the file extension `.xlsx`; previously it would throw an error when the file was `.xls` or `.xlm` files
* memory allocation improvements
* global options added for `minWidth` and `maxWidth`
* `write.xlsx()` >> `buildWorkbook()` can now handle `colWidths` passed as either a single element or a `list()`
* Added ability to change positioning of summary columns and rows.
  * These can be set with the `summaryCol` and `summaryRow` arguments in `pageSetup()`.
* `activeSheet` allows to set and get the active (displayed) sheet of a worbook.
* Adds new global options for workbook formatting ([#165](https://github.com/ycphs/openxlsx/issues/165); see `?op.openxlsx`)




# openxlsx 4.2.3
 
## New Features

* Most of functions in openxlsx now support non-ASCII arguments better. More specifically, we can use non-ASCII strings as names or contents for `createNamedRegion()` ([#103](https://github.com/ycphs/openxlsx/issues/103)), `writeComment()`, `writeData()`, `writeDataTable()` and `writeFormula()`. In addition, openxlsx now reads comments and region names that contain non-ASCII strings correctly on Windows. Thanks to @shrektan for the PR [#118](https://github.com/ycphs/openxlsx/pull/118).

* `setColWidths()` now supports zero-length `cols`, which is convinient when `cols` is dynamically provided [#128](https://github.com/ycphs/openxlsx/issues/128). Thanks to @shrektan for the feature request and the PR.
 
## Fixes for Check issues
 
* Fix to pass the tests for link-time optimization type mismatches

* Fix to pass the checks of native code (C/C++) based on static code analysis

## Bug Fixes

* Grouping columns after setting widths no longer throws an error ([#100](https://github.com/ycphs/openxlsx/issues/100))

* Fix inability to save workbook more than once ([#106](https://github.com/ycphs/openxlsx/issues/106))

* Fix `loadWorkbook()` sometimes importing incorrect column attributes

# openxlsx 4.2.2

## New Features

* Added features for `conditionalFormatting` to support also 'contains not', 'begins with' and 'ends with'

* Added return value for `saveWorkbook()` the default value for `returnValue` is `FALSE` ([#71](https://github.com/ycphs/openxlsx/issues/71))

* Added Tests for new parameter of `saveWorkbook()`

## Bug Fixes 
 
* Solved CRAN check errors based on the change disussed in [PR#17277](https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17277)

# openxlsx 4.2.0

## New Features

* Added `groupColumns()`, `groupRows()`, `ungroupColumns()`, and `ungroupRows()` to group/ugroup columns/rows ([#32](https://github.com/ycphs/openxlsx/issues/32))

## Bug Fixes 

* Allow xml-sensitve characters in sheetnames ([#78](https://github.com/ycphs/openxlsx/issues/78))

## Internal

* Updated roxygen2 to 7.1.1

# openxlsx 4.1.5.1

## Bug Fixes

*  fixed issue [#68](https://github.com/ycphs/openxlsx/issues/68])

# openxlsx 4.1.5

## New Features

*  Add functions to get and set the creator of the xlsx file

*  add function to set the name of the user who last modified the xlsx file

## Bug Fixes

*  Fixed NEWS hyperlink

*  Fixed writing of mixed EST/EDT datetimes

*  Added description for `writeFormula()` to use only english function names

*  Fixed validateSheet for special characters

## Internal

*  applied the tidyverse-style to the package `styler::style_pkg()`

*  include tests for `cloneWorksheet`

# openxlsx 4.1.4

## New Features

*  Added `getCellRefs()` as function. [#7](https://github.com/ycphs/openxlsx/issues/7)

*  Added parameter for customizing na.strings

## Bug Fixes

*  Use `zip::zipr()` instead of `zip::zip()`.

*  Keep correct visibility option for loadWorkbook. [#12](https://github.com/ycphs/openxlsx/issues/12])

*  Add space surrounding "wrapText" [#17](https://github.com/ycphs/openxlsx/issues/17)

*  Corrected Percentage, Accounting, Comma, Currency class on column level

## Internal

*  update to rogygen2 7.0.0

# openxlsx 4.1.3

## New Features

*  Added a `NEWS.md` file to track changes to the package.
*  Added `pkgdown` to create site.

## Bug Fixes

*  Return values for cpp changed to R_NilValue for r-devel tests

*  Added empty lines at the end of files

# openxlsx 4.1.2

*  Changed maintainer

# openxlsx 4.1.1

## New Features

*  `sep.names` allows choose other separator than '.' for variable names with a blank inside

*  Improve handling of non-region names in `getNamedRegions` and add related test

# openxlsx 4.1.0

## New Features

*  `deleteNamedRegions` to delete named region and optionally the worksheet data

*  set Workbook properties 'title', 'subject', 'category' 

## Bug Fixes

*  `pageSetup` fails when passing in sheet by name

*  matching sheet names with special characters now works 

*  `skipEmptyCols` being ignored by `read.xlsx.Workbook`

*  zero column data.frames would throw an error.

*  `read.xlsx` on files created using apache poi failed to match sheet name to xml file. 

*  deleted table re-appearing after save & load.

*  newline characters in table names would corrupt file

*  datetime precision

# openxlsx 4.0.17

## New Features

*  `getNamedRegions` returns sheet name and cell references along with the named regions.

*  `borderStyle` and `borderColour` can be vector to specify different values for each side

*  `dataValidation` type "list"

*  `dataBar showValue`, gradient and border can now be set through conditionalFormatting()

*  options("openxlsx.zipflags") to pass additional flags to zip application e.g. compression level

*  `getTables()` and `removeTable()` to show and remove Excel table objects 

*  set column to 'hidden' with `setColWidths()`

## Bug Fixes

*  `skipEmptyRows` & `skipEmptyCols` was being ignored by `read.xlsx`

*  date detection basic_string error

*  multiple spaces in table column names were not being maintained thus corrupting the xlsx file.

*  openXL fail silently on relative paths

*  `headerStyle` failed when writing a list of length 1 using `write.xlsx`

*  `detectDate` for `read.xlsx` issues

*  some Excel column types causing existing styling to be removed

*  `na.strings` no longer ignored for `read.xlsx.Workbook`

*  partial dollar matches on 'font' and 'fill' fixed

*  maintain hidden columns and their custom widths in `loadWorkbook()`

*  overwriting cells with borders sometimes removed the border styling

# openxlsx 4.0.0

## New Features

*  Reduced RAM usage and improved performance

*  maintain vbaProject, slicers, pivotTables on load

*  Read and load from URL

## Bug Fixes

*  Fix date time conversion accuracy issues. 

*  Allow multibyte characters in names and comments.

*  Remove `tolower()` over style number formats to allow uppercase cell formatting

*  Stacking styles fixed.

# openxlsx 3.0.2

## New Features

*  "between" type for conditional formatting values in some interval.

*  `colWidths` parameter added to `write.xlsx` for auto column widths.

*  `freezePane` parameter handling added to `write.xlsx`.

*  `visible` parameter to `addWorksheet` to hide worksheets.

*  `sheetVisible` function to get and assign worksheet visibility state "hidden"/"visible"

*  `pageBreak` function to add page breaks to worksheets.

## Bug Fixes

*  `keepNA` parameter added to `write.xlsx`. Passed to `writeData`/`writeDataTable`

# openxlsx 3.0.1

## New Features

*  improved performance of `read.xlsx` and `loadWorkbook`

*  `writeFormula` funciton added to write cell formulas. Also columns
  with class "formula" are written as cell formulas similar how column
  classes determine cell styling

*  Functionality to write comments and maintain comments with `loadWorkbook`

*  `check.names` argument added `read.xlsx` to make syntactically valid variable names

*  `loadWorkbook` maintains cell indents

*  `namedRegion` parameter added to `read.xlsx` to read a named region.

*  `getNamed` regions to return names of named regions in a workbook

*  `getSheetNames` to get worksheet names within an xlsx file.

## Bug Fixes

*  `convertToDateTime` now handles NA values

*  `read.xlsx` rows bug fixed where non-consecutive cells were skipped.

*  `convertToDate` & `convertToDateTime` now handle NA values.

*  out of bounds worksheet fixed for libre office xlsx files.

*  `loadWorkbook` now maintains `chartSheets `

# openxlsx 2.4.0

## New Features

*  stackable cell styling

*  `getDateOrigin` function to return the date origin used internally by the xlsx file to pass to
  `convertToDate`
  
*  Auto-detection of date cells. Cells that "look" like dates will be converted to dates when reading from file.

*  `read.xlsx.Workbook` to read from workbook objects

*  `colIndex`, `rowIndex` added to `read.xlsx` to only read specified rows and columns

*  Excel slicers now maintained by `loadWorkbook`

*  fill styles extended to support `gradientFill`

## Bug Fixes

*  Encoding fixed and multi-byte characters now supported.

*  `read.xlsx` now maintains multiple consecutive spaces and newline characters.

*  `convertToDate` & `convertToDateTime` now handle NA values.

*  multiple selected worksheet issue whioch preventing adding of new worksheets in Excel.

*  `zoom` parameter now limited to [10, 400] and documentation updated.

*  `write.xlsx` colnames parameter being assigned to rownames

*  Handling of NaN and Inf values in `writeData`

# openxlsx 2.1.3

## New Features

*  `conditionalFormatting` type "databar"

*  `asTable` parameter to `write.xlsx` to writing using `writeDataTable`.

*  extended `numFmt` formatting to numeric rounding also added option("openxlsx.numFmt" = ...)
 for default number formatting of numeric columns

*  additional `numFmt` "comma" to format numerics with "," thousands separator 

*  `tableName` parameter to `writeDataTable` to assign the table a name

*  `headerStyle` parameter to `writeDataTable` for additional column names styling

*  `textRotation` parameter to `createStyle` to rotate cell text

*  functions `addFilter` & `removeFilter` to add filters to columns

*  Headers & footers extended, can now be set with `addWorksheet` and `setHeaderFooter`.
  `setHeader` & `setFooter` deprecated.  

*  "fitToWidth" and "fitToHeight" logicals in `pageSetup`.

*  "zoom" parameter in addWorksheet to set worksheet zoom level.

*  "withFilter"" parameter to writeDataTable and writeData to remove table filters

*  `keepNa` parameter to `writeDataTable` and `writeData` to write NA values as #N/A

*  auto column widths can now be set with width = "auto"

## VIGNETTE

*  section on `write.xlsx` in Introductory vignette

## Bug Fixes

*  Fix reading in of apostrophes

*  Styling blank cells no longer corrupts workbooks

*  `read.xlsx` now correctly reads `sharedStrings` with inline styling

*  `sharedStrings` now exact matches true/false to determine logical values from workbooks.

*  fomulas in column caused openxlsx to crash. This has been fixed.

# openxlsx 2.0.15

## New Features

*  `writeData` now style based on column class the same as `writeDataTable`

*  Vignette "Formatting" for examples focussed on formatting

*  Customizable date formatting with `createStyle` and also through option("openxlsx.dateFormat" = ...)

*  Customizable POSIX formatting with `createStyle` and also through option("openxlsx.datetimeFormat" = ...)

*  Generalised `conditionalFormat` function to complex expressions and color scales.

*  `writeData` border type "all" to draw all borders and maintain column styling.

*  Deprecated "sheets" and replaced with "names" function

*  column class "scientific" to automatically style as scientific numbers

*  `writeData` now handles additional object classes: coxph, cox.zph, summary.coxph1 from Survival package

## Bug Fixes

*  Invalid XML characters in hyperlinks now replaced.

*  Encoding issues when writing data read in with `read.xlsx`

*  scientific notation resulting in corrupt workbooks fix

*  Multiple saves of Workbooks containing conditional formatting were corrupt.

*  Latin1 characters now write correctly.

*  logicals written as 0/1 instead of TRUE/FALSE

# openxlsx 2.0.1

## New Features

*  `write.xlsx` function to write data directly to file via the `writeData` function
 with basic cell styling.

*  `writeDataTable` now styles columns of class 'Date', 'POSIXct', 'POSIXt', 'currency', 'accounting', 'percentage'
 as Excel formats Date, Date, Date, Currency, Accounting, Percentage respectively.

*  Data of class 'Date', 'POSIXct', 'POSIXt', 'currency', 'accounting' are converted to integers
 upon writing (as opposed to characters).

*  `writeDataTable` converts columns of class 'hyperlink' to hyperlinks.

*  logicals are converted to Excel booleans

*  hyperlinks in loaded workbooks are now maintained

*  `borderStyle` argument to `createStyle` to modify border line type.

*  `borderStyle` argument to `writeData` to modify border line type.

*  "worksheetOrder" function to shuffle order of worksheets when writing to file

*  `openXL` function to open an excel file or Workbook object

## Bug Fixes

*  conversion of numeric data to integer in `read.xlsx` fixed.

*  `readWorkbook`/`read.xlsx` should work now. Empty values are 
  now padded with NA. Many other bugs fixed.

*  borders on single row and/or column data.frames now work.

*  `readWorkbook`/`read.xlsx` check for TRUE/FALSE values is now case-insensitive.

*  sheet names containing invalid xml charcters (&, <, >, ', ") now work when referencing
  by name and will not result in a corrupt workbook.

*  sheet names containing non-local characters can now be referenced by name.

*  Invalid factor level when missing values in `writeData`

*  `saveWorkbook` now accepts relative paths.

*  Non-local character encoding issues.

*  errors in vignette examples.

*  numbers with > 8 digits were rounded in `writeData`
