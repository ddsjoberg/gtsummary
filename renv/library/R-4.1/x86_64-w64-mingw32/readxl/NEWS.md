# readxl 1.3.1

Pragmatic patch release to update some tests in advance of v2.1.0 of the tibble package. That release updates name repair: standard suffix becomes `...j`, instead of `..j`, partially motivated by user experience in readxl.

# readxl 1.3.0

## Dependency changes

readxl embeds libxls v1.5.0. This is the first *official* release of libxls in several years, although readxl has been tracking the development version in the interim. The libxls project is now officially hosted at <https://github.com/libxls/libxls>. In particular, libxls v1.5.0 addresses these two CVEs:

  * [CVE-2018-20452](http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2018-20452)
  * [CVE-2018-20450](http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2018-20450)

# readxl 1.2.0

## Column name repair

readxl exposes the `.name_repair` argument that is [coming to version 2.0.0 of the tibble package](https://www.tidyverse.org/articles/2018/11/tibble-2.0.0-pre-announce/). The readxl default is `.name_repair = "unique"`, keeping with the readxl convention to ensure column names are neither missing nor duplicated.

* [Column Names](https://readxl.tidyverse.org/articles/articles/column-names.html) is a new article about this feature.
* readxl delegates name repair to tibble, therefore the installed tibble version determines how names are repaired.
* If tibble >= v2.0.0, the full power of `.name_repair` is available, defaulting to `.name_repair = "unique"`. Otherwise, the legacy function `tibble::repair_names(prefix = "X", sep = "__")` is used, replicating the behaviour of readxl v1.1.0.
  - Consider a spreadsheet with three columns: one unnamed and two named `x`.
  - Content of cells in Excel: `""`, `x`, `x` 
  - New style column names: `..1`, `x..2`, `x..3`
  - Legacy column names: `X__1`, `x`, `x__1`
* Once per session, readxl emits a message stating that it works best with tibble >= v2.0.0. It is anticipated that this will become a hard minimum version requirement in a future version of readxl.
  
## Other changes

* `read_excel()` and friends gain a `progress` argument that controls a progress spinner (#243, #538).

* `read_xls()` and `read_xlsx()` pass the `trim_ws` argument along (#514).

* readxl has a [new article](https://readxl.tidyverse.org/articles/articles/multiple-header-rows.html) on reading Excel files with multiple header rows (#486, #492  @apreshill).

* xlsx files that do not have a "styles" part can now be read (#505, #506 @jt6)

* All paths are passed through `normalizePath()` (#498, #499, new behaviour for xlsx but not xls) and `enc2native()` (#370).
  
## Dependency changes

readxl is now tested back to R >= 3.1.

Embedded libxls has been updated, using the source in <https://github.com/evanmiller/libxls>. readxl's DESCRIPTION now records the SHA associated to the embedded libxls in a `Note`.

# readxl 1.1.0

* `read_excel()` and `excel_sheets()` associate a larger set of file extensions with xlsx and are better able to guess the format of a file with a nonstandard or missing extension. This is about deciding whether to treat a file as xls or xlsx. (#342, #411, #457)

  - `excel_format()` is the newly-exported format-guessing function.
  - `format_from_ext()` is a low-level helper, also exported, that only consults file extension. In addition to the obvious interpretation of `.xls` and `.xlsx`, the extensions `.xlsm`, `.xltx`, and `.xltm` are now associated with xlsx.
  - `format_from_signature()` is a low-level helper, also exported, that consults the file's signature (a.k.a. magic number). It's handy for files that lack an extension.

* Embedded libxls has been updated to address security vulnerabilitities identified in late 2017 (#441, #442).

  - [CVE-2017-12110](https://www.talosintelligence.com/vulnerability_reports/TALOS-2017-0462), [CVE-2017-2896](https://www.talosintelligence.com/vulnerability_reports/TALOS-2017-0403), and [CVE-2017-2897](https://www.talosintelligence.com/vulnerability_reports/TALOS-2017-0404) were demonstrated to affect readxl v1.0.0. These have been addressed in libxls and the embedded version of libxls incorporates those fixes.
  - Although [CVE-2017-12111](https://www.talosintelligence.com/vulnerability_reports/TALOS-2017-0463) and [CVE-2017-2919](https://www.talosintelligence.com/vulnerability_reports/TALOS-2017-0426) mention readxl, the notices clarify that these CVEs do not actually affect readxl. Both have been fixed in libxls for quite a while: CVE-2017-12111 since 2014 and CVE-2017-2919 since 2012.

* xlsx structured as a "minimal conformant SpreadsheetML package" can be read. Most obvious feature of such sheets is the lack of an `xl/` directory in the unzipped form. (xlsx, #435, #437)

* Reading xls sheet with exactly 65,536 rows no longer enters an infinite loop. (xls, #373, #416, #432 @vkapartzianis)

* Doubles, including datetimes, coerced to character from xls now have much higher precision, comparable to the xlsx behaviour. (xls, #430, #431)

* Integer-y numbers larger than 2^31 are coerced properly to string (xls, #346)

* Shared strings are only compared to NA strings after lookup, never on the basis of their index. (xlsx, #401)

* Better checks and messaging around nonexistent files. (#392)

* Add `$(C_VISIBILITY)` to compiler flags to hide internal symbols from the dll. (#385 @jeroen)

* Numeric data in a logical column now coerces properly to logical. (xlsx, #385 @nacnudus)

# readxl 1.0.0

## Sheet geometry

* `range` is a new argument for reading a rectangular range, possibly open. (#314, #8)

* `n_max` is a new argument that limits the number of data rows read. (#306, #281)

* Empty cells, rows, columns (xlsx #248 and #240, xls #271): Cells with no content are no longer loaded, even if they appear in the file. Affects cells that have no data but that carry explicit formatting, detectable in Excel as seemingly empty cells with a format other than "General". Such cells may still exist in the returned tibble, with value `NA`, depending on the sheet geometry.
  
    * Eliminates a source of trailing rows (#203) and columns (#236, #162, #146) consisting entirely of `NA`.
    * Eliminates a subtle source of disagreement between user-provided column names and guessed column types (#169, #81).
    * Embedded empty columns are no longer automatically dropped, regardless of whether there is a column name. (#157, #261)
    * Worksheets that are completely empty or that contain only column names no longer error, but return a tibble with zero rows. (#222, #144, #65)
    * Improved handling of leading and embedded blank rows and explicit row skipping. (#224, #194, #178, #156, #101)

* User-supplied `col_names` are processed relative to user-supplied `col_types`, if given. Specifically, `col_names` is considered valid if it has the same length as `col_types`, before *or after* removing skipped columns. (#81, #261)

## Column types and coercion

* `"list"` is a new accepted value for `col_types`. Loads data as a list of length-1 vectors, that are typed using the logic from `col_types = NULL`, but on a cell-by-cell basis (#262 @gergness).

* `"logical"` is a new accepted value for `col_types`. When `col_types = NULL`, it is the guessed type for cells Excel advertises as Boolean. When a column has no data, it is now filled with logical `NA`. (#277, #270)

* `"guess"` is a new accepted value for `col_types`. Allows the user to specify some column types, while allowing others to be guessed (#286)

* A user-specified `col_types` of length one will be replicated to have length equal to the number of columns. (#127, #114, #261)

* `"blank"` has been deprecated in favor of the more descriptive and readr-compatible `"skip"`, which is now the preferred way to request that a column be skipped. (#260, #193, #261)

* `guess_max` is a new argument that lets user adjust the number of rows used to guess column types. (#223, #257 @tklebel and @jennybc)

* `trim_ws` is a new argument to remove leading and trailing whitespace. It defaults to `TRUE`. (#326, #211)

* `na` can now hold multiple NA values, e.g., `read_excel("missing-values.xls", na = c("NA", "1"))`. (#13, #56, @jmarshallnz)

* Coercions and cell data:

  - Numeric data that appears in a date column is coerced to a date. Throws a warning. (#277, #266)
  - Dates that appear in a numeric column are converted to `NA` instead of their integer representation. Throws warning. (#277, #263)
  - "Number stored as text": when a text cell is found in a numeric column, `read_excel()` attempts to coerce the string to numeric and falls back to `NA` if unsuccessful. Throws warning. (#277, #217, #106)
  - Cells in error are treated as blank and are imported as `NA` (instead of the string `"error"`). (#277, #62)
  - BoolErr cells are now handled in xls. Suppresses message `"Unknown type: 517"`. (#274, #259)
  - Dates that arise from a formula are treated as dates (vs. numeric) in xls. (#277)
  - Dates in .xlsx files saved with LibreOffice are now recognized as such. (#134, @zeehio)

## Compatibility

Many 3rd party tools write xls and xlsx that comply with the spec, but that are quite different from files produced by Excel.

  * Namespace prefixes are now stripped from element names and attributes when parsing XML from xlsx. Workaround for the creative approach taken in some other s/w, coupled with rapidxml's lack of namespace support. (#295, #268, #202, #80)

  * Excel mixes 0- and 1-indexing in reported row and column dimensions for xls and libxls expects that. Other s/w may index from 0 for both, preventing libxls from reading the last column. Patched to restore access to those cells. (#273, #180, #152, #99)

  * More robust logic for sheet lookup in xlsx. Improves compatibility with xlsx written by a variety of tools and/or xlsx containing chartsheets. (#233, #104, #200, #168, #116, @jimhester and @jennybc)

  * The `numFmtId` attribute is no longer accessed when it does not exist (xlsx written by <http://epplus.codeplex.com>). (#191, #229)

  * Location is inferred for cells that do not declare their location (xlsx written by JMP). (#240, #163, #102)

## Other

* `read_xls()` and `read_xlsx()` are now exposed, such that files without an `.xls` or `.xlsx` extension can be read. (#85, @jirkalewandowski)

* The [Lotus 1-2-3 leap year bug](https://support.microsoft.com/en-us/help/214326/excel-incorrectly-assumes-that-the-year-1900-is-a-leap-year) is now accounted for, i.e. date-times prior to March 1, 1900 import correctly. Date-times on the non-existent leap day February 29, 1900 import as NA and throw a warning. (#264, #148, #292)

* The tibble package is now imported (#175, @krlmlr) and `tibble::repair_names()` is used to prevent empty, `NA`, or duplicated column names. (#216, #208, #199 #182, #53, #247).

* Default column names for xlsx now start with X__1 instead of X__0. (#98, @zeehio, @krlmlr)

* Fix compilation warnings/failures (FreeBSD 10.3 #221, gcc 4.9.3 #124) and/or problems reading xls (CentOS 6.6 #189). (#244, #245, #246 @jeroen)

* Unwanted printed output (e.g., `DEFINEDNAME: 21 00 00 ...`) is suppressed when reading xls that contains a defined range. (#82, #188, @PedramNavid)

# readxl 0.1.1

* Add support for correctly reading strings in .xlsx files containing escaped 
  unicode characters (e.g. `_x005F_`). (#51, @jmarshallnz)
