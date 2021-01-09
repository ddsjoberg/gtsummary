# readr 1.4.0

## Breaking changes

* `write_*()` functions first argument is now `file` instead of `path`, for consistency with the `read_*()` functions.
  `path` has been deprecated and will be removed in a future version of readr (#1110, @brianrice2)

* `write_*()` functions now output any NaN values in the same way as NA values, controlled by the `na=` argument. (#1082).

## New features

* It is now possible to generate a column specification from any tibble (or data.frame) with `as.col_spec()` and convert any column specification to a short representation with `as.character()`

      s <- as.col_spec(iris)
      s
      #> cols(
      #>   Sepal.Length = col_double(),
      #>   Sepal.Width = col_double(),
      #>   Petal.Length = col_double(),
      #>   Petal.Width = col_double(),
      #>   Species = col_factor(levels = c("setosa", "versicolor", "virginica"), ordered = FALSE, include_na = FALSE)
      #> )
      as.character(s)
      #> [1] "ddddf"

* The cli package is now used for all messages.

* The runtime performance for tables with an extreme number of columns is greatly improved (#825)

* Compressed files are now detected by magic numbers rather than by the file extension (#1125)

* A memory leak when reading files is now fixed (#1092)

* `write_*()` functions gain a `eol =` argument to control the end of line character used (#857).
  This allows writing of CSV files with Windows newlines (CRLF) if desired.

* The Rcpp dependency has been removed in favor of cpp11.

* The build system has been greatly simplified so should work on more systems.

## Additional features and fixes

* The full problem field is now displayed in the problems tibble, as intended (#444).

* New `%h` placeholder for parsing unrestricted hours (<0 and >23) to support parsing durations (#549, @krlmlr).

* `as.character.col_spec()` now handles logical columns as well (#1127)

* `fwf_positions(end)` no longer has a default argument and must be specified (#996)

* `guess_parser()` gains a `na` argument and removes NA values before guessing (#1041).

* `parse_guess()` now passes the `na` argument to `guess_parser()` 

* `read_*` functions now close properly all connections, including on errors like HTTP errors when reading from a url (@cderv, #1050).

* `read_delimited()` no longer mistakenly stats literal filenames (#1063)

* `read_lines()` now ignores quotations when skipping lines (#991).

* `read_lines(skip_empty_rows = TRUE)` no longer crashes if a file ends with an empty line (#968)

* `write_*()` functions now invisibly return the input data frame unchanged, rather than a version with factors and dates converted to strings. (@jesse-ross, #975). 

* `write_csv2()` now formats decimal numbers more consistently with `utils::write.csv2()` (#1087)

* `write_csv2()` and `format_csv2()` no longer pad number columns with whitespaces (@keesdeschepper, #1046).

* `write_excel_csv()` no longer outputs a byte order mark when appending to a file (#1075).

* Uses of `tibble::data_frame` updated to `tibble::tibble` ([tidyverse/dplyr#4069](https://github.com/tidyverse/dplyr/issues/4069), @thays42, #1124, @brianrice2)

* `read_delimited()` now returns an empty `tibble::data_frame()` rather than signaling an error when given a connection with an empty file (@pralitp, #963).

* More helpful error when trying to write out data frames with list columns (@ellessenne, #938)

* `type_convert()` removes a 'spec' attribute, because the current columns likely have modified data types.  The 'spec' attribute is set by functions like `read_delim()` (@jimhester, @wibeasley, #1032).

* `write_rds()` now can specify the Rds version to use. The default value is 2 as it's compatible to R versions prior to 3.5.0 (@shrektan, #1001).

# readr 1.3.1

* Column specifications are now coloured when printed. This makes it easy to
  see at a glance when a column is input as a different type then the rest.
  Colouring can be disabled by setting `options(crayon.enabled = FALSE)`.

* `as.col_spec()` can now use named character vectors, which makes
  `read_csv("file.csv", col_types = c(xyz = "c"))` equivalent to
  `read_csv("file.csv", col_types = cols(xyz = col_character())`

* Fix skipping when single quotes are embedded in double quoted strings, and
  single quotes in skipped or commented lines (#944, #945).

* Fix for compilation using custom architectures on macOS (#919)

* Fix for valgrind errors (#941)

# readr 1.3.0

## Breaking Changes

### Blank line skipping

readr's blank line skipping has been modified to be more consistent and to
avoid edge cases that affected the behavior in 1.2.0. The skip parameter now
behaves more similar to how it worked previous to readr 1.2.0, but in addition
the parameter `skip_blank_rows` can be used to control if fully blank lines are
skipped. (#923)

### tibble data frame subclass

readr 1.3.0 returns results with a `spec_tbl_df` subclass. This differs from a
regular tibble only that the `spec` attribute (which holds the column
specification) is lost as soon as the object is subset (and a normal `tbl_df`
object is returned).

Historically `tbl_df`'s lost their attributes once they were subset. However
recent versions of tibble retain the attributes when subetting, so the
`spec_tbl_df` subclass is needed to ensure the previous behavior.

This should only break compatibility if you are explicitly checking the class
of the returned object. A way to get backwards compatible behavior is to
call subset with no arguments on your object, e.g. `x[]`.

## Bugfixes

* `hms` objects with NA values are now written without whitespace padding (#930).
* `read_*()` functions now return `spec_tbl_df` objects, which differ from
  regular `tbl_df` objects only in that the `spec` attribute is removed (and
  they are demoted to regular `tbl_df` objects) as soon as they are subset
  (#934).
* `write_csv2()` now properly respects the `na` argument (#928)
* Fixes compilation with multiple architectures on linux (#922).
* Fixes compilation with R < 3.3.0

# readr 1.2.1

This release skips the clipboard tests on CRAN servers

# readr 1.2.0

## Breaking Changes

### Integer column guessing

readr functions no longer guess columns are of type integer, instead these
columns are guessed as numeric. Because R uses 32 bit integers and 64 bit
doubles all integers can be stored in doubles, guaranteeing no loss of
information. This change was made to remove errors when numeric columns were
incorrectly guessed as integers. If you know a certain column is an integer and
would like to read them as such you can do so by specifying the column type
explicitly with the `col_types` argument.

### Blank line skipping

readr now always skips blank lines automatically when parsing, which may change
the number of lines you need to pass to the `skip` parameter. For instance if
your file had a one blank line then two more lines you want to skip previously
you would pass `skip = 3`, now you only need to pass `skip = 2`.

## New features

### Melt functions

There is now a family of `melt_*()` functions in readr. These functions store
data in 'long' or 'melted' form, where each row corresponds to a single
value in the dataset. This form is useful when your data is ragged and not
rectangular.

``` r
data <-"a,b,c
1,2
w,x,y,z"

readr::melt_csv(data)
#> # A tibble: 9 x 4
#>     row   col data_type value
#>   <dbl> <dbl> <chr>     <chr>
#> 1     1     1 character a    
#> 2     1     2 character b    
#> 3     1     3 character c    
#> 4     2     1 integer   1    
#> 5     2     2 integer   2    
#> 6     3     1 character w    
#> 7     3     2 character x    
#> 8     3     3 character y    
#> 9     3     4 character z
```

Thanks to Duncan Garmonsway (@nacnudus) for great work on the idea an
implementation of the `melt_*()` functions!

### Connection improvements

readr 1.2.0 changes how R connections are parsed by readr.
In previous versions of readr the connections were read into an in-memory raw vector, then passed to the readr functions.
This made reading connections from small to medium datasets fast, but also meant that the dataset had to fit into memory at least twice (once for the raw data, once for the parsed data).
It also meant that reading could not begin until the full vector was read through the connection.

Now we instead write the connection to a temporary file (in the R temporary directory), than parse that temporary file.
This means connections may take a little longer to be read, but also means they will no longer need to fit into memory.
It also allows the use of the chunked readers to process the data in parts.

Future improvements to readr would allow it to parse data from connections in a streaming fashion, which would avoid many of the drawbacks of either method.

### Additional new features

* `melt_*()` functions added for reading ragged data (#760, @nacnudus).
* `AccumulateCallback` R6 class added to provide an example of accumulating values in a single result (#689, @blakeboswell).
* `read_fwf()` can now accept overlapping field specifications (#692, @gergness)
* `type_convert()` now allows character column specifications and also silently
  skips non-character columns (#369, #699)
* The `parse_*()` functions and `read_fwf()` gain a `trim_ws` argument to
  control whether the fields should be trimmed before parsing (#636, #735).
* `parse_number()` now parses numbers in scientific notation using `e` and `E`
  (#684, @sambrady3).
* Add `write_excel_csv2()` function to allow writing csv files with comma as a
  decimal separator and semicolon as a column separator (#753, @olgamie).
* `read_*()` files now support reading from the clipboard by using `clipboard()` (#656).
* `write_file()` gains a `sep` argument, to specify the line separator (#665).
* Allow files to be read via FTP over SSH by recognising `sftp` as a URL protocol (#707, @jdeboer).
* `parse_date*() accepts `%a` for local day of week (#763, @tigertoes).
* Added function `read_lines_raw_chunked()` (#710, @gergness)
* `write_csv2()` added to complement `write_excel_csv2()` and allow writing csv file readable by `read_csv2()`
  (#870, @cderv).
* `as.col_spec()` is now exported (#517).
* `write*()` functions gain a `quote_escape` argument to control how quotes are escaped in the output (#854).
* `read*()` functions now have a more informative error when trying to read a remote bz2 file (#891).
* `spec_table2()` function added to correspond to `read_table2()` (#778, @mawds).
* `parse_factor()` now has `levels = NULL` by default (#862, @mikmart).
* `"f"` can now be used as a shortcode for `col_factor()` in `cols()` and the
  `col_types` argument to `read_delim()` and friends (#810, @mikmart).
* Functions now read connections to a temporary file rather than to an in-memory object (#610, #76).

## Bug Fixes

* `standardise_path()` now uses a case-insensitive comparison for the file extensions (#794).
* `parse_guess()` now guesses logical types when given (lowercase) 'true' and 'false' inputs (#818).
* `read_*()` now do not print a progress bar when running inside a RStudio notebook chunk (#793)
* `read_table2()` now skips comments anywhere in the file (#908).
* `parse_factor()` now handles the case of empty strings separately, so you can
  have a factor level that is an empty string (#864).
* `read_delim()` now correctly reads quoted headers with embedded newlines (#784).
* `fwf_positions()` now always returns `col_names` as a character (#797).
* `format_*()` now explicitly marks it's output encoding as UTF-8 (#697).
* `read_delim()` now ignores whitespace between the delimiter and quoted fields (#668).
* `read_table2()` now properly ignores blank lines at the end of a file like
  `read_table()` and `read_delim()` (#657).
* `read_delim()`, `read_table()` and `read_table()` now skip blank lines at the
  start of a file (#680, #747).
* `guess_parser()` now guesses a logical type for columns which are all
  missing. This is useful when binding multiple files together where some files
  have missing columns. (#662).
* Column guessing will now never guess an integer type. This avoids issues
  where double columns are incorrectly guessed as integers if they have only
  integer values in the first 1000 (#645, #652).
* `read_*()` now converts string `file`s to UTF-8 before parsing, which is convenient for non-UTF-8 platforms
  in most cases (#730, @yutannihilation).
* `write_csv()` writes integers up to 10^15 without scientific notation (#765, @zeehio)
* `read_*()` no longer throws a "length of NULL cannot be changed" warning when
  trying to resize a skipped column (#750, #833).
* `read_*()` now handles non-ASCII paths properly with R >=3.5.0 on Windows (#838, @yutannihilation).
* `read*()`'s `trim_ws` parameter now trims both spaces and tabs (#767)

# readr 1.1.1

* Point release for test compatibility with tibble v1.3.1.
* Fixed undefined behavior in localtime.c when using `locale(tz = "")` after
  loading a timezone due to incomplete reinitialization of the global locale.

# readr 1.1.0

## New features

### Parser improvements
* `parse_factor()` gains a `include_na` argument, to include `NA` in the factor levels (#541).
* `parse_factor()` will now can accept `levels = NULL`, which allows one to generate factor levels based on the data (like stringsAsFactors = TRUE) (#497).
* `parse_numeric()` now returns the full string if it contains no numbers (#548).
* `parse_time()` now correctly handles 12 AM/PM (#579).
* `problems()` now returns the file path in additional to the location of the error in the file (#581).
* `read_csv2()` gives a message if it updates the default locale (#443, @krlmlr).
* `read_delim()` now signals an error if given an empty delimiter (#557).
* `write_*()` functions witting whole number doubles are no longer written with a trailing `.0` (#526).

### Whitespace / fixed width improvements

* `fwf_cols()` allows for specifying the `col_positions` argument of
  `read_fwf()` with named arguments of either column positions or widths
  (#616, @jrnold).
* `fwf_empty()` gains an `n` argument to control how many lines are read for whitespace to determine column structure (#518, @Yeedle).
* `read_fwf()` gives error message if specifications have overlapping columns (#534, @gergness)
* `read_table()` can now handle `pipe()` connections (#552).
* `read_table()` can now handle files with many lines of leading comments (#563).
* `read_table2()` which allows any number of whitespace characters as delimiters, a more exact replacement for `utils::read.table()` (#608).

## Writing to connections
* `write_*()` functions now support writing to binary connections. In addition output filenames with `.gz`, `.bz2` or `.xz` will automatically open the appropriate connection and to write the compressed file. (#348)
* `write_lines()` now accepts a list of raw vectors (#542).

## Miscellaneous features
* `col_euro_double()`, `parse_euro_double()`, `col_numeric()`, and `parse_numeric()` have been removed.
* `guess_encoding()` returns a tibble, and works better with lists of raw vectors (as returned by `read_lines_raw()`).
* `ListCallback` R6 Class to provide a more flexible return type for callback functions (#568, @mmuurr)
* `tibble::as.tibble()` now used to construct tibbles (#538).
* `read_csv`, `read_csv2`, and `read_tsv` gain a `quote` argument, (#631, @noamross)

## Bugfixes
* `parse_factor()` now converts data to UTF-8 based on the supplied locale (#615).
* `read_*()` functions with the `guess_max` argument now throw errors on inappropriate inputs (#588).
* `read_*_chunked()` functions now properly end the stream if `FALSE` is returned from the callback.
* `read_delim()` and `read_fwf()` when columns are skipped using `col_types` now report the correct column name (#573, @cb4ds).
* `spec()` declarations that are long now print properly (#597).
* `read_table()` does not print `spec` when `col_types` is not `NULL` (#630, @jrnold).
* `guess_encoding()` now returns a tibble for all ASCII input as well (#641).

# readr 1.0.0

## Column guessing

The process by which readr guesses the types of columns has received a substantial overhaul to make it easier to fix problems when the initial guesses aren't correct, and to make it easier to generate reproducible code. Now column specifications are printing by default when you read from a file:

```R
challenge <- read_csv(readr_example("challenge.csv"))
#> Parsed with column specification:
#> cols(
#>   x = col_integer(),
#>   y = col_character()
#> )
```

And you can extract those values after the fact with `spec()`:

```R
spec(challenge)
#> cols(
#>   x = col_integer(),
#>   y = col_character()
#> )
```

This makes it easier to quickly identify parsing problems and fix them (#314). If the column specification is long, the new `cols_condense()` is used to condense the spec by identifying the most common type and setting it as the default. This is particularly useful when only a handful of columns have a different type (#466).

You can also generating an initial specification without parsing the file using `spec_csv()`, `spec_tsv()`, etc.

Once you have figured out the correct column types for a file, it's often useful to make the parsing strict. You can do this either by copying and pasting the printed output, or for very long specs, saving the spec to disk with `write_rds()`. In production scripts, combine this with `stop_for_problems()` (#465): if the input data changes form, you'll fail fast with an error.

You can now also adjust the number of rows that readr uses to guess the column types with `guess_max`:

```R
challenge <- read_csv(readr_example("challenge.csv"), guess_max = 1500)
#> Parsed with column specification:
#> cols(
#>   x = col_double(),
#>   y = col_date(format = "")
#> )
```

You can now access the guessing algorithm from R. `guess_parser()` will tell you which parser readr will select for a character vector (#377). We've made a number of fixes to the guessing algorithm:

* New example `extdata/challenge.csv` which is carefully created to cause 
  problems with the default column type guessing heuristics.

* Blank lines and lines with only comments are now skipped automatically
  without warning (#381, #321).

* Single '-' or '.' are now parsed as characters, not numbers (#297).

* Numbers followed by a single trailing character are parsed as character,
  not numbers (#316).

* We now guess at times using the `time_format` specified in the `locale()`.

We have made a number of improvements to the reification of the `col_types`, `col_names` and the actual data:

* If `col_types` is too long, it is subsetted correctly (#372, @jennybc).

* If `col_names` is too short, the added names are numbered correctly 
  (#374, @jennybc).
  
* Missing column name names are now given a default name (`X2`, `X7` etc) (#318).
  Duplicated column names are now deduplicated. Both changes generate a warning;
  to suppress it supply an explicit `col_names` (setting `skip = 1` if there's
  an existing ill-formed header).

* `col_types()` accepts a named list as input (#401).

## Column parsing

The date time parsers recognise three new format strings:

* `%I` for 12 hour time format (#340).

* `%AD` and `%AT` are "automatic" date and time parsers. They are both slightly
  less flexible than previous defaults. The automatic date parser requires a 
  four digit year, and only accepts `-` and `/` as separators (#442). The 
  flexible time parser now requires colons between hours and minutes and 
  optional seconds (#424). 

`%y` and `%Y` are now strict and require 2 or 4 characters respectively.

Date and time parsing functions received a number of small enhancements:

* `parse_time()` returns `hms` objects rather than a custom `time` class (#409).
  It now correctly parses missing values (#398).

* `parse_date()` returns a numeric vector (instead of an integer vector) (#357).

* `parse_date()`, `parse_time()` and `parse_datetime()` gain an `na` 
  argument to match all other parsers (#413).
  
* If the format argument is omitted `parse_date()` or `parse_time()`, 
  date and time formats specified in the locale will be used. These now
  default to `%AD` and `%AT` respectively.
  
* You can now parse partial dates with `parse_date()` and 
 `parse_datetime()`, e.g. `parse_date("2001", "%Y")` returns `2001-01-01`.

`parse_number()` is slightly more flexible - it now parses numbers up to the first ill-formed character. For example `parse_number("-3-")` and `parse_number("...3...")` now return -3 and 3 respectively. We also fixed a major bug where parsing negative numbers yielded positive values (#308).

`parse_logical()` now accepts `0`, `1` as well as lowercase `t`, `f`, `true`, `false`. 

## New readers and writers

* `read_file_raw()` reads a complete file into a single raw vector (#451).

* `read_*()` functions gain a `quoted_na` argument to control whether missing
  values within quotes are treated as missing values or as strings (#295).
  
* `write_excel_csv()` can be used to write a csv file with a UTF-8 BOM at the
  start, which forces Excel to read it as UTF-8 encoded (#375).
  
* `write_lines()` writes a character vector to a file (#302).

* `write_file()` to write a single character or raw vector
  to a file (#474).

* Experimental support for chunked reading a writing (`read_*_chunked()`)
  functions. The API is unstable and subject to change in the future (#427).

## Minor features and bug fixes

* Printing double values now uses an
  [implementation](https://github.com/juj/MathGeoLib/blob/master/src/Math/grisu3.c)
  of the [grisu3 algorithm](http://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf)
  which speeds up writing of large numeric data frames by ~10X. (#432) '.0' is
  appended to whole number doubles, to ensure they will be read as doubles as
  well. (#483)

* readr imports tibble so that you get consistent `tbl_df` behaviour 
  (#317, #385).
  
* New example `extdata/challenge.csv` which is carefully created to cause 
  problems with the default column type guessing heuristics.

* `default_locale()` now sets the default locale in `readr.default_locale`
  rather than regenerating it for each call. (#416).

* `locale()` now automatically sets decimal mark if you set the grouping 
  mark. It throws an error if you accidentally set decimal and grouping marks
  to the same character (#450).

* All `read_*()` can read into long vectors, substantially increasing the
  number of rows you can read (#309).

* All `read_*()` functions return empty objects rather than signaling an error 
  when run on an empty file (#356, #441).

* `read_delim()` gains a `trim_ws` argument (#312, noamross)

* `read_fwf()` received a number of improvements:

  * `read_fwf()` now can now reliably read only a partial set of columns
    (#322, #353, #469)
   
  * `fwf_widths()` accepts negative column widths for compatibility with the 
    `widths` argument in `read.fwf()` (#380, @leeper).
  
  * You can now read fixed width files with ragged final columns, by setting
    the final end position in `fwf_positions()` or final width in `fwf_widths()` 
    to `NA` (#353, @ghaarsma). `fwf_empty()` does this automatically.

  * `read_fwf()` and `fwf_empty()` can now skip commented lines by setting a
    `comment` argument (#334).

* `read_lines()` ignores embedded null's in strings (#338) and gains a `na`
  argument (#479).

* `readr_example()` makes it easy to access example files bundled with readr.

* `type_convert()` now accepts only `NULL` or a `cols` specification for
  `col_types` (#369).

* `write_delim()` and `write_csv()` now invisibly return the input data frame
  (as documented, #363).

* Doubles are parsed with `boost::spirit::qi::long_double` to work around a bug 
  in the spirit library when parsing large numbers (#412).

* Fix bug when detecting column types for single row files without headers
  (#333).

# readr 0.2.2

* Fix bug when checking empty values for missingness (caused valgrind issue
  and random crashes).

# readr 0.2.1

* Fixes so that readr works on Solaris.

# readr 0.2.0

## Internationalisation

readr now has a strategy for dealing with settings that vary from place to place: locales. The default locale is still US centric (because R itself is), but you can now easily override the default timezone, decimal separator, grouping mark, day & month names, date format, and encoding. This has lead to a number of changes:

* `read_csv()`, `read_tsv()`, `read_fwf()`, `read_table()`, 
  `read_lines()`, `read_file()`, `type_convert()`, `parse_vector()` 
  all gain a `locale` argument.
  
* `locale()` controls all the input settings that vary from place-to-place.

* `col_euro_double()` and `parse_euro_double()` have been deprecated.
  Use the `decimal_mark` parameter to `locale()` instead.
  
* The default encoding is now UTF-8. To load files that are not 
  in UTF-8, set the `encoding` parameter of the `locale()` (#40).
  New `guess_encoding()` function uses stringi to help you figure out the
  encoding of a file.
  
* `parse_datetime()` and `parse_date()` with `%B` and `%b` use the
  month names (full and abbreviate) defined in the locale (#242).
  They also inherit the tz from the locale, rather than using an
  explicit `tz` parameter.
  
See `vignette("locales")` for more details.

## File parsing improvements

* `cols()` lets you pick the default column type for columns not otherwise
  explicitly named (#148). You can refer to parsers either with their full 
  name (e.g. `col_character()`) or their one letter abbreviation (e.g. `c`).

* `cols_only()` allows you to load only named columns. You can also choose to 
  override the default column type in `cols()` (#72).

* `read_fwf()` is now much more careful with new lines. If a line is too short, 
  you'll get a warning instead of a silent mistake (#166, #254). Additionally,
  the last column can now be ragged: the width of the last field is silently 
  extended until it hits the next line break (#146). This appears to be a
  common feature of "fixed" width files in the wild.

* In `read_csv()`, `read_tsv()`, `read_delim()` etc:
 
  * `comment` argument allows you to ignore comments (#68).
  
  * `trim_ws` argument controls whether leading and trailing whitespace is 
    removed. It defaults to `TRUE` (#137).
  
  * Specifying the wrong number of column names, or having rows with an 
    unexpected number of columns, generates a warning, rather than an error 
    (#189).

  * Multiple NA values can be specified by passing a character vector to 
    `na` (#125). The default has been changed to `na = c("", "NA")`. Specifying 
    `na = ""` now works as expected with character columns (#114).

## Column parsing improvements

Readr gains `vignette("column-types")` which describes how the defaults work and how to override them (#122).

* `parse_character()` gains better support for embedded nulls: any characters 
  after the first null are dropped with a warning (#202).

* `parse_integer()` and `parse_double()` no longer silently ignore trailing
  letters after the number (#221).

* New `parse_time()` and `col_time()` allows you to parse times (hours, minutes, 
  seconds) into number of seconds since midnight. If the format is omitted, it 
  uses a flexible parser that looks for hours, then optional colon, then 
  minutes, then optional colon, then optional seconds, then optional am/pm 
  (#249).

* `parse_date()` and `parse_datetime()`: 

    * `parse_datetime()` no longer incorrectly reads partial dates (e.g. 19, 
      1900, 1900-01) (#136). These triggered common false positives and after 
      re-reading the ISO8601 spec, I believe they actually refer to periods of 
      time, and should not be translated in to a specific instant (#228). 
      
    * Compound formats "%D", "%F", "%R", "%X", "%T", "%x" are now parsed 
      correctly, instead of using the  ISO8601 parser (#178, @kmillar). 
      
    * "%." now requires a non-digit. New "%+" skips one or more non-digits.
      
    * You can now use `%p` to refer to AM/PM (and am/pm) (#126).
      
    * `%b` and `%B` formats (month and abbreviated month name) ignore case 
      when matching (#219).
      
    * Local (non-UTC) times with and without daylight savings are now parsed
      correctly (#120, @andres-s).

* `parse_number()` is a somewhat flexible numeric parser designed to read
  currencies and percentages. It only reads the first number from a string
  (using the grouping mark defined by the locale). 
  
* `parse_numeric()` has been deprecated because the name is confusing - 
  it's a flexible number parser, not a parser of "numerics", as R collectively
  calls doubles and integers. Use `parse_number()` instead. 

As well as improvements to the parser, I've also made a number of tweaks to the heuristics that readr uses to guess column types:

* New `parse_guess()` and `col_guess()` to explicitly guess column type.

* Bumped up row inspection for column typing guessing from 100 to 1000.

* The heuristics for guessing `col_integer()` and `col_double()` are stricter.
  Numbers with leading zeros now default to being parsed as text, rather than
  as integers/doubles (#266).
  
* A column is guessed as `col_number()` only if it parses as a regular number
  when you ignoring the grouping marks.

## Minor improvements and bug fixes

* Now use R's platform independent `iconv` wrapper, thanks to BDR (#149).

* Pathological zero row inputs (due to empty input, `skip` or `n_max`) now
  return zero row data frames (#119).
  
* When guessing field types, and there's no information to go on, use
  character instead of logical (#124, #128).

* Concise `col_types` specification now understands `?` (guess) and
  `-` (skip) (#188).

* `count_fields()` starts counting from 1, not 0 (#200).

* `format_csv()` and `format_delim()` make it easy to render a csv or 
  delimited file into a string.

* `fwf_empty()` now works correctly when `col_names` supplied (#186, #222).

* `parse_*()` gains a `na` argument that allows you to specify which values 
  should be converted to missing.

* `problems()` now reports column names rather than column numbers (#143).
  Whenever there is a problem, the first five problems are printing out 
  in a warning message, so you can more easily see what's wrong.

* `read_*()` throws a warning instead of an error is `col_types`
  specifies a non-existent column (#145, @alyst).

* `read_*()` can read from a remote gz compressed file (#163).

* `read_delim()` defaults to `escape_backslash = FALSE` and 
  `escape_double = TRUE` for consistency. `n_max` also affects the number 
  of rows read to guess the column types (#224).

* `read_lines()` gains a progress bar. It now also correctly checks for 
  interrupts every 500,000 lines so you can interrupt long running jobs.
  It also correctly estimates the number of lines in the file, considerably
  speeding up the reading of large files (60s -> 15s for a 1.5 Gb file).

* `read_lines_raw()` allows you to read a file into a list of raw vectors,
  one element for each line.

* `type_convert()` gains `NA` and `trim_ws` arguments, and removes missing
  values before determining column types.

* `write_csv()`, `write_delim()`, and `write_rds()` all invisibly return their
  input so you can use them in a pipe (#290).

* `write_delim()` generalises `write_csv()` to write any delimited format (#135).
  `write_tsv()` is a helpful wrapper for tab separated files.
  
    * Quotes are only used when they're needed (#116): when the string contains 
      a quote, the delimiter, a new line or NA. 
      
    * Double vectors are saved using same amount of precision as 
      `as.character()` (#117). 
      
    * New `na` argument that specifies how missing values should be written 
      (#187)
    
    * POSIXt vectors are saved in a ISO8601 compatible format (#134). 
    
    * No longer fails silently if it can't open the target for 
      writing (#193, #172).

* `write_rds()` and `read_rds()` wrap around `readRDS()` and `saveRDS()`,
  defaulting to no compression (#140, @nicolasCoutin).
