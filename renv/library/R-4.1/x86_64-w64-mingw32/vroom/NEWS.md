# vroom 1.5.5

* `vroom()` now supports files with only carriage return newlines (`\r`). (#360, https://github.com/tidyverse/readr/issues/1236)

* `vroom()` now parses single digit datetimes more consistently as readr has done (https://github.com/tidyverse/readr/issues/1276)

* `vroom()` now parses `Inf` values as doubles (https://github.com/tidyverse/readr/issues/1283)

* `vroom()` now parses `NaN` values as doubles (https://github.com/tidyverse/readr/issues/1277)

* `VROOM_CONNECTION_SIZE` is now parsed as a double, which supports scientific notation (#364)

* `vroom()` now works around specifying a `\n` as the delimiter (#365, https://github.com/tidyverse/dplyr/issues/5977)

* `vroom()` no longer crashes if given a `col_name` and `col_type` both less than the number of columns (https://github.com/tidyverse/readr/issues/1271)

* `vroom()` no longer hangs if given an empty value for `locale(grouping_mark=)` (https://github.com/tidyverse/readr/issues/1241)

* Fix performance regression when guessing with large numbers of rows (https://github.com/tidyverse/readr/issues/1267)

# vroom 1.5.4

* `vroom(col_types=)` now accepts column type names like those accepted by utils::read.table. e.g.
    vroom::vroom(col_types = list(a = "integer", b = "double", c = "skip"))

* `vroom()` now respects the `quote` parameter properly in the first two lines of the file (https://github.com/tidyverse/readr/issues/1262)

* `vroom_write()` now always correctly writes its output including column names in UTF-8 (https://github.com/tidyverse/readr/issues/1242)

* `vroom_write()` now creates an empty file when given a input without any columns (https://github.com/tidyverse/readr/issues/1234)

# vroom 1.5.3

* `vroom(col_types=)` now truncates the column types if the user passes too many types. (#355)

* `vroom()` now always includes the last row when guessing (#352)

* `vroom(trim_ws = TRUE)` now trims field content within quotes as well as without (#354).
  Previously vroom explicitly retained field content inside quotes regardless of the value of `trim_ws`.

# vroom 1.5.2

* `vroom()` now supports inputs with unnamed column types that are less than the number of columns (#296)

* `vroom()` now outputs the correct column names even in the presence of skipped columns (#293, [tidyverse/readr#1215](https://github.com/tidyverse/readr/issues/1215))

* `vroom_fwf(n_max=)` now works as intended when the input is a connection.

* `vroom()` and `vroom_write()` now automatically detect the compression format regardless of the file extension for bzip2, xzip, gzip and zip files (#348)

* `vroom()` and `vroom_write()` now automatically support many more archive formats thanks to the archive package.
  These include new support for writing zip files, reading and writing 7zip, tar and ISO files.

* `vroom(num_threads = 1)` will now not spawn any threads.
  This can be used on as a workaround on systems without full thread support.

* Threads are now automatically disabled on non-macOS systems compiling against clang's libc++.
  Most systems non-macOS systems use the more common gcc libstdc++, so this should not effect most users.

# vroom 1.5.1

* Parsers now treat NA values as NA even if they are valid values for the types (#342)

* Element-wise indexing into lazy (ALTREP) vectors now has much less overhead (#344)

# vroom 1.5.0

## Major improvements

* New `vroom(show_col_types=)` argument to more simply control when column types are shown.

* `vroom()`, `vroom_fwf()` and `vroom_lines()` now support multi-byte encodings such as UTF-16 and UTF-32 by converting these files to UTF-8 under the hood (#138)

* `vroom()` now supports skipping comments and blank lines within data, not just at the start of the file (#294, #302)

* `vroom()` now uses the tzdb package when parsing date-times (@DavisVaughan, #273)

* `vroom()` now emits a warning of class `vroom_parse_issue` if there are non-fatal parsing issues.

* `vroom()` now emits a warning of class `vroom_mismatched_column_name` if the user supplies a column type that does not match the name of a read column (#317).

* The vroom package now uses the MIT license, as part of systematic relicensing throughout the r-lib and tidyverse packages (#323)

## Minor improvements and fixes

* `vroom() correctly reads double values with comma as decimal separator (@kent37 #313)

* `vroom()` now correctly skips lines with only one quote if the format doesn't use quoting (https://github.com/tidyverse/readr/issues/991#issuecomment-616378446)

* `vroom()` and `vroom_lines()` now handle files with mixed windows and POSIX line endings (https://github.com/tidyverse/readr/issues/1210)

* `vroom()` now outputs a tibble with the expected number of columns and types based on `col_types` and `col_names` even if the file is empty (#297).

* `vroom()` no longer mis-indexes files read from connections with windows line endings when the two line endings falls on separate sides of the read buffer (#331)

* `vroom()` no longer crashes if `n_max = 0` and `col_names` is a character (#316)

* `vroom()` now preserves the spec attribute when vroom and readr are both loaded (#303)

* `vroom()` now allows specifying column names in `col_types` that have been repaired (#311)

* `vroom()` no longer inadvertently calls `.name_repair` functions twice (#310).

* `vroom()` is now more robust to quoting issues when tracking the CSV state (#301)

* `vroom()` now registers the S3 class with `methods::setOldClass()` (r-dbi/DBI#345)

* `col_datetime()` now supports '%s' format, which represents decimal seconds since the Unix epoch.

* `col_numeric()` now supports `grouping_mark` and `decimal_mark` that are unicode characters, such as U+00A0 which is commonly used as the grouping mark for numbers in France (https://github.com/tidyverse/readr/issues/796).

* `vroom_fwf()` gains a `skip_empty_rows` argument to skip empty lines (https://github.com/tidyverse/readr/issues/1211)

* `vroom_fwf()` now respects `n_max`, as intended (#334)

* `vroom_lines()` gains a `na` argument.

* `vroom_write_lines()` no longer escapes or quotes lines.

* `vroom_write_lines()` now works as intended (#291).

* `vroom_write(path=)` has been deprecated, in favor of `file`, to match readr.

* `vroom_write_lines()` now exposes the `num_threads` argument.

* `problems()` now prints the correct row number of parse errors (#326)

* `problems()` now throws a more informative error if called on a readr object (#308).

* `problems()` now de-duplicates identical problems (#318)

* Fix an inadvertent performance regression when reading values (#309)

* `n_max` argument is correctly respected in edge cases (#306)

* factors with implicit levels now work when fields are quoted, as intended (#330)

* Guessing double types no longer unconditionally ignores leading whitespace. Now whitespace is only ignored when `trim_ws` is set.

# vroom 1.4.0

## Major changes and new functions

* vroom now tracks indexing and parsing errors like readr. The first time an issue is encountered a warning will be signaled. A tibble of all found problems can be retrieved with `vroom::problems()`. (#247)

* Data with newlines within quoted fields will now automatically revert to using a single thread and be properly read (#282)

* NUL values in character data are now permitted, with a warning.

* New `vroom_write_lines()` function to write a character vector to a file (#291)

* `vroom_write()` gains a `eol=` parameter to specify the end of line character(s) to use. Use `vroom_write(eol = "\r\n")` to write a file with Windows style newlines (#263).

## Minor improvements and fixes

* Datetime formats used when guessing now match those used when parsing (#240)

* Quotes are now only valid next to newlines or delimiters (#224)

* `vroom()` now signals an R error for invalid date and datetime formats, instead of crashing the session (#220).

* `vroom(comment = )` now accepts multi-character comments (#286)

* `vroom_lines()` now works with empty files (#285)

* Vectors are now subset properly when given invalid subscripts (#283)

* `vroom_write()` now works when the delimiter is empty, e.g. `delim = ""` (#287).

* `vroom_write()` now works with all ALTREP vectors, including string vectors (#270)

* An internal call to `new.env()` now correctly uses the `parent` argument (#281)

# vroom 1.3.2

* Test failures on R 4.1 related to factors with NA values fixed (#262)

* `vroom()` now works without error with readr versions of col specs (#256, #264, #266)

# vroom 1.3.1

* Test failures on R 4.1 related to POSIXct classes fixed (#260)

* Column subsetting with double indexes now works again (#257)

* `vroom(n_max=)` now only partially downloads files from connections, as intended (#259)

# vroom 1.3.0

* The Rcpp dependency has been removed in favor of cpp11.

* `vroom()` now handles cases when `id` is set and a column in skipped (#237)

* `vroom()` now supports column selections when there are some empty column names (#238)

* `vroom()` argument `n_max` now works properly for files with windows newlines and no final newline (#244)

* Subsetting vectors now works with `View()` in RStudio if there are now rows to subset (#253).

* Subsetting datetime columns now works with `NA` indices (#236).

# vroom 1.2.1

* `vroom()` now writes the column names if given an input with no rows (#213)

* `vroom()` columns now support indexing with NA values (#201)

* `vroom()` no longer truncates the last value in a file if the file contains windows newlines but no final newline (#219).

* `vroom()` now works when the `na` argument is encoded in non ASCII or UTF-8 locales _and_ the file encoding is not the same as the native encoding (#233).

* `vroom_fwf()` now verifies that the positions are valid, namely that the begin value is always less than the previous end (#217).

* `vroom_lines()` gains a `locale` argument so you can control the encoding of the file (#218)

* `vroom_write()` now supports the `append` argument with R connections (#232)

# vroom 1.2.0

## Breaking changes

* `vroom_altrep_opts()` and the argument `vroom(altrep_opts =)` have been
  renamed to `vroom_altrep()` and `altrep` respectively. The prior names have
  been deprecated.

## New Features

* `vroom()` now supports reading Big Integer values with the `bit64` package.
  Use `col_big_integer()` or the "I" shortcut to read a column as big integers. (#198)

* `cols()` gains a `.delim` argument and `vroom()` now uses it as the delimiter
  if it is provided (#192)

* `vroom()` now supports reading from `stdin()` directly, interpreted as the
  C-level standard input (#106).

## Minor improvements and fixes

* `col_date` now parses single digit month and day (@edzer, #123, #170)

* `fwf_empty()` now uses the `skip` parameter, as intended.

* `vroom()` can now read single line files without a terminal newline (#173).

* `vroom()` can now select the id column if provided (#110).

* `vroom()` now correctly copies string data for factor levels (#184)

* `vroom()` no longer crashes when files have trailing fields, windows newlines
  and the file is not newline or null terminated.

* `vroom()` now includes a spec object with the `col_types` class, as intended.

* `vroom()` now better handles floating point values with very large exponents
  (#164).

* `vroom()` now uses better heuristics to guess the delimiter and now throws an
  error if a delimiter cannot be guessed (#126, #141, #167).

* `vroom()` now has an improved error message when a file does not exist (#169).

* `vroom()` no longer leaks file handles (#177, #180)

* `vroom()` now outputs its messages on `stdout()` rather than `stderr()`,
  which avoids the text being red in RStudio and in the Windows GUI.

* `vroom()` no longer overflows when reading files with more than 2B entries (@wlattner, #183).

* `vroom_fwf()` is now more robust if not all lines are the expected length (#78)

* `vroom_fwf()` and `fwf_empty()` now support passing `Inf` to `guess_max()`.

* `vroom_str()` now works with S4 objects.

* `vroom_fwf()` now handles files with dos newlines properly.

* `vroom_write()` now does not try to write anything when given empty inputs (#172).

* Dates, times, and datetimes now properly consider the locale when parsing.

* Added benchmarks with _wide_ data for both numeric and character data (#87, @R3myG)

* The delimiter used for parsing is now shown in the message output (#95 @R3myG)

# vroom 1.0.2

## New Features

* The column created by `id` is now stored as an run length encoded Altrep
  vector, which uses less memory and is much faster for large inputs. (#111)

## Minor improvements and fixes

* `vroom_lines()` now properly respects the `n_max` parameter (#142)

* `vroom()` and `vroom_lines()` now support reading files which do not end in
  newlines by using a file connection (#40).

* `vroom_write()` now works with the standard output connection `stdout()` (#106).

* `vroom_write()` no longer crashes non-deterministically when used on Altrep vectors.

* The integer parser now returns NA values for invalid inputs (#135)

* Fix additional UBSAN issue in the mio project reported by CRAN (#97)

* Fix indexing into connections with quoted fields (#119)

* Move example files for `vroom()` out of `\dontshow{}`.

* Fix integer overflow with very large files (#116, #119)

* Fix missing columns and windows newlines (#114)

* Fix encoding of column names (#113, #115)

* Throw an error message when writing a zip file, which is not supported (@metaOO, #145)

* Default message output from `vroom()` now uses `Rows` and `Cols` (@meta00, #140)


# vroom 1.0.1

## New Features

* `vroom_lines()` function added, to (lazily) read lines from a file into a
  character vector (#90).

## Minor improvements and fixes

* Fix for a hang on Windows caused by a race condition in the progress bar (#98)

* Remove accidental runtime dependency on testthat (#104)

* Fix to actually return non-Altrep character columns on R 3.2, 3.3 and 3.4.

* Disable colors in the progress bar when running in RStudio, to work around an
  issue where the progress bar would be garbled (https://github.com/rstudio/rstudio/issues/4777)

* Fix for UBSAN issues reported by CRAN (#97)

* Fix for rchk issues reported by CRAN (#94)

* The progress bar now only updates every 10 milliseconds.

* Getting started vignette index entry now more informative (#92)

# vroom 1.0.0

* Initial release

* Added a `NEWS.md` file to track changes to the package.
