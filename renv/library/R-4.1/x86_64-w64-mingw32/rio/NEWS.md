# rio 0.5.27

* Documentation fixes for CRAN.

# rio 0.5.26

* Added support for "zsav" format. (#273)

# rio 0.5.25

* Modified tests per email request from CRAN.
* Added `coerce_character` argument (default FALSE) to `factorize()` to enable coercing character columns to factor. (#278)

# rio 0.5.24

* Fix handling of "label" and "labels" attributes when exporting using haven methods (SPSS, Stata, SAS). (#268, h/t Ruben Arslan)
* Fix (a different bug?) handling factors by haven::labelled() (#271, Alex Bokov)
* HTML import can now handle multiple tbody elements within a single table, a th element in a non-header row, and empty elements in either the header or data. (#260, #263, #264 Bill Denney)

# rio 0.5.23

* CSVY support is now provided by `data.table::fread()` and `data.table::fwrite()`, providing significant performance gains.
* Added an internal `arg_reconcile()` function to streamline the task of removing/renaming arguments for compatibility with various functions (#245, Alex Bokov)

# rio 0.5.22

* Added an `export_list()` function to write a list of data frames to multiple files using a vector of file names or a file pattern. (#207, h/t Bill Denney)
* Added an `is_file_text()` function to determine whether a file is in a plain-text format. Optionally narrower subsets of characters can be specified, e.g. ASCII. (#236 Alex Bokov)

# rio 0.5.21

* Added support for Apache Arrow (Parquet) files. (#214)
* Fix dropping of variable label in `characterize()` and `factorize()`. (#204, h/t David Armstrong)
* `import_list()` now returns a `filename` attribute for each data frame in the list (when importing from multiple files), in order to distinguish files with the same base name but different extensions (e.g., `import_list(c("foo.csv", "foo.tsv"))`). (#208, h/t Vimal Rawat)
* Import of DBF files now does not convert strings to factors. (#202, h/t @jllipatz)
* Implemented `import()` method for .dump R files. (#240)

# rio 0.5.20

* Additional pointers were added to indicate how to load .doc, .docx, and .pdf files (#210, h/t Bill Denney)
* Ensure that tests only run if the corresponding package is installed.  (h/t Bill Denney)
* Escape ampersands for html and xml export (#234 Alex Bokov)

# rio 0.5.19

* Fix behavior of `export()` to plain text files when `append = TRUE` (#201, h/t Julián Urbano)
* `import_list()` now preserve names of Excel sheets, etc. when the 'which' argument is specified. (#162, h/t Danny Parsons)
* Modify message and errors when working with unrecognized file formats. (#195, h/t Trevor Davis)
* Add support for GraphPad Prism .pzfx files (#205, h/t Bill Denney)

# rio 0.5.18

* Adjust `import()`/`export()` for JSON file formats to allow non-data frame objects. Behavior modeled after RDS format. (#199 h/t Nathan Day)

# rio 0.5.17

* Fix `the condition has length > 1 and only the first element will be used` warning in `gather_attributes()`. (#196, h/t Ruben Arslan)

# rio 0.5.16

* Fix `the condition has length > 1 and only the first element will be used` warning in `standardize_attributes()`.

# rio 0.5.15

* Modified some further code to produce compatibility with haven 2.0.0 release. (#188)
* Add some additional function suggestions for the ledger package. (#190, h/t Trevor Davis)

# rio 0.5.14

* Changes to `gather_attrs()` for haven 2.0.0 release. (#188)
* Fixed a bug that generated a superfluous warning in `import()`.
* Some style guide changes to code.

# rio 0.5.13

* Allow `import()` of objects other than data frames from R-serialized (.rds and .rdata) files. Also, export of such objects to .rds files is supported, as previously intended. (#183, h/t Nicholas Jhirad)
* Added (suggests) support for import of EViews files using `hexView::readEViews()`. (#163, h/t Boris Demeshev)

 # rio 0.5.12

* Add better package specification to `install_formats()` so that it reads from the `Suggests` field of the `DESCRIPTION` file.
* Edit header of `README.Rmd` (and thusly `README.md`) to stop complaining about a lack of title field.
* Fix typo in `CONTRIBUTING.md` (line said "three arguments", but only listed two).

# rio 0.5.11

* Fixed a bug in `import()` wherein matlab files were ignored unless `format` was specified, as well as a related bug that made importing appear to fail for matlab files. (#171)
* Fixed a bug in `export()` wherein `format` was ignored. (#99, h/t Sebastian Sauer)
* Fixed a bug in the importing of European-style semicolon-separated CSV files. Added a test to ensure correct behavior. (#159, h/t Kenneth Rose)
* Updated documentation to reflect recent changes to the xlsx `export()` method. (#156)

# rio 0.5.10

* Removed some csvy-related tests, which were failing on CRAN.

# rio 0.5.9

* Removed longstanding warnings from the tests of `export()` to fixed-width format.

# rio 0.5.8

* Export the `get_ext()` function. (#169)
* Fix a bug related to an xml2 bug (#168, h/t Jim Hester)
* `import_list()` gains improved file name handling. (#164, h/t Ruaridh Williamson)
* Removed the `overwrite` argument from `export()` method for xlsx files. Instead, existing workbooks are always overwritten unless which is specified, in which case only the specified sheet (if it exists) is overwritten. If the file exists but the `which` sheet does not, the data are added as a new sheet to the existing workbook. (#156)

# rio 0.5.7

* Import of files with the ambiguous .dat extension, which are typically text-delimited files, are now passed to `data.table::fread()` with a message. Export to the format remains unsupported. (#98, #155)
* Added support for export to SAS XPORT format (via `haven::write_xpt()`). (#157)
* Switched default import package for SAS XPORT format to `haven::read_xpt()` with a `haven = FALSE` toggle restoring the previous default behavior using `foreign::read.xpt()`. (#157)

# rio 0.5.6

* Fixed a bug in `import()` from compressed files wherein the `which` argument did not necessarily return the correct file if >=2 files in the compressed folder.
* Tweak handling of `export()` to xlsx workbooks when `which` is specified. (#156)

# rio 0.5.5

* Expanded test suite and increased test coverage, fixing a few tests that were failing on certain CRAN builds.

# rio 0.5.4

* New functions `characterize()` and `factorize()` provide methods for converting "labelled" variables (e.g., from Stata or SPSS) into character or factor variables using embedded metadata. This can also be useful for exporting a metadata-rich file format into a plain text file. (#153)

# rio 0.5.3

* Fixed a bug in writing to .zip and .tar archives related to absolute file paths.
* Fixed some small bugs in `import_list()` and added tests for behavior.
* Add .bib as known-unsupported format via `bib2df::bib2df()`.
* Expanded test coverage.

# rio 0.5.3

* Fixed a bug in `.import.rio_xlsx()` when `readxl = FALSE`. (#152, h/t Danny Parsons)
* Added a new function `spread_attrs()` that reverses the `gather_attrs()` operation.
* Expanded test coverage.

# rio 0.5.1

* `export()` now sets variables with a "labels" attribute to **haven**'s "labelled" class.

# rio 0.5.0

* CRAN Release.
* Restored import of **openxlsx** so that writing to xlsx is supported on install. (#150)

# rio 0.4.28

* Improved documentation of mapping between file format support and the packages used for each format. (#151, h/t Patrick Kennedy)
* `import_list()` now returns a `NULL` entry for any failed imports, with a warning. (#149)
* `import_list()` gains additional arguments `rbind_fill` and `rbind_label` to control rbind-ing behavior. (#149)

# rio 0.4.27

* Import to and export from the clipboard now relies on `clipr::read_clip()` and `clipr::write_clip()`, respectively, thus (finally) providing Linux support. (#105, h/t Matthew Lincoln)
* Added an `rbind` argument to `import_list()`. (#149)
* Added a `setclass` argument to `import_list()`, ala the same in `import()`.
* Switched `requireNamespace()` calls to `quietly = TRUE`.

# rio 0.4.26

* Further fixes to .csv.gz import/export. (#146, h/t Trevor Davis)

# rio 0.4.25

* Remove unecessary **urltools** dependency.
* New function `import_list()` returns a list of data frames from a multi-object Excel Workbook, .Rdata file, zip directory, or HTML file. (#126, #129)
* `export()` can now write a list of data frames to an Excel (.xlsx) workbook. (#142, h/t Jeremy Johnson)
* `export()` can now write a list of data frames to an HTML (.html) file.

# rio 0.4.24

* Verbosity of `export(format = "fwf")` now depends on `options("verbose")`.
* Fixed various errors, warnings, and messages in fixed-width format tests.
* Modified defaults and argument handling in internal function `read_delim()`.
* Fixed handling of "data.table", "tibble", and "data.frame" classes in `set_class()`. (#144)

# rio 0.4.23

* Moved all non-critical format packages to Suggests, rather than Imports. (#143)
* Added support for Matlab formats. (#78, #98)
* Added support for fst format. (#138)

# rio 0.4.22

* Rearranged README.
* Bumped readxl dependency to `>= 0.1.1` (#130, h/t Yongfa Chen)
* Pass explicit `excel_format` arguments when using **readxl** functions. (#130)
* Google Spreadsheets can now be imported using any of the allowed formats (CSV, TSV, XLSX, ODS).
* Added support for writing to ODS files via `readODS::write_ods()`. (#96)

# rio 0.4.21

* Handle HTML tables with `<tbody>` elements. (h/t Mohamed Elgoussi)

# rio 0.4.20

* Fixed a big in the `.import.rio_xls()` and `.import.rio_xlsx()` where the `sheet` argument would return an error.

# rio 0.4.19

* Fixed a bug in the import of delimited files when `fread = FALSE`. (#133, h/t Christopher Gandrud)

# rio 0.4.18

* With new data.table release, export using `fwrite()` is now the default for text-based file formats.

# rio 0.4.17

* Fixed a bug in `.import.rio_xls()` wherein the `which` argument was ignored. (h/t Mohamed Elgoussi)

# rio 0.4.16

* Added support for importing from multi-table HTML files using the `which` argument. (#126)

# rio 0.4.15

* Improved behavior of `import()` and `export()` with respect to unrecognized file types. (#124, #125, h/t Jason Becker)
* Added explicit tests of the S3 extension mechanism for `.import()` and `.export()`.
* Attempt to recognize compressed but non-archived file formats (e.g., ".csv.gz"). (#123, h/t trevorld)

# rio 0.4.14

* Update import and export methods to use new xml2 for XML and HTML export. (#86)

# rio 0.4.13

* Fix failing tests related to stricter variable name handling for Stata files in development version of haven. (#113, h/t Hadley Wickham)
* Added support for export of .sas7bdat files via haven (#116)
* Restored support for import from SPSS portable via haven (#116)
* Updated import methods to reflect changed formal argument names in haven. (#116)
* Converted to roxygen2 documentation and made NEWS an explicit markdown file.

# rio 0.4.12

* rio sets `options(datatable.fread.dec.experiment=FALSE)` during onLoad to address a Unix-specific locale issue.

# rio 0.4.11

* Note unsupported NumPy i/o via RcppCNPy. (#112)
* Fix import of European-style CSV files (sep = "," and sep2 = ";"). (#106, #107, h/t Stani Stadlmann)

# rio 0.4.10

* Changed feather Imports to Suggests to make rio installable on older R versions. (#104)
* Noted new RStudio add-in, GREA, that uses rio. (#109)
* Migrated CSVY-related code to separate package (https://github.com/leeper/csvy/). (#111)

# rio 0.4.9

* Removed unnecessary error in xlsx imports. (#103, h/t Kevin Wright)

# rio 0.4.8

* Fixed a bug in the handling of "labelled" class variables imported from haven. (#102, h/t Pierre LaFortune)

# rio 0.4.7

* Improved use of the `sep` argument for import of delimited files. (#99, h/t Danny Parsons)
* Removed support for import of SPSS Portable (.por) files, given deprecation from haven. (#100)

# rio 0.4.5

* Fixed other tests to remove (unimportant) warnings.
* Fixed a failing test of file compression that was found in v0.4.3 on some platforms.

# rio 0.4.3

* Improved, generalized, tested, and expanded documentation of `which` argument in `import()`.
* Expanded test suite and made some small fixes.

# rio 0.4.2

* Added support to import and export to `feather` data serialization format. (#88, h/t Jason Becker)

# rio 0.4.1

* Fixed behavior of `gather_attrs()` on a data.frame with no attributes to gather. (#94)
* Removed unrecognized file format error for import from compressed files. (#93)

# rio 0.4.0

* CRAN Release.

# rio 0.3.19

* Added a `gather_attrs()` function that moves variable-level attributes to the data.frame level. (#80)
* Added preliminary support for import from HTML tables (#86)

# rio 0.3.18

* Added support for export to HTML tables. (#86)

# rio 0.3.17

* Fixed a bug in import from remote URLs with incorrect file extensions.

# rio 0.3.16

* Added support for import from fixed-width format files via `readr::read_fwf()` with a specified `widths` argument. This may enable faster import of these types of files and provides a base-like interface for working with readr. (#48)

# rio 0.3.15

* Added support for import from and export to yaml. (#83)
* Fixed a bug when reading from an uncommented CSVY yaml header that contained single-line comments. (#84, h/t Tom Aldenberg)

# rio 0.3.14

* Diagnostic messages were cleaned up to facilitate translation. (#57)

# rio 0.3.12

* `.import()` and `.export()` are now exported S3 generics and documentation has been added to describe how to write rio extensions for new file types. An example of this functionality is shown in the new suggested "rio.db" package.

# rio 0.3.11

* `import()` now uses xml2 to read XML structures and `export()` uses a custom method for writing to XML, thereby negating dependency on the XML package. (#67)
* Enhancements were made to import and export of CSVY to store attribute metadata as variable-level attributes (like imports from binary file formats).
* `import()` gains a `which` argument that is used to select which file to return from within a compressed tar or zip archive.
* Export to tar now tries to correct for bugs in `tar()` that are being fixed in base R via [PR#16716](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16716).

# rio 0.3.10

* Fixed a bug in `import()` (introduced in #62, 7a7480e5) that prevented import from clipboard. (h/t Kevin Wright)
* `export()` returns a character string. (#82)

# rio 0.3.9

* The use of `import()` for SAS, Stata, and SPSS files has been streamlined. Regardless of whether the `haven = TRUE` argument is used, the data.frame returned by `import()` should now be (nearly) identical, with all attributes stored at the variable rather than data.frame level. This is a non-backwards compatible change. (#80)

# rio 0.3.8

* Fixed error in export to CSVY with a commented yaml header. (#81, h/t Andrew MacDonald)

# rio 0.3.7

* `export()` now allows automatic file compression as tar, gzip, or zip using the `file` argument (e.g., `export(iris, "iris.csv.zip")`).

# rio 0.3.6

* Expanded verbosity of `export()` for fixed-width format files and added a commented header containing column class and width information.
* Exporting factors to fixed-width format now saves those values as integer rather than numeric.
* Expanded test suite and separated tests into format-specific files. (#51)

# rio 0.3.5

* Export of CSVY files now includes commenting the yaml header by default. Import of CSVY accommodates this automatically. (#74)

# rio 0.3.3

* Export of CSVY files and metadata now supported by `export()`. (#73)
* Import of CSVY files now stores dataset-level metadata in attributes of the output data.frame. (#73, h/t Tom Aldenberg)
* When rio receives an unrecognized file format, it now issues a message. The new internal `.import.default()` and `.export.default()` then produce an error. This enables add-on packages to support additional formats through new s3 methods of the form `.import.rio_EXTENSION()` and `.export.rio_EXTENSION()`.

# rio 0.3.2

* Use S3 dispatch internally to call new (unexported) `.import()` and `.export()` methods. (#42, h/t Jason Becker)

# rio 0.3.0

* Release to CRAN.
* Set a default numerical precision (of 2 decimal places) for export to fixed-width format.

# rio 0.2.13

* Import stats package for `na.omit()`.

# rio 0.2.11

* Added support for direct import from Google Sheets. (#60, #63, h/t Chung-hong Chan)

# rio 0.2.7

* Refactored remote file retrieval into separate (non-exported) function used by `import()`. (#62)
* Added test sutie to test file conversion.
* Expanded test suite to include test of all export formats.

# rio 0.2.6

* Cleaned up NAMESPACE file.

# rio 0.2.5

* If file format for a remote file cannot be identified from the supplied URL or the final URL reported by `curl::curl_fetch_memory()`, the HTTP headers are checked for a filename in the Content-Disposition header. (#36)
* Removed longurl dependency. This is no longer needed because we can identify formats using curl's url argument.
* Fixed a bug related to importing European-style ("csv2") format files. (#44)
* Updated CSVY import to embed variable-level metadata. (#52)
* Use `urltools::url_parse()` to extract file extensions from complex URLs (e.g., those with query arguments). (#56)
* Fixed NAMESPACE notes for base packages. (#58)

# rio 0.2.4

* Modified behavior so that files imported using haven now store variable metadata at the data.frame level by default (unlike the default behavior in haven, which can cause problems). (#37, h/t Ista Zahn)
* Added support for importing CSVY (http://csvy.org/) formatted files. (#52)
* Added import dependency on data.table 1.9.5. (#39)

# rio 0.2.2

* Uses the longurl package to expand shortened URLs so that their file type can be easily determined.

# rio 0.2.1

* Improved support for importing from compressed directories, especially web-based compressed directories. (#38)
* Add import dependency on curl >= 0.6 to facilitate content type parsing and format inference from URL redirects. (#36)
* Add bit64 to `Suggests` to remove an `import` warning.

# rio 0.2

* `import` always returns a data.frame, unless `setclass` is specified. (#22)
* Added support for import from legacy Excel (.xls) files `readxl::read_excel`, making its use optional. (#19)
* Added support for import from and export to the system clipboard on Windows and Mac OS.
* Added support for export to simple XML documents. (#12)
* Added support for import from simple XML documents via `XML::xmlToDataFrame`. (#12)
* Added support for import from ODS spreadsheet formats. (#12, h/t Chung-hong Chan)
* Use `data.table::fread` by default for reading delimited files. (#3)
* Added support for import and export of `dput` and `dget` objects. (#10)
* Added support for reading from compressed archives (.zip and .tar). (#7)
* Added support for writing to fixed-width format. (#8)
* Set `stringsAsFactors = FALSE` as default for reading tabular data. (#4)
* Added support for HTTPS imports. (#1, h/t Christopher Gandrud)
* Added support for automatic file naming in `export` based on object name and file format. (#5)
* Exposed `convert` function.
* Added vignette, knitr-generated README.md, and updated documentation. (#2)
* Added some non-exported functions to simplify argument passing and streamline package API. (#6)
* Separated `import`, `export`, `convert`, and utilities into separate source code files.
* Expanded the set of supported file types/extensions, switched SPSS, SAS, and Stata formats to **haven**, making its use optional.

# rio 0.1.2

* Updated documentation and fixed a bug in csv import without header.

# rio 0.1.1

* Initial release
