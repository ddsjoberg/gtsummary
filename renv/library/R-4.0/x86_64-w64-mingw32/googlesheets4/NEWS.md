# googlesheets4 0.3.0

All requests are now made with retry capability. Specifically, when a request fails due to a `429 RESOURCE_EXHAUSTED` error, it is retried a few times, with suitable delays. Note that if it appears that you *personally* have exhausted your quota (more than 100 requests in 100 seconds), the initial waiting time is 100 seconds and this indicates you need to get your own OAuth app or service account.

When googlesheets4 and googledrive are used together in the same session, we alert you if you're logged in to these package with different Google identities.

`gs4_get()` retrieves information about protected ranges.

# googlesheets4 0.2.0

googlesheets4 can now write and modify Sheets.

Several new articles are available at [googlesheets4.tidyverse.org](https://googlesheets4.tidyverse.org/articles/index.html).

## Function naming scheme

The universal `sheets_` prefix has been replaced by a scheme that conveys more information about the scope of the function. There are three prefixes:

* `gs4_`: refers variously to the googlesheets4 package, v4 of the Google 
  Sheets API, or to operations on one or more (spread)Sheets
* `sheet_`: operations on one or more (work)sheets
* `range_`: operations on a range of cells

The addition of write/edit functionality resulted in many new functions and the original naming scheme proved to be problematic. The article [Function and class names](https://googlesheets4.tidyverse.org/articles/articles/function-class-names.html) contains more detail.

Any function present in the previous CRAN release, v0.1.1, still works, but triggers a warning with strong encouragement to switch to the current name.

## Write Sheets

googlesheets4 now has very broad capabilities around Sheet creation and modification. These functions are ready for general use but are still marked experimental, as they may see some refinement based on user feedback.

  * `gs4_create()` creates a new Google Sheet and, optionally, writes one or
    more data frames into it (#61).
  * `sheet_write()` (also available as `write_sheet()`) writes a data frame
    into a new or existing (work)sheet, inside an existing (or new)
    (spread)Sheet.
  * `sheet_append()` adds rows to an existing data table.
  * `range_write()` writes to a cell range.
  * `range_flood()` "floods" all cells in a range with the same content.
    `range_clear()` is a wrapper around `range_flood()` for the special case
     of clearing cell values.
  * `range_delete()` deletes a range of cells.
  
## (Work)sheet operations

The `sheet_*()` family of functions operate on the (work)sheets inside an existing (spread)Sheet:
  
  * (`sheet_write()` and `sheet_append()` are described above.)
  * `sheet_properties()` returns a tibble of metadata with one row per
     sheet.
  * `sheet_names()` returns sheet names.
  * `sheet_add()` adds one or more sheets.
  * `sheet_copy()` copies a sheet.
  * `sheet_delete()` deletes one or more sheets.
  * `sheet_relocate()` moves sheets around.  
  * `sheet_rename()` renames one sheet.
  * `sheet_resize()` changes the number of rows or columns in a sheet.
  
## Range operations

`range_speedread()` reads from a Sheet using its "export=csv" URL and, therefore, uses readr-style column type specification. It still supports fairly general range syntax and auth. For very large Sheets, this can be substantially faster than `read_sheet()`.

`range_read_cells()` (formerly known as `sheets_cells()`) gains two new arguments that make it possible to get more data on more cells. By default, we get only the fields needed to parse cells that contain values. But `range_read_cells(cell_data = "full", discard_empty = FALSE)` is now available if you want full cell data, including formatting, even for cells that have no value (#4).

`range_autofit()` adjusts column width or row height to fit the data. This only affects the display of a sheet and does not change values or dimensions.

## Printing a Sheet ID

The print method for `sheets_id` objects now attempts to reveal the current Sheet metadata available via `gs4_get()`, i.e. it makes an API call (but it should never error).

## Other changes and additions

`gs_formula()` implements a vctrs S3 class for storing Sheets formulas.

`gs4_fodder()` is a convenience function that creates a filler data frame you can use to make toy sheets you're using to practice on or for a reprex.

## Renamed classes

The S3 class `sheets_Spreadsheet` is renamed to `googlesheets4_spreadsheet`, a consequence of rationalizing all internal and external classes (detailed in the article [Function and class names](https://googlesheets4.tidyverse.org/articles/articles/function-class-names.html)). `googlesheets4_spreadsheet` is the class that holds metadata for a Sheet and it is connected to the API's [`Spreadsheet`](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets#resource:-spreadsheet) schema. The return value of `gs4_get()` has this class.

## Bug fixes

* `read_sheet()` passes its `na` argument down to the helpers that parse cells, so that `na` actually has the documented effect (#73).

# googlesheets4 0.1.1

* Patch release to modify a test fixture, to be compatible with tibble v3.0.
  Related to tibble's increased type strictness.

# googlesheets4 0.1.0

* Added a `NEWS.md` file to track changes to the package.
