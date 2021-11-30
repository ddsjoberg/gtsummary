# cellranger 1.1.0

Rebooting to support parsing of spreadsheet formulas and cell references as they appear in unevaluated expressions. This work is still in progress but a CRAN update is required now to update a test for testthat v1.0.x.

Package is beginning to implement classes and methods related to cell location and reference from 'Spreadsheet Implementation Technology' by Peter Sestoft, MIT Press, 2014.

New classes:

  * `cell_addr`: one or more absolute cell addresses
  * `ra_ref`: single absolute, relative, or mixed cell reference

# cellranger 1.0.0

  * The two components of a `cell_limits` object now correspond NOT to row and column limits, but rather to the upper left and lower right cells of the rectangle. See #6. It was too confusing to have different conventions for the object and its print method.
  
  * If the maximum row or column is specified, but the minimum is not, then we automatically set the associated minimum to 1, instead of leaving as `NA`.

  * The `header` argument of `anchored()` has been renamed to `col_names`, for greater consistency with [`readr`](https://github.com/hadley/readr), [`readxl`](https://github.com/hadley/readxl), and [`googlesheets`](https://github.com/jennybc/googlesheets/).

  * Added a `NULL` method for `as.cell_limits` generic so that `as.cell_limits(NULL)` returns default, degenerate cell limits, i.e. the min and max for rows and columns are uniformly `NA`.

  * A `cell_limits` object now inherits from "list".

# cellranger 0.1.0

  * Initial CRAN release
