#' Column `"ci"` Deprecated
#'
#' @description
#'
#' `r lifecycle::badge('deprecated')`\cr
#' # Overview
#'
#' When the gtsummary package was first written, the gt package was not on CRAN
#' and the version of the package that was available did not have the ability
#' to merge columns.
#' Due to these limitations, the pre-formatted `"ci"` column was added to show the combined
#' `"conf.low"` and `"conf.high"` columns.
#'
#' Column merging in both gt and gtsummary packages has matured over the years,
#' and we are now adopting a more modern approach by using these features.
#' As a result, the pre-formatted `"ci"` column will eventually be dropped from `.$table_body`.
#'
#' By using column merging, the `conf.low` and `conf.high` remain numeric
#' and we can to continue to update how these columns are formatted, even after printing the table.
#'
#' The `"ci"` column is hidden, meaning that it appears in `.$table_body`, but is not printed.
#' This means that references to the column in your code will not error, but will likely not have the intended effect.
#'
#' # How to update?
#'
#' In most cases it is a simple change to adapt your code to the updated
#' structure: simply swap `ci` with `conf.low`.
#'
#' See below for examples on how to update your code.
#'
#' ## `modify_header()`
#'
#' While the `"ci"` column is hidden, if a new header is defined for the column it will be unhidden.
#' Code that changes the header of `"ci"` will likely lead to duplicate columns appearing in your table
#' (that is, the `"ci"` column and the merged `"conf.low"` and `"conf.high"` columns).
#'
#' ```{r, echo=FALSE}
#' dplyr::tribble(
#'   ~`**Old Code**`, ~`**Updated Code**`,
#'   "`modify_header(ci = \"Confidence Interval\")`", "`modify_header(conf.low = \"Confidence Interval\")`"
#' ) |>
#'   knitr::kable()
#' ```
#'
#' ## `modify_spanning_header()`
#'
#' ```{r, echo=FALSE}
#' dplyr::tribble(
#'   ~`**Old Code**`, ~`**Updated Code**`,
#'   "`modify_spanning_header(ci = \"Confidence Interval\")`", "`modify_spanning_header(conf.low = \"Confidence Interval\")`"
#' ) |>
#'   knitr::kable()
#' ```
#'
#' ## `modify_spanning_header()`
#'
#' ```{r, echo=FALSE}
#' dplyr::tribble(
#'   ~`**Old Code**`, ~`**Updated Code**`,
#'   "`modify_spanning_header(ci = \"Confidence Interval\")`", "`modify_spanning_header(conf.low = \"Confidence Interval\")`"
#' ) |>
#'   knitr::kable()
#' ```
#'
#' ## `modify_column_merge()`
#'
#' ```{r, echo=FALSE}
#' dplyr::tribble(
#'   ~`**Old Code**`, ~`**Updated Code**`,
#'   "`modify_column_merge(pattern = \"{estimate} ({ci})\")`", "`modify_column_merge(pattern = \"{estimate} ({conf.low}, {conf.high})\"`"
#' ) |>
#'   knitr::kable()
#' ```
#'
#' ## `modify_column_hide()`
#'
#' ```{r, echo=FALSE}
#' dplyr::tribble(
#'   ~`**Old Code**`, ~`**Updated Code**`,
#'   "`modify_column_hide(columns = \"ci\")`", "`modify_column_hide(columns = \"conf.low\")`"
#' ) |>
#'   knitr::kable()
#' ```
#'
#' ## `inline_text()`
#'
#' ```{r, echo=FALSE}
#' dplyr::tribble(
#'   ~`**Old Code**`, ~`**Updated Code**`,
#'   "`inline_text(pattern = \"{estimate} (95% CI {ci})\")`", "`inline_text(pattern = \"{estimate} (95% CI {conf.low}, {conf.high})\")`"
#' ) |>
#'   knitr::kable()
#' ```
#'
#' @keywords internal
#' @name deprecated_ci_column
NULL






