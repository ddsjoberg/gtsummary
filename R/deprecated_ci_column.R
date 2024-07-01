#' `"ci"` Column Deprecated
#'
#' @description
#'
#' \lifecycle{deprecated}\cr
#' ## Overview
#'
#' When the gtsummary package was first written, the gt package was not on CRAN
#' and the version of the package that was available did not have the ability
#' to merge columns.
#' Due to these limitations, the `"ci"` column was added to show the combined
#' `"conf.low"` and `"conf.high"` columns.
#'
#' Column merging in both gt and gtsummary packages has matured over the years,
#' and we are now adopting a more modern approach by using these features.
#' As a result, the `"ci"` column will eventually be dropped from `.$table_body`.
#'
#' By using column merging, the `conf.low` and `conf.high` remain numeric
#' and we can to continue to update how these columns are formatted.
#'
#' ## How to update?
#'
#' In most cases it is a simple change to adapt your code to the updated
#' structure: simply swap `ci` with `conf.low`.
#' For example, if you had updated the column header with `modify_header(ci = "Confidence Interval")`,
#' you would update your code to
#'
#' ```r
#' modify_header(conf.low = "Confidence Interval")
#' ```
#'
#' If you need to update a reference from `pattern` argument in `modify_column_merge()` or
#' `inline_text()`, you can replace the `"{ci}"` reference with `"{conf.low}, {conf.high}"`.
#'
#' @keywords internal
#' @name deprecated_ci_column
NULL
