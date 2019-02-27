#' Simple utility function to get extract and calculate additional information
#' about the 'by' variable in \code{\link{tbl_summary}}
#'
#' Given a dataset and the name of the 'by' variable, this function returns a
#' data frame with unique levels of the by variable, the by variable ID, a character
#' version of the levels, and the column name for each level in the \code{\link{tbl_summary}}
#' output data frame.
#'
#' @param data data frame
#' @param by character name of the `by` variable found in data
#' @keywords internal
#' @author Daniel Sjoberg

df_by <- function(data, by) {
    data %>%
    select(c(by)) %>%
    set_names("by") %>%
    count_("by") %>%
    mutate_(N = ~sum(n), p = ~n / N) %>%
    arrange_("by") %>%
    mutate_(
      by_id = ~ 1:n(), # 'by' variable ID
      by_chr = ~ as.character(by), # Character version of 'by' variable
      by_col = ~ paste0("stat_", by_id) # Column name of in fmt_table1 output
    ) %>%
    select(starts_with("by"), everything())
}
# > df_by(mtcars, "am")
# # A tibble: 2 x 7
#      by     n     N     p by_id by_chr by_col
#   <dbl> <int> <int> <dbl> <int> <chr>  <chr>
# 1     0    19    32 0.594     1 0      stat_1
# 2     1    13    32 0.406     2 1      stat_2

# > df_by(iris, "Species")
# # A tibble: 3 x 7
#   by             n     N     p by_id by_chr     by_col
#   <fct>      <int> <int> <dbl> <int> <chr>      <chr>
# 1 setosa        50   150 0.333     1 setosa     stat_1
# 2 versicolor    50   150 0.333     2 versicolor stat_2
# 3 virginica     50   150 0.333     3 virginica  stat_3
