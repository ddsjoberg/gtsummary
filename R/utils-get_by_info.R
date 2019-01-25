#' Simple utility function to get extract and calculate additional information
#' about the 'by' variable in \code{\link{fmt_table1}}
#'
#' Given a dataset and the name of the 'by' variable, this function returns a
#' data frame with unique levels of the by variable, the by variable ID, a character
#' version of the levels, and the column name for each level in the \code{\link{fmt_table1}}
#' output data frame.
#'
#' @param data data frame
#' @param by character name of the `by` variable found in data
#' @keywords internal

get_by_info <- function(data, by) {
  by_info <-
    data %>%
    dplyr::select(dplyr::one_of(by)) %>%
    dplyr::distinct() %>%
    dplyr::arrange_(by)
  by_info <-
    by_info %>%
    dplyr::mutate_(
      by = ~ by_info[[by]], # Unique values of 'by' variable, sorted
      by_id = ~ 1:n(), # 'by' variable ID
      by_chr = ~ as.character(by), # Character version of 'by' variable
      by_col = ~ paste0("stat_by", by_id) # Column name of in fmt_table1 output
    )
  return(by_info)
}

# output is a data frame

# > get_by_info(mtcars, "am")
# am by by_id by_chr   by_col
# 1  0  0     1      0 stat_by1
# 2  1  1     2      1 stat_by2

# > get_by_info(iris, "Species")
#      Species         by by_id     by_chr   by_col
# 1     setosa     setosa     1     setosa stat_by1
# 2 versicolor versicolor     2 versicolor stat_by2
# 3  virginica  virginica     3  virginica stat_by3
