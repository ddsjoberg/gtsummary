#' Renames columns to values in a row of data frame
#'
#' @param data input data set
#' @param row_n row number to take names from. Default is row 1
#' @param delete_row logical argument whether to delete row after renaming.
#' Default is `TRUE`
#' @export

row_to_name <- function(data, row_n = 1, delete_row = TRUE) {
  # replacing column names with values from the first row in the data frame
  names(data) <- data[row_n, ] %>% t() %>% as.vector()
  if (delete_row == TRUE) data <- data[-row_n, ]

  return(data)
}
