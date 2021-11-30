## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.myConnectionClass <- function(con) 2L

