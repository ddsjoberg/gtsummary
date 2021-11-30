## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4, tibble.print_max = 4)

## ----setup---------------------------------------------------------------
library(readxl)

## ----eval = FALSE--------------------------------------------------------
#  read_excel("yo.xlsx", skip = 5)
#  read_excel("yo.xlsx", n_max = 100)
#  read_excel("yo.xlsx", skip = 5, n_max = 100)
#  read_excel("yo.xlsx", range = "C1:E7")
#  read_excel("yo.xlsx", range = cell_rows(6:23))
#  read_excel("yo.xlsx", range = cell_cols("B:D"))
#  read_excel("yo.xlsx", range = anchored("C4", dim = c(3, 2)))

## ----out.width = '70%', echo = FALSE-------------------------------------
knitr::include_graphics("img/geometry.png")

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"))

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"), skip = 1)

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"), skip = 3)

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"), n_max = 2)

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"), n_max = 1000)

## ------------------------------------------------------------------------
read_excel(readxl_example("deaths.xlsx"), range = "arts!A5:F15")

## ----out.width = '70%', echo = FALSE-------------------------------------
knitr::include_graphics("img/geometry.png")

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"), range = "A2:C4")

## ----eval = FALSE--------------------------------------------------------
#  ## rows only
#  read_excel(..., range = cell_rows(1:10))
#  ## is equivalent to
#  read_excel(..., range = cell_rows(c(1, 10)))
#  
#  ## columns only
#  read_excel(..., range = cell_cols(1:26))
#  ## is equivalent to all of these
#  read_excel(..., range = cell_cols(c(1, 26)))
#  read_excel(..., range = cell_cols("A:Z"))
#  read_excel(..., range = cell_cols(LETTERS))
#  read_excel(..., range = cell_cols(c("A", "Z"))

## ------------------------------------------------------------------------
read_excel(readxl_example("geometry.xlsx"), range = cell_rows(4:8))

## ------------------------------------------------------------------------
read_excel(
  readxl_example("geometry.xlsx"),
  col_names = paste("var", 1:4, sep = "_"),
  range = anchored("C5", c(3, 4))
)

## ------------------------------------------------------------------------
read_excel(
  readxl_example("geometry.xlsx"),
  col_names = FALSE,
  range = cell_limits(c(5, 3), c(NA, NA))
)

