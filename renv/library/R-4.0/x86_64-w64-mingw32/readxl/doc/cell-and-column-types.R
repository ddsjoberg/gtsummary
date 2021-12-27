## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4, tibble.print_max = 4)

## ----setup---------------------------------------------------------------
library(readxl)

## ----eval = FALSE--------------------------------------------------------
#  read_excel("yo.xlsx")
#  read_excel("yo.xlsx", col_types = "numeric")
#  read_excel("yo.xlsx", col_types = c("date", "skip", "guess", "numeric"))

## ------------------------------------------------------------------------
read_excel(readxl_example("deaths.xlsx"), range = cell_rows(5:15))

## ------------------------------------------------------------------------
read_excel(
  readxl_example("deaths.xlsx"),
  range = cell_rows(5:15),
  col_types = c("guess", "skip", "guess", "skip", "skip", "skip")
)

## ------------------------------------------------------------------------
(clippy <- 
   read_excel(readxl_example("clippy.xlsx"), col_types = c("text", "list")))
tibble::deframe(clippy)
sapply(clippy$value, class)

## ------------------------------------------------------------------------
deaths <- read_excel(readxl_example("deaths.xlsx"))
print(deaths, n = Inf)

## ------------------------------------------------------------------------
(nms <- names(read_excel(readxl_example("datasets.xlsx"), n_max = 0)))
(ct <- ifelse(grepl("^Petal", nms), "text", "guess"))
read_excel(readxl_example("datasets.xlsx"), col_types = ct)

## ------------------------------------------------------------------------
df <- read_excel(readxl_example("type-me.xlsx"), sheet = "logical_coercion",
                 col_types = c("logical", "text"))
print(df, n = Inf)

## ----out.width = '70%', echo = FALSE-------------------------------------
knitr::include_graphics("img/type-me-logical.png")

## ------------------------------------------------------------------------
df <- read_excel(readxl_example("type-me.xlsx"), sheet = "numeric_coercion",
                 col_types = c("numeric", "text"))
print(df, n = Inf)

## ----out.width = '70%', echo = FALSE-------------------------------------
knitr::include_graphics("img/type-me-numeric.png")

## ------------------------------------------------------------------------
df <- read_excel(readxl_example("type-me.xlsx"), sheet = "date_coercion",
                 col_types = c("date", "text"))
print(df, n = Inf)

## ----out.width = '70%', echo = FALSE-------------------------------------
knitr::include_graphics("img/type-me-date.png")

## ------------------------------------------------------------------------
df <- read_excel(readxl_example("type-me.xlsx"), sheet = "text_coercion",
                 col_types = c("text", "text"))
print(df, n = Inf)

## ----out.width = '70%', echo = FALSE-------------------------------------
knitr::include_graphics("img/type-me-text.png")

