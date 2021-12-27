## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(dplyr)
library(dbplyr)

## -----------------------------------------------------------------------------
mf <- memdb_frame(g = c(1, 1, 2, 2, 2), x = 1:5, y = 5:1)
mf

mf %>% 
  group_by(g) %>% 
  summarise_all(mean, na.rm = TRUE)

## -----------------------------------------------------------------------------
mtcars_db <- tbl_memdb(mtcars)
mtcars_db %>% 
  group_by(cyl) %>% 
  summarise(n = n()) %>% 
  show_query()

## -----------------------------------------------------------------------------
x <- c("abc", "def", "ghif")

lazy_frame(x = x, con = simulate_postgres()) %>% 
  head(5) %>% 
  show_query()

lazy_frame(x = x, con = simulate_mssql()) %>% 
  head(5) %>% 
  show_query()

## -----------------------------------------------------------------------------
translate_sql(substr(x, 1, 2), con = simulate_postgres())
translate_sql(substr(x, 1, 2), con = simulate_sqlite())

