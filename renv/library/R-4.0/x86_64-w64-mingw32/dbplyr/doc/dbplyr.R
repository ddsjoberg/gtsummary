## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("dbplyr")

## ----setup, message = FALSE---------------------------------------------------
library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

## ---- eval = FALSE------------------------------------------------------------
#  con <- DBI::dbConnect(RMariaDB::MariaDB(),
#    host = "database.rstudio.com",
#    user = "hadley",
#    password = rstudioapi::askForPassword("Database password")
#  )

## -----------------------------------------------------------------------------
copy_to(con, nycflights13::flights, "flights",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum",
    "dest"
  )
)

## -----------------------------------------------------------------------------
flights_db <- tbl(con, "flights")

## -----------------------------------------------------------------------------
flights_db 

## -----------------------------------------------------------------------------
flights_db %>% select(year:day, dep_delay, arr_delay)

flights_db %>% filter(dep_delay > 240)

flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))

## -----------------------------------------------------------------------------
tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  arrange(desc(delay)) %>%
  filter(n > 100)

## -----------------------------------------------------------------------------
tailnum_delay_db

## -----------------------------------------------------------------------------
tailnum_delay_db %>% show_query()

## -----------------------------------------------------------------------------
tailnum_delay <- tailnum_delay_db %>% collect()
tailnum_delay

## ---- error = TRUE------------------------------------------------------------
nrow(tailnum_delay_db)

tail(tailnum_delay_db)

