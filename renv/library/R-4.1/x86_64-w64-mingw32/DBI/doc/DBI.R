## ----setup, include=FALSE,warning=FALSE, message=FALSE------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(error = Sys.getenv("IN_PKGDOWN") != "true" || (getRversion() < "3.5"))

## -----------------------------------------------------------------------------
library(DBI)

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "relational.fit.cvut.cz",
  port = 3306,
  username = "guest",
  password = "relational",
  dbname = "sakila"
)

dbListTables(con)
dbDisconnect(con)

## -----------------------------------------------------------------------------
con <- dbConnect(RMariaDB::MariaDB(), username = "guest", password = "relational", host = "relational.fit.cvut.cz", port = 3306, dbname = "sakila")
dbListFields(con, "film")

## -----------------------------------------------------------------------------
df <- dbReadTable(con, "film")
head(df, 3)

## -----------------------------------------------------------------------------
df <- dbGetQuery(con, "SELECT film_id, title, description FROM film WHERE release_year = 2006")
head(df,3)

## -----------------------------------------------------------------------------
df <- dbGetQuery(con, "SELECT film_id, title, description FROM film WHERE release_year = 2006 and rating = 'G'")
head(df,3)

## ----message=FALSE------------------------------------------------------------
library(dplyr)

lazy_df <-
  tbl(con, "film") %>%
  filter(release_year == 2006) %>%
  select(film_id, title, description)
head(lazy_df, 3)

## -----------------------------------------------------------------------------
dbDisconnect(con)

