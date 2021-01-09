## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(DBI)

con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
con

## -----------------------------------------------------------------------------
dbListTables(con)

## -----------------------------------------------------------------------------

dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)


## -----------------------------------------------------------------------------
dbListFields(con, "mtcars")

## -----------------------------------------------------------------------------
dbReadTable(con, "mtcars")

## -----------------------------------------------------------------------------
df <- dbGetQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
df

## -----------------------------------------------------------------------------
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
df <- dbFetch(res)
dbClearResult(res)
df

## -----------------------------------------------------------------------------
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
while(!dbHasCompleted(res)){
  chunk <- dbFetch(res, n = 5)
  print(nrow(chunk))
}

## -----------------------------------------------------------------------------
dbClearResult(res)
dbDisconnect(con)

