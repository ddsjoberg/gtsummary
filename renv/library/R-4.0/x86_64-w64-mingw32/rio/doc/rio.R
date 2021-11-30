## ---- echo=FALSE, results='hide'----------------------------------------------
library("rio")

export(mtcars, "mtcars.csv")
export(mtcars, "mtcars.rds")
export(mtcars, "mtcars.dta")
export(mtcars, "mtcars_noext", format = "csv")

## -----------------------------------------------------------------------------
library("rio")

x <- import("mtcars.csv")
y <- import("mtcars.rds")
z <- import("mtcars.dta")

# confirm identical
all.equal(x, y, check.attributes = FALSE)
all.equal(x, z, check.attributes = FALSE)

## -----------------------------------------------------------------------------
head(import("mtcars_noext", format = "csv"))

## ---- echo=FALSE, results='hide'----------------------------------------------
unlink("mtcars.csv")
unlink("mtcars.rds")
unlink("mtcars.dta")
unlink("mtcars_noext")

## -----------------------------------------------------------------------------
library("rio")

export(mtcars, "mtcars.csv")
export(mtcars, "mtcars.rds")
export(mtcars, "mtcars.dta")

## -----------------------------------------------------------------------------
library("magrittr")
mtcars %>% subset(hp > 100) %>%  aggregate(. ~ cyl + am, data = ., FUN = mean) %>% export(file = "mtcars2.dta")

## -----------------------------------------------------------------------------
# export to sheets of an Excel workbook
export(list(mtcars = mtcars, iris = iris), "multi.xlsx")

## -----------------------------------------------------------------------------
# export to an .Rdata file
## as a named list
export(list(mtcars = mtcars, iris = iris), "multi.rdata")

## as a character vector
export(c("mtcars", "iris"), "multi.rdata")

## -----------------------------------------------------------------------------
export_list(list(mtcars = mtcars, iris = iris), "%s.tsv")

## -----------------------------------------------------------------------------
# create file to convert
export(mtcars, "mtcars.dta")

# convert Stata to SPSS
convert("mtcars.dta", "mtcars.sav")

## -----------------------------------------------------------------------------
# create an ambiguous file
fwf <- tempfile(fileext = ".fwf")
cat(file = fwf, "123456", "987654", sep = "\n")

# see two ways to read in the file
identical(import(fwf, widths = c(1,2,3)), import(fwf, widths = c(1,-2,3)))

# convert to CSV
convert(fwf, "fwf.csv", in_opts = list(widths = c(1,2,3)))
import("fwf.csv") # check conversion

## ---- echo=FALSE, results='hide'----------------------------------------------
unlink("mtcars.dta")
unlink("mtcars.sav")
unlink("fwf.csv")
unlink(fwf)

## ---- echo=FALSE, results='hide'----------------------------------------------
unlink("mtcars.csv")
unlink("mtcars.rds")
unlink("mtcars.rdata")
unlink("mtcars.dta")
unlink("multi.xlsx")
unlink("multi.rdata")
unlink("mtcars2.dta")
unlink("mtcars.tsv")
unlink("iris.tsv")

