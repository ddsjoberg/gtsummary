## ----setup, include = FALSE---------------------------------------------------
knitr::opts_knit$set(root.dir = tempdir())
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 3)

## -----------------------------------------------------------------------------
library(vroom)

## -----------------------------------------------------------------------------
# See where the example file is stored on your machine
file <- vroom_example("mtcars.csv")
file

# Read the file, by default vroom will guess the delimiter automatically.
vroom(file)

# You can also specify it explicitly, which is (slightly) faster, and safer if
# you know how the file is delimited.
vroom(file, delim = ",")

## -----------------------------------------------------------------------------
mt <- tibble::rownames_to_column(mtcars, "model")
purrr::iwalk(
  split(mt, mt$cyl),
  ~ vroom_write(.x, glue::glue("mtcars_{.y}.csv"), "\t")
)

## -----------------------------------------------------------------------------
files <- fs::dir_ls(glob = "mtcars*csv")
files
vroom(files)

## -----------------------------------------------------------------------------
vroom(files, id = "path")

## ---- include = FALSE---------------------------------------------------------
# just to clear .Last.value
1 + 1
gc()
unlink(files)

## -----------------------------------------------------------------------------
file <- vroom_example("mtcars.csv.gz")

vroom(file)

## -----------------------------------------------------------------------------
read_all_zip <- function(file, ...) {
  filenames <- unzip(file, list = TRUE)$Name
  vroom(purrr::map(filenames, ~ unz(file, .x)), ...)
}

## ---- eval = as.logical(Sys.getenv("NOT_CRAN", "false"))----------------------
#  file <- "https://raw.githubusercontent.com/r-lib/vroom/master/inst/extdata/mtcars.csv"
#  vroom(file)

## ---- eval = as.logical(Sys.getenv("NOT_CRAN", "false"))----------------------
#  file <- "https://raw.githubusercontent.com/r-lib/vroom/master/inst/extdata/mtcars.csv.gz"
#  vroom(file)

## -----------------------------------------------------------------------------
file <- vroom_example("mtcars.csv.gz")

vroom(file, col_select = c(model, cyl, gear))

## -----------------------------------------------------------------------------
vroom(file, col_select = c(1, 3, 11))

## -----------------------------------------------------------------------------
vroom(file, col_select = starts_with("d"))

## -----------------------------------------------------------------------------
vroom(file, col_select = list(car = model, everything()))

## -----------------------------------------------------------------------------
fwf_sample <- vroom_example("fwf-sample.txt")
cat(readLines(fwf_sample))

## -----------------------------------------------------------------------------
vroom_fwf(fwf_sample, fwf_empty(fwf_sample, col_names = c("first", "last", "state", "ssn")))

## -----------------------------------------------------------------------------
vroom_fwf(fwf_sample, fwf_widths(c(20, 10, 12), c("name", "state", "ssn")))

## -----------------------------------------------------------------------------
vroom_fwf(fwf_sample, fwf_positions(c(1, 30), c(20, 42), c("name", "ssn")))

## -----------------------------------------------------------------------------
vroom_fwf(fwf_sample, fwf_cols(name = 20, state = 10, ssn = 12))

## -----------------------------------------------------------------------------
vroom_fwf(fwf_sample, fwf_cols(name = c(1, 20), ssn = c(30, 42)))

## -----------------------------------------------------------------------------
# read the 'hp' columns as an integer
vroom(vroom_example("mtcars.csv"), col_types = c(hp = "i"))

# also skip reading the 'cyl' column
vroom(vroom_example("mtcars.csv"), col_types = c(hp = "i", cyl = "_"))

# also read the gears as a factor
vroom(vroom_example("mtcars.csv"), col_types = c(hp = "i", cyl = "_", gear = "f"))

## -----------------------------------------------------------------------------
vroom(vroom_example("mtcars.csv"), col_types = c(.default = "c"))

## -----------------------------------------------------------------------------
vroom(
  vroom_example("mtcars.csv"),
  col_types = list(hp = col_integer(), cyl = col_skip(), gear = col_factor())
)

## -----------------------------------------------------------------------------
vroom(
  vroom_example("mtcars.csv"),
  col_types = list(gear = col_factor(levels = c(gear = c("3", "4", "5"))))
)

## ---- eval = FALSE------------------------------------------------------------
#  vroom(
#    vroom_example("mtcars.csv"),
#    .name_repair = ~ janitor::make_clean_names(., case = "all_caps")
#  )

## -----------------------------------------------------------------------------
vroom_write(mtcars, "mtcars.tsv")

## ---- include = FALSE---------------------------------------------------------
unlink("mtcars.tsv")

## -----------------------------------------------------------------------------
vroom_write(mtcars, "mtcars.csv", delim = ",")

## ---- include = FALSE---------------------------------------------------------
unlink("mtcars.csv")

## -----------------------------------------------------------------------------
vroom_write(mtcars, "mtcars.tsv.gz")

vroom_write(mtcars, "mtcars.tsv.bz2")

vroom_write(mtcars, "mtcars.tsv.xz")

## ---- include = FALSE---------------------------------------------------------
unlink(c("mtcars.tsv.gz", "mtcars.tsv.bz2", "mtcars.tsv.xz"))

## ---- eval = nzchar(Sys.which("pigz"))----------------------------------------
vroom_write(mtcars, pipe("pigz > mtcars.tsv.gz"))

## ---- include = FALSE---------------------------------------------------------
unlink("mtcars.tsv.gz")

