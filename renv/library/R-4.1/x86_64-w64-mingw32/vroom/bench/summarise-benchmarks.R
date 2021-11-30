library(vroom)
library(dplyr)
library(fs)
library(purrr)
library(tidyr)

summarise_dir <- function(dir, desc) {
  out_file <- path(path_dir(dir), path_ext_set(path_file(dir), "tsv"))
  col_types <- cols(
    exprs = col_character(),
    process = col_character(),
    real = col_character(),
    size = col_double(),
    rows = col_double(),
    cols = col_double()
  )

  dir_ls(dir, glob = "*tsv") %>%
    discard(~endsWith(.x, "input.tsv")) %>%
    vroom(id = "path", col_types = col_types) %>%
    mutate(path = path_ext_remove(path_file(path))) %>%
    group_by(path) %>%
    mutate(op = desc) %>%
    separate(path, c("reading_package", "manip_package"), "-") %>%
    pivot_longer(., cols = c(process, real), names_to = "type", values_to = "time") %>%
    select(reading_package, manip_package, op, type, time, size, max_memory, rows, cols) %>%
    vroom_write(out_file, delim = "\t")
}

summarise_dir(here::here("inst/bench/all_numeric-long"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
summarise_dir(here::here("inst/bench/all_numeric-wide"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
summarise_dir(here::here("inst/bench/all_character-long"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
summarise_dir(here::here("inst/bench/all_character-wide"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
summarise_dir(here::here("inst/bench/taxi"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
summarise_dir(here::here("inst/bench/taxi_multiple"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
summarise_dir(here::here("inst/bench/taxi_writing"), c("setup", "writing"))
summarise_dir(here::here("inst/bench/fwf"), c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate"))
