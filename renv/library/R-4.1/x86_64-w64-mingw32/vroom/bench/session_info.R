#!/usr/bin/env Rscript

vroom::vroom_write(
  sessioninfo::package_info(c("vroom", "readr", "dplyr", "data.table", "base"), dependencies = FALSE, include_base = TRUE),
  here::here("inst", "bench", "session_info.tsv"),
  delim = "\t"
)
