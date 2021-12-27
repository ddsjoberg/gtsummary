{
  library(vroom)
  data <- vroom(file, col_types = c(pickup_datetime = "c"))
  vroom:::vroom_materialize(data, replace = TRUE)
}

readr::write_tsv(data, pipe(sprintf("pigz > %s", tempfile(fileext = ".gz"))))
