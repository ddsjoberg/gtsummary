{
  library(vroom)
  data <- vroom(file, col_types = c(pickup_datetime = "c"))
  vroom:::vroom_materialize(data, replace = TRUE)
}

vroom_write(data, tempfile(fileext = ".tsv"), delim = "\t")
