{
  library(vroom)
  data <- vroom(file, col_types = c(pickup_datetime = "c"))
  vroom:::vroom_materialize(data, replace = TRUE)
}

data.table::fwrite(data, tempfile(fileext = ".gz"), sep = "\t", nThread = 1)
