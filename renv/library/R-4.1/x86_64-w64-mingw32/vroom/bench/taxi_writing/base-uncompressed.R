{
  library(vroom)
  data <- vroom(file, col_types = c(pickup_datetime = "c"))
  vroom:::vroom_materialize(data, replace = TRUE)
}

write.table(data, tempfile(fileext = ".tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
