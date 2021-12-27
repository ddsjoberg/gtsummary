{
  library(vroom)
  data <- vroom(file, col_types = c(pickup_datetime = "c"))
  vroom:::vroom_materialize(data, replace = TRUE)
}

{
  con <- gzfile(tempfile(fileext = ".gz"), "wb")
  write.table(data, con, sep = "\t", quote = FALSE, row.names = FALSE)
  close(con)
}
