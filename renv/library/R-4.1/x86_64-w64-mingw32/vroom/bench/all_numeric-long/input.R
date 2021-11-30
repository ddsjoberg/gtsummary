args <- commandArgs(trailingOnly = TRUE)
rows <- as.integer(args[[1]])
cols <- as.integer(args[[2]])
output <- args[[3]]

set.seed(42)
RNGversion("3.5.3")

data <- vroom::gen_tbl(rows, cols, col_types = strrep("d", cols))

vroom::vroom_write(data, output, "\t")
