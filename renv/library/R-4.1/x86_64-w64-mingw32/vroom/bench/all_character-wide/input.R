args <- commandArgs(trailingOnly = TRUE)
rows <- as.integer(args[[1]])
cols <- as.integer(args[[2]])
output <- args[[3]]

set.seed(42)
RNGversion("3.5.3")

library(vroom)

# We want ~ 1000 rows to filter
num_levels <- 5
levels <- c("helpless_sheep", gen_name(num_levels - 1))

filt_p <- 1000 / rows

# The prob for the rest should just be evenly spaced
rest_p <- rep((1 - filt_p) / (num_levels - 1), num_levels - 1)

col_types <-  stats::setNames(
  c(list(
      col_factor(levels = levels, prob = c(filt_p, rest_p))),
    rep(list(col_character()), cols - 1)
  ), make.names(seq_len(cols)))

data <- gen_tbl(rows, cols, col_types = col_types)

vroom_write(data, output, "\t")
