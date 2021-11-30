#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source_file <- args[[1]]
out_file <- args[[2]]

file <- args[-c(1:2)]

cat(source_file, "\n")
out <- bench::workout_expressions(as.list(parse(source_file, keep.source = FALSE)))

x <- vroom::vroom(file, col_types = list())

out$size <- sum(file.size(file))
out$rows <- nrow(x)
out$cols <- ncol(x)
out$process <- as.numeric(out$process)
out$real <- as.numeric(out$real)
out$max_memory <- as.numeric(bench::bench_process_memory()[["max"]])

vroom::vroom_write(out, out_file)
