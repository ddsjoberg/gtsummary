library(data.table)
x <- fread(file, sep = "\t", quote = "", strip.white = FALSE, na.strings = NULL)
print(x)
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[X1 > 3, ]
e <- x[ , .(mean(X1)), by = as.integer(X2)]
