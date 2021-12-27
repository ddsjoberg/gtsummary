({})
x <- read.delim(file, quote = "", na.strings = NULL, stringsAsFactors = FALSE)
print(head(x, 10))
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$X1 > 3, ]
e <- tapply(x$X1, as.integer(x$X2), mean)
