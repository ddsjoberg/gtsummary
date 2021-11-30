({})
x <- read.delim(file, quote = "", na.strings = NULL, stringsAsFactors = FALSE)
print(head(x, 10))
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$X1 == "helpless_sheep", ]
e <- tapply(x$X2, x$X1, function(x) mean(nchar(x)))
