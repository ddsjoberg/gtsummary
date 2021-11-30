library(vroom)
x <- vroom(file, trim_ws = FALSE, quote = "", escape_double = FALSE, na = character())
print(x)
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$X1 == "helpless_sheep", ]
e <- tapply(x$X2, x$X1, function(x) mean(nchar(x)))
