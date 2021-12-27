library(vroom)
x <- vroom(file, trim_ws = FALSE, quote = "", escape_double = FALSE, na = character(), altrep = FALSE)
print(x)
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$X1 > 3, ]
e <- tapply(x$X1, as.integer(x$X2), mean)
