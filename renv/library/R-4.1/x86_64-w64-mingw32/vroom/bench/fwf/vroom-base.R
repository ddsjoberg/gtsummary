library(vroom)
x <- vroom_fwf(file, fields, col_types = types)
print(head(x, 10))
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$PERSONS== "06", ]
e <- tapply(x$TOTPUMA5, x$STATE, mean)
