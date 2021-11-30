({})
x <- read.delim(file, sep = ",", quote = "", na.strings = NULL, stringsAsFactors = FALSE)
print(head(x, 10))
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$payment_type == "UNK", ]
e <- tapply(x$tip_amount, x$payment_type, mean)
