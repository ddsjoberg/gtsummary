library(vroom)
x <- vroom(file, col_types = c(pickup_datetime = "c"), trim_ws = FALSE, quote = "", escape_double = FALSE, na = character())
print(x)
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$payment_type == "UNK", ]
e <- tapply(x$tip_amount, x$payment_type, mean)
