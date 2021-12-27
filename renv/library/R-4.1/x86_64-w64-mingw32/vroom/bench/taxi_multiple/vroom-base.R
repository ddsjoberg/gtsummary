library(vroom)
x <- vroom(file, id = "path", col_types = c(pickup_datetime = "c"), trim_ws = FALSE, quote = "", escape_double = FALSE, na = character(), altrep = TRUE)
print(head(x, 10))
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[x$payment_type == "UNK", ]
e <- tapply(x$tip_amount, x$payment_type, mean)
