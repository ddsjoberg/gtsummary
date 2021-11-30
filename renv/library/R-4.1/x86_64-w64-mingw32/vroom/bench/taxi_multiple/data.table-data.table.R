library(data.table)
x <- rbindlist(idcol = "path",
  lapply(stats::setNames(file, file), fread, sep = ",", quote = "", strip.white = FALSE, na.strings = NULL)
)
print(x)
a <- head(x)
b <- tail(x)
c <- x[sample(NROW(x), 100), ]
d <- x[payment_type == "UNK", ]
e <- x[ , .(mean(tip_amount)), by = payment_type]
