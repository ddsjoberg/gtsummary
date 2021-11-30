require(Hmisc)
x <- 1:10
y <- (11:20) + .1
label(y) <- 'Y'
attributes(y)
d <- data.frame(x, y)
attributes(d$y)
m <- model.frame(y ~ x, data=d)
m$y
attributes(m$y)
mr <- model.response(m)
attributes(mr)
mr
