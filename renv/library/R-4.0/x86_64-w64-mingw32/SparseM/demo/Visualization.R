data(triogramX)
image(X)
cat("Hit RETURN to continue\n");readLines(n=1)
image(t(X)%*%X)
