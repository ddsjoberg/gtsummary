## generate leaves by recursion
## author: Taiyun Wei <taiyun.wei@cos.name>
recur.leaf <- function(n = 1e+05, ...) {
  x <- c(0.5, 0.5)
  plot(x[1], x[2], xlim = c(-3, 3), ylim = c(0, 10), type = 'n', 
       ann = FALSE, axes = FALSE)
  p <- c(0.85, 0.92, 0.99, 1)
  A <- rbind(c(0.85, 0.04), c(-0.04, 0.85), c(0.2, -0.26), 
             c(0.23, 0.22), c(-0.15, 0.28), c(0.26, 0.24), c(0, 0), 
             c(0, 0.16))
  B <- cbind(c(0, 1.6), c(0, 1.6), c(0, 0.44), c(0, 0))
  
  for (i in 1:n) {
    ran <- runif(1)
    ind <- rank(c(p, ran), ties.method = 'min')[5]
    x <- A[(2 * ind - 1):(2 * ind), ] %*% x + B[, ind]
    points(x[1], x[2], pch = '.', ...)
  }
}
recur.leaf(col = 'green') 
