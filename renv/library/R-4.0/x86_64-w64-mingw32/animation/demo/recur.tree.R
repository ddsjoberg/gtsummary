## generate a tree by recursion
## author: Taiyun Wei <taiyun.wei@cos.name>
recur.tree <- function(x1, y1, x2, y2, n, xlim = c(-1, 
                                                   1), ylim = c(0, 2), col = 'blue', add = FALSE) {
  if (!add) 
    plot(0, 0, xlim = xlim, ylim = ylim, type = 'n', xlab = '', 
         ylab = '', asp = 1, ann = FALSE, axes = FALSE)
  tree <- function(x1, y1, x2, y2, n) {
    flag <- 0
    theta <- pi/6
    if (x2 < x1) 
      flag <- 1
    if (n > 1) {
      tree(x1, y1, (2 * x1 + x2)/3, (2 * y1 + y2)/3, n - 1)
      tree((2 * x1 + x2)/3, (2 * y1 + y2)/3, (2 * x2 + x1)/3, (2 * y2 + y1)/3, n - 1)
      tree((2 * x2 + x1)/3, (2 * y2 + y1)/3, x2, y2, n - 1)
      tree((2 * x1 + x2)/3, (2 * y1 + y2)/3, 
           (2 * x1 + x2)/3 +
             sin(pi/2 - atan((y2 - y1)/(x2 - x1)) - theta + flag * pi) * sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3),
           (2 * y1 + y2)/3 +
             cos(pi/2 - atan((y2 - y1)/(x2 - x1)) - theta + flag * pi) * sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3),
           n - 1)
      tree((2 * x2 + x1)/3, (2 * y2 + y1)/3,
           (2 * x2 + x1)/3 + sin(pi/2 - atan((y2 - y1)/(x2 - x1)) + theta + flag * pi) *
             sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3),
           (2 * y2 + y1)/3 + cos(pi/2 - atan((y2 - y1)/(x2 - x1)) + theta + flag * pi) *
             sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3),
           n - 1)
    }
    else {
      x <- c(x1, x2)
      y <- c(y1, y2)
      xx <- c((2 * x1 + x2)/3,
              (2 * x1 + x2)/3 + sin(pi/2 - atan((y2 - y1)/(x2 - x1)) - theta + flag * pi) * 
                sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3))
      yy <- c((2 * y1 + y2)/3,
              (2 * y1 + y2)/3 + cos(pi/2 - atan((y2 - y1)/(x2 - x1)) - theta + flag * pi) * 
                sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3))
      xxx <<- c((2 * x2 + x1)/3,
                (2 * x2 + x1)/3 + sin(pi/2 - atan((y2 - y1)/(x2 - x1)) + theta + flag * pi) * 
                  sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3))
      yyy <<- c((2 * y2 + y1)/3,
                (2 * y2 + y1)/3 + cos(pi/2 - atan((y2 - y1)/(x2 - x1)) + theta + flag * pi) * 
                  sqrt(((y2 - y1)^2 + (x2 - x1)^2)/3))
      lines(x, y, type = 'l', col = col)
      lines(xx, yy, type = 'l', col = col)
      lines(xxx, yyy, type = 'l', col = col)
    }
  }
  tree(x1, y1, x2, y2, n)
}

## example
for (i in 1:5) {
  recur.tree(0, 0, 0, 1.5, i, col = i)
  Sys.sleep(1)
}
## plot.tree(0.6, 0, 0.6, 0.4, 4, add = TRUE, col = 'red') 
