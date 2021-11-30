## generate a snow flake by recursion
## author: Taiyun Wei <taiyun.wei@cos.name>
recur.snow <- function(k, col = 'blue') {
  plot(0, 0, xlim = c(0, 1), ylim = c(-sqrt(3)/6, sqrt(3)/2), 
       asp = 1, type = 'n', ann = FALSE, axes = FALSE)
  plotkoch <- function(x1, y1, x2, y2, n) {
    if (n > 1) {
      plotkoch(x1, y1, (2 * x1 + x2)/3, (2 * y1 + y2)/3, n - 1)
      plotkoch((2 * x1 + x2)/3, (2 * y1 + y2)/3, 
               (x1 + x2)/2 - (y1 - y2) * sqrt(3)/6,
               (y1 + y2)/2 - (x2 - x1) * sqrt(3)/6, n - 1)
      plotkoch((x1 + x2)/2 - (y1 - y2) * sqrt(3)/6,
               (y1 + y2)/2 - (x2 - x1) * sqrt(3)/6, (2 * x2 + x1)/3, 
               (2 * y2 + y1)/3, n - 1)
      plotkoch((2 * x2 + x1)/3, (2 * y2 + y1)/3, x2, y2, n - 1)
    }
    else {
      x = c(x1, (2 * x1 + x2)/3, (x1 + x2)/2 - (y1 - y2) * sqrt(3)/6, (2 * x2 + x1)/3, x2)
      y = c(y1, (2 * y1 + y2)/3, (y1 + y2)/2 - (x2 - x1) * sqrt(3)/6, (2 * y2 + y1)/3, y2)
      lines(x, y, type = 'l', col = col)
    }
  }
  plotkoch(0, 0, 1, 0, k)
  plotkoch(0.5, sqrt(3)/2, 0, 0, k)
  plotkoch(1, 0, 0.5, sqrt(3)/2, k)
}

## example
for (i in 1:5) {
  recur.snow(i, col = i)
  Sys.sleep(1)
} 
