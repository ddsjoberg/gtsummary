## Merry Christmas with snowflakes
## Author: Jing Jiao; modified by Yihui Xie
## URL: http://cos.name/cn/topic/103272
library(animation)
oopt = ani.options(interval = 0.1)
pp <- function(N) {
  x1 <- runif(N, 0, N)
  y1 <- runif(N, 0, N)
  par(ann = F, bg = 'darkblue', pch = 8, mar = rep(0, 4))
  x <- seq(1, 30 * N)
  j <- sample(x, 30)
  plot(1, ann = F, type = 'n', axes = F, xlim = c(0, N), ylim = c(0, N))
  points(j, N - 1.5 * rep(1, 30), col = 'white', cex = 2)
  interval = ani.options('interval')
  for (i in 2:N) {
    x <- seq(1, 30 * N)
    x <- x[-j]
    j <- c(sample(x, 30), j)
    plot(1, ann = F, type = 'n', axes = F, xlim = c(0, N), ylim = c(0, N))
    y <- N - 1.5 * rep(1:i, rep(30, i))
    points(j, y, col = 'white', cex = 2)
    z <- sample(N, length(x1), replace = T)
    points(x1[i], y1[i], col = N - i, cex = 3)
    points(x1[i + 1], y1[i + 1], col = N - i - 1, cex = 2.5)
    text(N/2, N/2, 'Merry Christmas', srt = 360 * i/N, col = rainbow(N)[i], cex = 4.5 * i/N)
    ani.pause()
  }
}
pp(100)
## use saveGIF(pp(100), interval = 0.1) to export a GIF animation
ani.options(oopt)
