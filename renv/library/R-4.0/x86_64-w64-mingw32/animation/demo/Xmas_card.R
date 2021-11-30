## Christmas and New Year card
## Author: Yuan Huang @ PSU

dev.new(title = 'Happy New Year!', height = 7, width = 7)

par(mar = c(0.01, 0.01, 0.01, 0.01), oma = c(0, 0, 0, 0))
t <- seq(-90, 90, by = 1)
a <- 2
xx <- a * (2 * cos(t) - cos(2 * t))
yy <- a * (2 * sin(t) - sin(2 * t))

plot(yy, xx, type = 'l', col = colors()[51], lwd = 2, axes = F)
mtext(paste('From Someone', 'O(^_^)O', '@ Somewhere'), cex = 1.5,
      line = -1.4, side = SOUTH <- 1, adj = 0, outer = TRUE, col = colors()[134])
mtext(paste(format(Sys.time(), '%Y-%m-%d')), cex = 1.5, line = -1.4,
      side = SOUTH <- 1, adj = 1, outer = TRUE, col = colors()[134])
grid.x <- seq(range(yy)[1], range(yy)[2], length = 20)
grid.y <- seq(range(xx)[1], range(xx)[2], length = 20)

image(x = grid.x, y = grid.y, z = matrix(rnorm(400), 20),
      col = heat.colors(20, alpha = 0.5), add = T)
text(quantile(yy, 0.5) - 0.5, max(xx) - 1.5, 'Dear_____:', cex = 2.5,
     col = colors()[512])
cc <- colors()[c(367, 419, 473, 464, 456, 503, 552, 91, 616, 432,
                 618, 367)]
l.cc <- length(cc)
ccc <- colors()[c(0, 15, 519, 520, 1, 3, 640, 399, 64, 419, 536, 400)]
l.ccc <- length(ccc)
a <- runif(50, range(yy)[1], range(yy)[2])
b <- runif(50, range(xx)[1], range(xx)[2])

for (i in 1:102) {
  text(quantile(yy, 0.5), max(xx) - 3, 'Merry Christmas!', col = cc[i%%l.cc],
       lwd = 5, cex = 2.5)
  text(quantile(yy, 0.5), max(xx) - 3.8, 'and', col = cc[i%%l.cc], lwd = 5,
       cex = 2.5)
  text(quantile(yy, 0.5), max(xx) - 4.8, 'Happy New Year~', col = cc[i%%l.cc],
       lwd = 5, cex = 2.5)
  points(a, b, pch = 8, lwd = 3, col = ccc[i%%l.ccc])
  Sys.sleep(0.1)
}
