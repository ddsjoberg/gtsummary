## Set fireworks using R
## Author: Weicheng Zhu
## URL: http://cos.name/cn/topic/103272/

library(animation)
saveHTML({
  fire <- function(
    centre = c(0, 0), r = 1:5, theta = seq(0, 2 * pi, length = 100),
    l.col = rgb(1, 1, 0), lwd = 5, ...
  ) {
    x <- centre[1] + outer(r, theta, function(r, theta) r * sin(theta))
    y <- centre[2] + outer(r, theta, function(r, theta) r * cos(theta))
    matplot(x, y, type = 'l', lty = 1, col = l.col, add = T, lwd = lwd, ...)
  }
  
  f <- function(
    centre = rbind(c(-7, 7), c(7, 6)), n = c(7, 5), N = 20, l.col = c('rainbow', 'green'),
    p.col = 'red', lwd = 5, ...
  ) {
    # n stands for how many 'leaves' does the firework have.
    # l.col is the color of the 'leaves' which can be chosen from 'red','green','blue' and 'rainbow'(default).
    # p.col is the cntre point color.
    ani.options(interval = 0.1)
    lwd = lwd
    if (is.vector(centre) && length(n) == 1) {
      r = 1:n
      l = seq(0.1, 0.6, length = n)
      matplot(centre[1], centre[2], col = p.col, ...)
      for (r in r) {
        fire(
          centre = centre, r = seq(r - l[r], r + l[r], length = 10),
          theta = seq(0, 2 * pi, length = 10 * r) + 1, l.col = rainbow(n)[r],
          lwd = lwd, ...)
      }
    } else {
      matplot(centre[, 1], centre[, 2], col = p.col, ...)
      l = list()
      for (i in 1:length(n)) l[i] = list(seq(0.1, 0.6, length = n[i]))
      if (length(l.col) == 1)
        l.col = rep(l.col, length(n))
      r = 1:N
      for (r in r) {
        for (j in 1:length(n)) {
          if (r%%(n[j] + 1) == 0) {
            r1 = 1:n[j]
            l1 = seq(0.1, 0.6, length = n[j])
            for (r1 in r1) {
              fire(
                centre = centre[j, ], r = seq(r1 - l1[r1], r1 + l1[r1], length = 10),
                theta = seq(0, 2 * pi, length = 10 * r1) + 1, l.col = par('bg'),
                lwd = lwd + 2)
            }
          } else {
            if (l.col[j] == 'red')
              fire(
                centre = centre[j, ], 
                r = seq(r%%(n[j] + 1) - l[[j]][r%%(n[j] + 1)], r%%(n[j] + 1) + l[[j]][r%%(n[j] + 1)], length = 10),
                theta = seq(0, 2 * pi, length = 10 * r%%(n[j] + 1)) + 1, l.col = rgb(1, r%%(n[j] + 1)/n[j], 0),
                lwd = lwd, ...)
            else if (l.col[j] == 'green')
              fire(
                centre = centre[j, ],
                r = seq(r%%(n[j] + 1) - l[[j]][r%%(n[j] + 1)], r%%(n[j] + 1) + l[[j]][r%%(n[j] + 1)], length = 10),
                theta = seq(0, 2 * pi, length = 10 * r%%(n[j] + 1)) + 1, l.col = rgb(1 - r%%(n[j] + 1)/n[j], 1, 0),
                lwd = lwd, ...)
            else if (l.col[j] == 'blue')
              fire(
                centre = centre[j, ],
                r = seq(r%%(n[j] + 1) - l[[j]][r%%(n[j] + 1)], r%%(n[j] + 1) + l[[j]][r%%(n[j] + 1)], length = 10),
                theta = seq(0, 2 * pi, length = 10 * r%%(n[j] + 1)) + 1, l.col = rgb(r%%(n[j] + 1)/n[j], 0, 1),
                lwd = lwd, ...)
            else
              fire(
                centre = centre[j, ],
                r = seq(r%%(n[j] + 1) - l[[j]][r%%(n[j] + 1)], r%%(n[j] + 1) + l[[j]][r%%(n[j] + 1)], length = 10),
                theta = seq(0, 2 * pi, length = 10 * r%%(n[j] + 1)) + 1, l.col = rainbow(n[j])[r%%(n[j] + 1)],
                lwd = lwd, ...)
          }
          ani.pause()
        }
      }
    }
  }
  card <- function(N = 20, p.col = 'green', bgcolour = 'black', lwd = 5, ...) {
    ani.options(interval = 1)
    for (i in 1:N) {
      par(ann = F, bg = bgcolour, mar = rep(0, 4), pty = 's')
      f(N = i, lwd = lwd, ...)
      text(0, 0, 'Happy New Year', srt = 360 * i/N, col = rainbow(N)[i], cex = 4.5 * i/N)
      ani.pause()
    }
  }
  ani.options(interval = 0.2)
  card(N = 30, centre = rbind(c(-8, 8), c(8, 10), c(5, 0)), n = c(9, 5, 6), pch = 8,
       p.col = 'green', l.col = c('rainbow', 'red', 'green'), xlim = c(-12, 12), ylim = c(-12, 12))
}, img.name = 'fireworks_b', htmlfile = 'fireworks.html', ani.height = 500, ani.width = 600,
         title = 'Set Fireworks Using R', description = 'Fireworks demo by Weicheng Zhu')
