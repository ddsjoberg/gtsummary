#####################################################################
# Sierpinski triangle in R
# by Yihui Xie @ 2009-04-03
# see http://en.wikipedia.org/wiki/Sierpinski_triangle for details
#####################################################################

# a random start point in the triangle 
x = runif(1)
y = runif(1, 0, sqrt(3)/2)
while (!(y <= sqrt(3) * x & y <= -sqrt(3) * (x - 1))) {
  x = runif(1)
  y = runif(1, 0, sqrt(3)/2)
}
# set up the 'canvas'
par(mar = rep(0, 4), pty = 's')
plot(0, type = 'n', xlim = c(0, 1), ylim = c(0, sqrt(3)/2),
     ann = FALSE, axes = FALSE)
vx = c(0, 1, 0.5)
vy = c(0, 0, sqrt(3)/2)
# draw points
for (i in 1:20000) {
  idx = sample(3, 1)
  points(x <- (x + vx[idx])/2, y <- (y + vy[idx])/2, pch = '.')
}
