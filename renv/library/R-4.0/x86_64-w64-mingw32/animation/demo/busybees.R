####################################################################
# This has reminded me of bees I saw on my way to school in the
# village when I was a little child...
####################################################################

local({
  # number of bees
  n = 50
  # maximum number of frames
  nmax = 3000
  # locations
  x = rnorm(n)
  y = rnorm(n)
  # directions
  rx = sample(c(-1, 1), n, TRUE)
  ry = sample(c(-1, 1), n, TRUE)
  # range of the square
  r = 20
  op = par(pch = 19)
  for (i in 1:nmax) {
    dev.hold()
    rx[x >= r] = -1
    rx[x <= -r] = 1
    ry[y >= r] = -1
    ry[y <= -r] = 1
    x = x + abs(rnorm(n)) * rx
    y = y + abs(rnorm(n)) * ry
    plot(x, y, xlim = c(-r, r), ylim = c(-r, r), col = rainbow(n))
    dev.flush()
  }
  par(op)
})
