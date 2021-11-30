# Merry Xmas 2009!
# see: http://yihui.name/en/2009/12/merry-christmas-using-r/
n = length(speed <- runif(angle <- runif(
  x <- strsplit('MERRY CHRISTMAS', '')[[1]], 0, 360), 0, 15))
x11(width = 10, height = 3)
par(mar = rep(0, 4), bg = 'black')
for (j in 1:1000) {
  angle = angle + speed
  dev.hold()
  plot.new()
  plot.window(c(1, n), c(0, 1))
  for (i in 1:n) text(i, 0.5, x[i], srt = angle[i],
                      cex = runif(1, 1, 4), col = sample(colors(), 1))
  ani.pause(0.02)
}
