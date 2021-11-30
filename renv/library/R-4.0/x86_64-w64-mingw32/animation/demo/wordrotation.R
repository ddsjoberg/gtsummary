# the simplest animation: rotate the word
for (i in 1:360) {
  dev.hold()
  plot(1, ann = FALSE, type = 'n', axes = FALSE)
  text(1, 1, 'Animation', srt = i, col = rainbow(360)[i], cex = 7 * i/360)
  ani.pause(0.01)
}
