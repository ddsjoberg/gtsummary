library(animation)

## save the game to an HTML page
saveHTML({
  n = nrow(CLELAL09)
  
  ## draw the basketball court
  draw.court = function() {
    rect(0, 0, 94, 50)
    circle = function(x, y, r, from = 0, to = 2 * pi, lines = FALSE, ...) {
      theta = seq(from, to, length = 100)
      if (lines)
        lines(x + r * cos(theta), y + r * sin(theta), ...)
      else polygon(x + r * cos(theta), y + r * sin(theta), ...)
    }
    points(c(5.25, 94 - 5.25), c(25, 25), cex = 2)
    segments(47, 0, 47, 50)
    circle(47, 25, 8)
    circle(47, 25, 2, col = 'lightgray')
    theta1 = acos((25 - 35/12)/23.75)
    circle(5.25, 25, 23.75, -pi/2 + theta1, pi/2 - theta1, TRUE)
    circle(94 - 5.25, 25, 23.75, pi/2 + theta1, 3 * pi/2 - theta1, TRUE)
    segments(0, 35/12, 5.25 + 23.75 * sin(theta1), 35/12)
    segments(0, 50 - 35/12, 5.25 + 23.75 * sin(theta1), 50 - 35/12)
    segments(94, 35/12, 94 - 5.25 - 23.75 * sin(theta1), 35/12)
    segments(94, 50 - 35/12, 94 - 5.25 - 23.75 * sin(theta1), 50 - 35/12)
    circle(19, 25, 6, -pi/2, pi/2, TRUE)
    circle(19, 25, 6, pi/2, 3 * pi/2, TRUE, lty = 2)
    circle(94 - 19, 25, 6, pi/2, 3 * pi/2, TRUE)
    circle(94 - 19, 25, 6, -pi/2, pi/2, TRUE, lty = 2)
    circle(5.25, 25, 4, -pi/2, pi/2, TRUE)
    circle(94 - 5.25, 25, 4, pi/2, 3 * pi/2, TRUE)
    rect(0, 17, 19, 33, border = 'gray')
    rect(94, 17, 94 - 19, 33, border = 'gray')
  }
  
  with(CLELAL09, {
    jitterx = jitter(realx, amount = 1)
    jittery = jitter(realy, amount = 1)
    
    for (i in 1:n) {
      plot.new()
      par(mar = c(0.05, 1, 0.05, 1), xpd = TRUE)
      plot.window(c(0, 94), c(0, 50), asp = 1)
      draw.court()
      
      text(0, 25, 'CLE', adj = c(0.5, 0), srt = 90)
      text(94, 25, 'LAL', adj = c(0.5, 1), srt = 90)
      points(jitterx[1:i], jittery[1:i], pch = c(1, 4)[as.integer(result[1:i])])
      text(jitterx[i], jittery[i], player[i], adj = c(1, 0), cex = 1.3)
      points(jitterx[i], jittery[i], pch = c(1, 4)[as.integer(result[i])],
             cex = 2, lwd = 2)
      abline(v = realx[i], h = realy[i], lty = 2, col = 'gray')
      text(47, 0, sprintf('Period %s; Time: %s', period[i], time[i]),
           adj = c(0.5, 1))
      
      cat(i, '/', n, '\r')
      flush.console()
      ani.pause()
    }
  })
}, ani.width = 752, ani.height = 400, title = 'CLE vs LAL on Dec 25, 2009',
         description = 'The NBA game on 2009 Christmas. O: made; X: missed.')
