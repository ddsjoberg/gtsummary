## Initial version by Linlin Yan <linlin.yan@cos.name>
## URL: http://cos.name/cn/topic/101199
## Modified by Yihui Xie <yihui.xie@cos.name>

hanoi <- function(n, color = rainbow(n), interval = 0.5, ...) {
  tower <- list(1:n, NULL, NULL)
  draw.hanoi <- function() {
    par(mfrow = c(1, 3), mar = rep(1, 4), ann = FALSE, xaxt = 'n', yaxt = 'n')
    for (i in 1:3) {
      plot(c(-n, n), c(0, n + 2), type = 'n')
      if (length(tower[[i]]) > 0) {
        barplot(rev(tower[[i]]), add = TRUE, horiz = TRUE,
                col = color[rev(tower[[i]])], ...)
        barplot(-rev(tower[[i]]), add = TRUE, horiz = TRUE,
                col = color[rev(tower[[i]])], ...)
      }
    }
  }
  move.hanoi <- function(k, from, to, via) {
    if (k > 1) {
      move.hanoi(k - 1, from, via, to)
      move.hanoi(1, from, to, via)
      move.hanoi(k - 1, via, to, from)
    }
    else {
      cat('Move ', tower[[from]][1], ' from ', LETTERS[from],
          ' to ', LETTERS[to], '\n')
      flush.console()
      tower[[to]] <<- c(tower[[from]][1], tower[[to]])
      tower[[from]] <<- tower[[from]][-1]
      dev.hold()
      draw.hanoi()
      ani.pause()
    }
  }
  draw.hanoi()
  move.hanoi(n, 1, 2, 3)
}

x11(width = 7, height = 4)
hanoi(7)

# faster, more layers
hanoi(10, interval = 0.1)
