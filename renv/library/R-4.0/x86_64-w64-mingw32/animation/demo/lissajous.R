## Animate a series of lissajous. 
## This script will generate an animated gif in your Working Directory
## Author: Rob Steele <robsteele at yahoo dot com>
## URL: https://github.com/yihui/animation/issues/71#issuecomment-162857831


library(animation)


plot.lissajous <- function(omega.x, omega.y, delta = 0, num.thetas = 200, ...)
{
  thetas <- seq(from = 0, to = 2 * pi, length.out = num.thetas)
  xs <- sin(omega.x * thetas + delta)
  ys <- cos(omega.y * thetas)
  plot(x = xs, y = ys, type = "l", ann = FALSE, axes = FALSE, ...)
}


create.lissajous.gif <- function(gif.name          = "lissajous.gif",
                                 frames.per.second = 24,
                                 loop.length       = 3,   ## in seconds
                                 max.degree        = 5,
                                 line.width        = 3,
                                 image.dir         = tempdir())
{
  ## Create animation frames.
  num.frames <- round(loop.length * frames.per.second)
  deltas <- head(seq(from = 0, to = 2 * pi, length.out = num.frames + 1), -1)
  
  image.files <-
    sapply(X = seq_len(num.frames), FUN = function(i) {
      image.file <- file.path(image.dir, sprintf("img-%04d.png", i))
      png(file = image.file)
      par(mar   = c(1, 1, 1, 1),
          mfcol = c(max.degree, max.degree))
      for (omega.x in seq_len(max.degree)) {
        for (omega.y in seq_len(max.degree)) {
          plot.lissajous(omega.x = omega.x, omega.y = omega.y, delta = deltas[i], lwd = line.width)
        }
      }
      dev.off()
      return(image.file)
    })
  
  ## Combine the individual image files into an animated GIF file.
  oopt <- ani.options(interval   = 1 / frames.per.second,
                      loop       = TRUE,
                      autobrowse = TRUE)
  
  im.convert(files = file.path(image.dir, "img-*.png"), output = gif.name)
  
  ## Restore animation options.
  ani.options(oopt)
}


create.lissajous.gif()
