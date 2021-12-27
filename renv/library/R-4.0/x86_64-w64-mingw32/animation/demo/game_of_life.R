## Game of Life
## Author: Linlin Yan <linlin.yan@cos.name>; modified by Yihui Xie
## URL: http://cos.name/cn/topic/15402

library(animation)
oopt = ani.options()

run_life <- function(file, generation = ani.options('nmax'), fg = 'black', bg = 'transparent', wrap = TRUE) {
  
  load_life <- function(file) {
    s <- readLines(file)
    n <- as.integer(strsplit(s[1], ' ')[[1]])
    w <- n[1]; h <- n[2]; x <- n[3]; y <- n[4]
    s <- strsplit(s[-1], '')
    m <- matrix(0, w, h)
    for (i in seq_along(s)) {
      for (j in seq_along(s[[i]])) {
        yy <- y + i - 1
        xx <- x + j - 1
        if (yy > 0 && yy < h && xx > 1 && xx < w) {
          m[xx, yy] <- (s[[i]][j] != ' ')
        }
      }
    }
    return (m)
  }
  
  next_generation <- function(m) {
    if (wrap) {
      m <- m[c(ncol(m), 1:ncol(m), 1), c(nrow(m), 1:nrow(m), 1)]
    } else {
      m <- cbind(0, rbind(0, m, 0), 0)
    }
    x0 <- c(-2, -1)
    x1 <- c(-1, -ncol(m))
    x2 <- c(-(ncol(m) - 1), -ncol(m))
    y0 <- c(-2, -1)
    y1 <- c(-1, -nrow(m))
    y2 <- c(-(nrow(m) - 1), -nrow(m))
    f <- function(a, b) { b == 3 | (b == 2 & a == 1) }
    m <- f(m[y1, x1],
           m[y0, x0] + m[y0, x1] + m[y0, x2] +
             m[y1, x0]             + m[y1, x2] +
             m[y2, x0] + m[y2, x1] + m[y2, x2])
    return(m)
  }
  
  draw_life <- function(m) {
    par(mar = rep(0.1, 4))
    image(m, col = c(bg, fg), axes = FALSE)
    box()
  }
  
  m <- load_life(file)
  for (i in 1:generation) {
    dev.hold()
    draw_life(m)
    m <- next_generation(m)
    ani.pause()
  }
}

ani.options(ani.width=600, ani.height=600)

saveHTML({
  life_demo = textConnection(
    '50 50 5 45
111
  1
 1')
  ani.options(interval = 0.05, nmax = 165)
  run_life(life_demo)
  close(life_demo)
}, img.name='life_demo', htmlfile='life_demo.html', description = 'The first demo.')

saveHTML({
  ani.options(interval = 0.05, nmax = 300)
  life_gun = textConnection('100 100 5 5
                        *
                      * *
            **      **            **
           *   *    **            **
**        *     *   **
**        *   * **    * *
          *     *       *
           *   *
            **')
  run_life(life_gun)
  close(life_gun)
}, img.name='life_gun', htmlfile='life_gun.html',
         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0",
         description = "Gosper's Glider Gun!")


## puffer train
life_train = textConnection('200 200 50 50
*  *
    *
*   *
 ****

*
 **
  *
  *
 *

*  *
    *
*   *
 ****')
ani.options(interval = 0.01, nmax = 3000)
run_life(life_train)
close(life_train)

ani.options(oopt)
