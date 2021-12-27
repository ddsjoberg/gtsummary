## A demo on how to record GGobi plots into an animation
##
## The animation example is taken from:
## Hadley Wickham, Michael Lawrence, Duncan Temple Lang, and Deborah F. Swayne.
##     An introduction to rggobi. R News, 8(2):3-7, October 2008.

library(animation)
if (require('rggobi')) {
  saveHTML({
    n = 100
    df = data.frame(x=1:n, y=sin(1:n * pi/20) + runif(n, max=0.5))
    g = ggobi_longitudinal(df[1:80, ])
    df_g = g[1]
    for(i in 1:21) {
      df_g[, 2] = df[i:(i + 79), 2]
      ggobi_display_save_picture(path = sprintf(ani.options('img.fmt'), i),
                                 plot.only = TRUE)
    }
    close(g)
  }, use.dev = FALSE, img.name = 'ggobi_image', htmlfile = 'ggobi_animation.html',
           ani.width = 352, interval = 0.5,
           description = 'Screenshots taken from GGobi.')
}
## Note ani.width might vary accross different operating systems (I got 352 under Ubuntu)
