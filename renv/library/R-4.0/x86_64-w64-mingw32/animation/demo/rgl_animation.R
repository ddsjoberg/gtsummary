## how to use saveLatex() with the rgl package

library(animation)

if (require('rgl')) {
  saveLatex({
    ## ajust the view
    uM =
      matrix(c(-0.37, -0.51, -0.77, 0, -0.73, 0.67,
               -0.10, 0, 0.57, 0.53, -0.63, 0, 0, 0, 0, 1),
             nrow = 4, ncol = 4)
    
    ## note the width and height are 500px respectively
    open3d(userMatrix = uM, windowRect = c(30, 30, 530, 530))
    plot3d(pollen[, 1:3])
    zm = seq(1, .05, length = 20)
    par3d(zoom = 1)
    
    ## the most important argument is use.dev = FALSE!
    ## and the file extension should be 'png', since we are using rgl.snapshot()
    for (i in 1:length(zm)) {
      par3d(zoom = zm[i])
      rgl.snapshot(sprintf(ani.options('img.fmt'), i))
    }
    rgl.close()
  }, img.name='pollen_demo', use.dev=FALSE,
            ani.type='png', interval = .2,
            ani.opts = 'controls,width=3in',
            latex.filename='rgl_pollen_animation.tex',
            caption = 'Zoom in a 3D scatter plot to see the truth.',
            overwrite = FALSE)
  
} else warning('You have to install the rgl package first: install.packages("rgl")')

message('This demo needs rgl >= 0.92.798 and R >= 2.12.1')
