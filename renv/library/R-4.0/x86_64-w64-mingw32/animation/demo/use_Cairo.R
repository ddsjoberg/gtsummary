## use the (high-quality) Cairo device

library(animation)
library(Cairo)

saveHTML({
  ani.options(interval = 0.05, nmax = 1)
  
  CairoPNG(filename = ani.options('img.fmt'))
  
  par(pty = 's', mar = rep(1, 4))
  vi.lilac.chaser()
  
  dev.off()
}, img.name='chaser_cairo', htmlfile='chaser.html',
         use.dev=FALSE, ani.type='png',
         description = 'Using the high-quality CairoPNG() device')


saveHTML({
  ani.options(interval = 0.05, nmax = 1)
  
  par(pty = 's', mar = rep(1, 4))
  vi.lilac.chaser()
  
}, img.name='chaser_png', htmlfile='chaser.html',
         use.dev=TRUE, ani.dev='png', ani.type='png',
         description = c("Using R's png() device; can you see the difference?",
                         'Note the borders of the dots.'))
