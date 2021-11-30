## demo on how to insert animations in Sweave

## copy the Rnw file to temp dir
file.copy(system.file('misc', 'Sweave_animation.Rnw', package = 'animation'), tempdir())

owd = setwd(tempdir())

## run Sweave
Sweave('Sweave_animation.Rnw')


## this may not be necessary, if you have the LaTeX package 'animate' installed
file.copy(system.file('misc', 'animate', 'animate.sty', package = 'animation'),
          'animate.sty', overwrite = TRUE)
file.copy(system.file('misc', 'animate', 'animfp.sty', package = 'animation'),
          'animfp.sty', overwrite = TRUE)

## compile the tex document: it may take a while
tools::texi2dvi('Sweave_animation.tex', pdf = TRUE)

system(sprintf('%s %s', shQuote(normalizePath(getOption('pdfviewer'))),
               'Sweave_animation.pdf'))

setwd(owd)
