## ----setup, echo=FALSE, results="asis"----------------------------------------
source("setup.R")
setupKnitr(autoprint = TRUE)
set.seed(123)

## -----------------------------------------------------------------------------
with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                  type="s", col=as.numeric(Species)))

## ----persp3d, fig.height=3, fig.width=6, fig.keep="last"----------------------
library(MASS)
# from the fitdistr example
set.seed(123)
x <- rgamma(100, shape = 5, rate = 0.1)
fit <- fitdistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)
loglik <- function(shape, rate) sum(dgamma(x, shape=shape, rate=rate, 
                                           log=TRUE))
loglik <- Vectorize(loglik)
xlim <- fit$estimate[1]+4*fit$sd[1]*c(-1,1)
ylim <- fit$estimate[2]+4*fit$sd[2]*c(-1,1)

mfrow3d(1, 2, sharedMouse = TRUE)
persp3d(loglik, 
        xlim = xlim, ylim = ylim,
        n = 30)
zlim <- fit$loglik + c(-qchisq(0.99, 2)/2, 0)
next3d()
persp3d(loglik, 
        xlim = xlim, ylim = ylim, zlim = zlim,
        n = 30)

## -----------------------------------------------------------------------------
methods(plot3d)
methods(persp3d)

## ----fig.width=3, fig.height=3------------------------------------------------
triangles3d(cbind(x=rnorm(9), y=rnorm(9), z=rnorm(9)), col = "green")
decorate3d()
bg3d("lightgray")
aspect3d(1,1,1)

## -----------------------------------------------------------------------------
filename <- tempfile(fileext = ".png")
png(filename = filename)
plot(rnorm(1000), rnorm(1000))
dev.off()
open3d()
xyz <- cbind(c(0,1,1,0), 0, c(0,0,1,1))
quads3d(xyz, texture = filename, texcoords = xyz[,c(1, 3)]*2, col = "white", specular = "black")

## -----------------------------------------------------------------------------
cols <- rainbow(7)
layout3d(matrix(1:16, 4,4), heights=c(1,3,1,3))
text3d(0,0,0,"tetrahedron3d"); next3d()
shade3d(tetrahedron3d(col=cols[1])); next3d()
text3d(0,0,0,"cube3d"); next3d()
shade3d(cube3d(col=cols[2])); next3d()
text3d(0,0,0,"octahedron3d"); next3d()
shade3d(octahedron3d(col=cols[3])); next3d()
text3d(0,0,0,"dodecahedron3d"); next3d()
shade3d(dodecahedron3d(col=cols[4])); next3d()
text3d(0,0,0,"icosahedron3d"); next3d()
shade3d(icosahedron3d(col=cols[5])); next3d()
text3d(0,0,0,"cuboctahedron3d"); next3d()
shade3d(cuboctahedron3d(col=cols[6])); next3d()
text3d(0,0,0,"oh3d"); next3d()
shade3d(oh3d(col=cols[7]))

## -----------------------------------------------------------------------------
xyz <- matrix(rnorm(27), ncol = 3)
triangles3d(xyz, col = rainbow(9))
spheres3d(xyz, col = rainbow(9), radius = 0.1)

## ----eval = FALSE-------------------------------------------------------------
#  plots <- NULL
#  for (i in 1:3) {
#    plot3d(rnorm(10), rnorm(10), rnorm(10))
#    plots <- htmltools::tagList(plots, rglwidget())
#    close3d()
#  }
#  plots

## ----eval = FALSE-------------------------------------------------------------
#  foreignHigh()   # Produces a high level plot, but doesn't return
#                  # an appropriate value
#  highlevel()
#  foreignLow()    # Modifies the previous plot
#  lowlevel()

## -----------------------------------------------------------------------------
par3d("mouseMode")

## ----echo = 2:3---------------------------------------------------------------
close3d()
persp3d(volcano, col = "green")

## -----------------------------------------------------------------------------
lattice::wireframe(volcano, col = "green", 
		   screen = rglToLattice())
angles <- rglToBase()
persp(volcano, col = "green", shade = TRUE,
      theta = angles$theta, phi = angles$phi)

## ----echo=FALSE---------------------------------------------------------------
setdiff(ls("package:rgl"), documentedfns)

## ----echo=FALSE, results="asis"-----------------------------------------------
writeIndex(cols = if (knitr::is_html_output()) 5 else 4)

