## ----setup, include=FALSE-----------------------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available("1.14")) {
  warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14.  These were not found. Older versions will not work.")
  knitr::knit_exit()
}
knitr::opts_chunk$set(echo = TRUE)
library(rgl)
options(rgl.useNULL = TRUE)
setupKnitr(autoprint = TRUE)
M <- structure(c(0.997410774230957, 0.0707177817821503, -0.0130676832050085, 
0, -0.0703366547822952, 0.99714070558548, 0.02762770652771, 0, 
0.0149840852245688, -0.0266370177268982, 0.999532878398895, 0, 
0, 0, 0, 1), .Dim = c(4L, 4L))

## -----------------------------------------------------------------------------
theta <- 2*pi*c(0:2, 4:6, 8:10)/12
x <- cos(theta)
y <- sin(theta)
z <- rep(c(0,0,1), 3)
xyz <- cbind(x, y, z)
xyz <- xyz[c(1,2,6, 4,5,9, 7,8,3),]
open3d()
par3d(userMatrix = M)
triangles3d(xyz, col = rep(c("red", "green", "blue"), each = 3))

## ----fig.width=8--------------------------------------------------------------
open3d()
par3d(userMatrix = M)
layout3d(matrix(1:9, ncol = 3, byrow=TRUE),
         widths = c(1,2,2), heights = c(1, 3,3), 
         sharedMouse = TRUE)
text3d(0,0,0, " ")
next3d()
text3d(0,0,0, "depth_mask = TRUE")
next3d()
text3d(0,0,0, "depth_mask = FALSE")
next3d()
text3d(0,0,0, "alpha = 0.7")
next3d()
triangles3d(xyz, col = rep(c("red", "green", "blue"), each = 3), alpha = 0.7, depth_mask = TRUE)
next3d()
triangles3d(xyz, col = rep(c("red", "green", "blue"), each = 3), alpha = 0.7, depth_mask = FALSE)
next3d()
text3d(0,0,0, "alpha = 0.3")
next3d()
triangles3d(xyz, col = rep(c("red", "green", "blue"), each = 3), alpha = 0.3, depth_mask = TRUE)
next3d()
triangles3d(xyz, col = rep(c("red", "green", "blue"), each = 3), alpha = 0.3, depth_mask = FALSE)

## -----------------------------------------------------------------------------
open3d()
par3d(userMatrix = M)
triangles3d(xyz[1:3,], col = "red", alpha = 0.3, depth_mask = FALSE)
triangles3d(xyz[4:6,], col = "green", alpha = 0.7, depth_mask = TRUE)
triangles3d(xyz[7:9,], col = "blue", depth_mask = TRUE)

## ----fig.width=8--------------------------------------------------------------
open3d()
par3d(userMatrix = M)
layout3d(matrix(1:9, ncol = 3, byrow=TRUE),
         widths = c(1,2,2), heights = c(1, 3,3), 
         sharedMouse = TRUE)
text3d(0,0,0, " ")
next3d()
text3d(0,0,0, "depth_mask = TRUE")
next3d()
text3d(0,0,0, "depth_mask = FALSE")
next3d()
text3d(0,0,0, "alpha = 0.7")
next3d()
triangles3d(xyz, col = "red", alpha = 0.7, depth_mask = TRUE)
next3d()
triangles3d(xyz, col = "red", alpha = 0.7, depth_mask = FALSE)
next3d()
text3d(0,0,0, "alpha = 0.3")
next3d()
triangles3d(xyz, col = "red", alpha = 0.3, depth_mask = TRUE)
next3d()
triangles3d(xyz, col = "red", alpha = 0.3, depth_mask = FALSE)

