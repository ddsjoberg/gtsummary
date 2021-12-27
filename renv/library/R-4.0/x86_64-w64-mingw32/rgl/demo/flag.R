
wave <- function(time) {
  x <- seq(0,2, len=100)
  
  wavefn <- function(x) x * sin(-5*time + 1.5 * (x/2) * 2*pi)/10
  deriv <- function(x) (wavefn(x + 0.01) - wavefn(x - 0.01))/0.02

  arclen <- cumsum(sqrt(1 + deriv(x)^2))*(x[2]-x[1])
  keep <- arclen < 2
  x <- x[keep]
  y <- matrix(wavefn(x), length(x),20)
  z <- matrix(seq(0,1, len=20), length(x), 20, byrow=TRUE)
  arclen <- arclen[keep]
  
  par3d(skipRedraw = TRUE)
  if (nrow(ids3d())) pop3d()
  surface3d(x,y,z, texture_s=matrix(arclen/2, length(x), 20), texture_t=z, col="white")
  c(list(skipRedraw = FALSE), spin(time))
}

open3d()
material3d(texture = system.file("textures","rgl2.png", package="rgl"))
spin <- spin3d(rpm=6,axis=c(0,0,1))
if (!rgl.useNULL())
  play3d(wave, 10, startTime = 5)
