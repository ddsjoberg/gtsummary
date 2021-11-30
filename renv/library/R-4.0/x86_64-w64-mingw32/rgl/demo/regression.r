# demo: regression
# author: Daniel Adler

rgl.demo.regression <- function(n=100,xa=3,za=8,xb=0.02,zb=0.01,xlim=c(0,100),zlim=c(0,100)) {

  rgl.clear("all")
  rgl.bg(sphere = TRUE, color = c("black", "green"), lit = FALSE, size=2, alpha=0.2, back = "lines")
  rgl.light()
  rgl.bbox()

  x  <- runif(n,min=xlim[1],max=xlim[2])
  z  <- runif(n,min=zlim[1],max=zlim[2])
  ex <- rnorm(n,sd=3)
  ez <- rnorm(n,sd=2)
  
  esty  <- (xa+xb*x) * (za+zb*z) + ex + ez

  rgl.spheres(x,esty,z,color="gray",radius=1.5,specular="green",
              texture=system.file("textures/bump_dust.png",package="rgl"),
              texmipmap=TRUE, texminfilter="linear.mipmap.linear")

  regx  <- seq(xlim[1],xlim[2],len=100)
  regz  <- seq(zlim[1],zlim[2],len=100)
  regy  <- (xa+regx*xb) %*% t(za+regz*zb)
  
  rgl.surface(regx,regz,regy,color="blue",alpha=0.5,shininess=128)

  lx <- c(xlim[1],xlim[2],xlim[2],xlim[1])
  lz <- c(zlim[1],zlim[1],zlim[2],zlim[2])
  f <- function(x,z) (xa+x*xb) * t(za+z*zb)
  ly <- f(lx,lz)
  
  rgl.quads(lx,ly,lz,color="red",size=5,front="lines",back="lines",lit=FALSE)
}

rgl.open()
rgl.demo.regression()
