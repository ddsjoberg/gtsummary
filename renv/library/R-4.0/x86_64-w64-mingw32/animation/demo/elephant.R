## A Winking Pink Elephant by Neil Gunther (with very slight modifications)
## URL: http://perfdynamics.blogspot.com/2011/06/winking-pink-elephant.html
param <- c(50-30i,18+8i,12-10i,-14-60i,1+20i)
parar <- param * exp(1i*pi/2)  #rotate by 90 degrees
pinky <- function() {
  Cx <- as.complex(rep(0,length(param)))
  Cy <- as.complex(rep(0,length(param)))
  tv <- seq(0,2*pi,length=1000)
  
  for (i in 1:2) { #movie frames
    Cx[1] <- parar[1] + Im(param[1])
    Cx[2] <- parar[2] + Im(param[2])
    Cx[3] <- Re(param[3])
    Cx[4] <- Re(param[5]) - (i-1)
    Cx[5] <- Re(param[4])
    
    Cy[1] <- param[1] - Re(param[1]) + Im(param[4])
    Cy[2] <- param[2] - Re(param[2])
    Cy[3] <- param[3] - Re(param[3])
    
    x <- c(fourier(tv, Cx))
    y <- c(fourier(tv, Cy))
    
    dev.hold()
    plot(y, -x, type='l', col='red', lwd=10, axes=FALSE, ylab='', xlab='')
    lines(y, -x, type='l', col='pink', lwd=4)
    if (i > 1) points(Im(param[5]), Im(param[5]), col='black', pch=126, cex=2)
    else points(Im(param[5]), Im(param[5]), col='black', pch=20, cex=2)
    ani.pause(.25)
  }
}
fourier <- function(tt,cc) {
  wt <- rep(0, length(tt))
  fsum <- function(n) {
    if (n > 0) wt <- wt + fsum(n-1) + Re(cc[n]) * cos(n*tt) + Im(cc[n]) * sin(n*tt)
    return(wt)
  }
  fsum(length(cc))
}

message('Press Esc under Windows or Ctrl-C under *nix to stop the animation')
while (TRUE) pinky()
