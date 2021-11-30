
##########
### 3D HIST EXAMPLE:
##########

################################################################################

##### Required functions 'binplot' and 'hist3d':

binplot.3d<-function(x,y,z,alpha=1,topcol="#ff0000",sidecol="#aaaaaa") {
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
    
  x1<-c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
  z1<-c(rep(0,4),rep(c(0,0,z,z),4))
  y1<-c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
  x2<-c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
  z2<-c(rep(c(0,z),4),rep(0,8),rep(z,8) )
  y2<-c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )
  rgl.quads(x1,z1,y1,col=rep(sidecol,each=4),alpha=alpha)
  rgl.quads(c(x[1],x[2],x[2],x[1]),rep(z,4),c(y[1],y[1],y[2],y[2]),
              col=rep(topcol,each=4),alpha=1) 
  rgl.lines(x2,z2,y2,col="#000000")
}

hist3d<-function(x,y=NULL,nclass="auto",alpha=1,col="#ff0000",scale=10) {
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
  xy <- xy.coords(x,y)
  x <- xy$x
  y <- xy$y
  n<-length(x)
  if (nclass == "auto") nclass<-ceiling(sqrt(nclass.Sturges(x)))
  breaks.x <- seq(min(x),max(x),length=(nclass+1))
  breaks.y <- seq(min(y),max(y),length=(nclass+1))
  z<-matrix(0,(nclass),(nclass))
  for (i in seq_len(nclass)) {
    for (j in seq_len(nclass)) {
      z[i, j] <- (1/n)*sum(x < breaks.x[i+1] & y < breaks.y[j+1] & 
                            x >= breaks.x[i] & y >= breaks.y[j])
      binplot.3d(c(breaks.x[i],breaks.x[i+1]),c(breaks.y[j],breaks.y[j+1]),
                 scale*z[i,j],alpha=alpha,topcol=col)
    }
  }
}
################################################################################

rgl.open()

rgl.bg(color="gray")
rgl.light()

# Drawing a 'bin' for given coordinates:
binplot.3d(c(-0.5,0.5),c(4.5,5.5),2,alpha=0.6)

# Setting the viewpoint ('theta' and 'phi' have the same meaning as in persp):
rgl.viewpoint(theta=40,phi=40)

# Choosing a lightgrey background:
rgl.bg(col="#cccccc")


##### QUADS FORMING BIN:

rgl.open()

# Defining transparency and colors:
alpha<-0.7; topcol<-"#ff0000"; sidecol<-"#aaaaaa"

# Setting up coordinates for the quads and adding them to the scene:
y<-x<-c(-1,1) ; z<-4; of<-0.3
x12<-c(x[1],x[2],x[2],x[1]); x11<-rep(x[1],4); x22<-rep(x[2],4)
z00<-rep(0,4); z0z<-c(0,0,z,z); zzz<-rep(z,4); y11<-rep(y[1],4)
y1122<-c(y[1],y[1],y[2],y[2]); y12<-c(y[1],y[2],y[2],y[1]); y22<-rep(y[2],4)

rgl.quads(c(x12,x12,x11-of,x12,x22+of,x12),
          c(z00-of,rep(z0z,4),zzz+of),
          c(y1122,y11-of,y12,y22+of,y12,y1122),
          col=rep(c(rep(sidecol,5),topcol),each=4),alpha=c(rep(alpha,5),1))

# Setting up coordinates for the border-lines of the quads and drawing them:
yl1<-c(y[1],y[2],y[1],y[2]); yl2<-c(y[1]-of,y[1]-of)
xl<-c(rep(x[1],8),rep(x[1]-of,8),rep(c(x[1],x[2]),8),rep(x[2],8),rep(x[2]+of,8))
zl<-c(0,z,0,z,z+of,z+of,-of,-of,0,0,z,z,0,z,0,z,rep(0,4),rep(z,4),rep(-of,4),
      rep(z+of,4),z+of,z+of,-of,-of,rep(c(0,z),4),0,0,z,z)
yl<-c(yl2,y[2]+of,y[2]+of,rep(c(y[1],y[2]),4),y[1],y[1],y[2],y[2],yl2,
      rep(y[2]+of,4),yl2,y[2],y[2],rep(y[1],4),y[2],y[2],yl1,yl2,y[2]+of,
      y[2]+of,y[1],y[1],y[2],y[2],yl1)

rgl.lines(xl,zl,yl,col="#000000")


##### COMPLETE HISTOGRAM:

rgl.open()

# Choosing a lightgrey background:
rgl.bg(col="#cccccc")

# Setting the rng to a fixed value:
set.seed(1000)

# Drawing a 3d histogramm of 2500 normaly distributed observations:
hist3d(rnorm(2500),rnorm(2500),alpha=0.4,nclass=7,scale=30)
