# demo: lsystem.r
# author: Daniel Adler

#
# geometry 
#

deg2rad <- function( degree ) {
  return( degree*pi/180 )
}

rotZ.m3x3 <- function( degree ) {
  kc <- cos(deg2rad(degree))
  ks <- sin(deg2rad(degree))
  return( 
    matrix(
      c(
        kc, -ks,   0, 
        ks,  kc,   0,
         0,   0,   1
      ),ncol=3,byrow=TRUE
    ) 
  )
}

rotX.m3x3 <- function( degree ) {
  kc <- cos(deg2rad(degree))
  ks <- sin(deg2rad(degree))
  return(
    matrix(
      c(
         1,   0,   0,
         0,  kc, -ks,
         0,  ks,  kc
      ),ncol=3,byrow=TRUE
    )
  )
}

rotY.m3x3 <- function( degree ) {
  kc <- cos(deg2rad(degree))
  ks <- sin(deg2rad(degree))
  return(
    matrix(
      c(
        kc,   0,   ks,
         0,   1,    0,
       -ks,   0,   kc
      ),ncol=3,byrow=TRUE
    )
  )
}

rotZ <- function( v, degree ) {
  return( rotZ.m3x3(degree) %*% v)
}

rotX <- function( v, degree ) {
  return( rotX.m3x3(degree) %*% v)
}

rotY <- function( v, degree ) {
  return( rotY.m3x3(degree) %*% v)
}


#
# turtle graphics, rgl implementation:
#

turtle.init <- function(pos=c(0,0,0),head=0,pitch=90,roll=0,level=0) {
  rgl.clear("all")
  rgl.bg(color="black")
  rgl.light()
  return( list(pos=pos,head=head,pitch=pitch,roll=roll,level=level) )
}


turtle.move <- function(turtle, steps, color) {
  
  rm <- rotX.m3x3(turtle$pitch) %*% rotY.m3x3(turtle$head) %*% rotZ.m3x3(turtle$roll)
  
  from <- as.vector( turtle$pos )  
  dir  <- rm %*% c(0,0,-1)
  to   <- from + dir * steps
    
  x <- c( from[1], to[1] )
  y <- c( from[2], to[2] )
  z <- c( from[3], to[3] )
  rgl.lines(x,y,z,col=color,size=1.5,alpha=0.5)
  turtle$pos <- to
  return(turtle)
}

turtle.pitch <- function(turtle, degree) {
  turtle$pitch <- turtle$pitch + degree
  return(turtle)
}

turtle.head <- function(turtle, degree) {
  turtle$head <- turtle$head + degree
  return(turtle)
}

turtle.roll <- function(turtle, degree) {
  turtle$roll <- turtle$roll + degree
  return(turtle)
}

#
# l-system general
#


lsystem.code <- function( x )
  substitute( x )

lsystem.gen <- function( x, grammar, levels=0 ) {
  code <- eval( substitute( substitute( REPLACE , grammar ), list(REPLACE=x) ) )
  if (levels)
    return( lsystem.gen( code , grammar , levels-1 ) )
  else
    return( code )
}

#
# l-system plot
#

lsystem.plot <- function( expr, level ) {
  turtle <- turtle.init(level=level)
  lsystem.eval( expr, turtle )
}

lsystem.eval <- function( expr, turtle ) {
  if ( length(expr) == 3 ) {
    turtle <- lsystem.eval( expr[[2]], turtle )
    turtle <- lsystem.eval( expr[[3]], turtle )
    turtle <- lsystem.eval( expr[[1]], turtle )
  } else if ( length(expr) == 2 ) {
    saved <- turtle
    turtle <- lsystem.eval( expr[[1]], turtle )
    turtle <- lsystem.eval( expr[[2]], turtle )
    turtle <- saved
  } else if ( length(expr) == 1 ) {
    if ( as.name(expr) == "stem" )      turtle <- turtle.move(turtle, 5, "brown")
    else if ( as.name(expr) == "short") turtle <- turtle.move(turtle, 5, "brown")
    else if ( as.name(expr) == "leaf" ) {
      rgl.spheres(turtle$pos[1],turtle$pos[2],turtle$pos[3],radius=0.1+turtle$level*0.3,color="green")
      rgl.sprites(turtle$pos[1],turtle$pos[2],turtle$pos[3],radius=0.5+turtle$level*0.3 ,color="green",texture=system.file("textures/particle.png",package="rgl"),textype="alpha",alpha=0.5)     
    }
    else if ( as.name(expr) == "roll" ) turtle <- turtle.head(turtle, 60)
    else if ( as.name(expr) == "down" ) turtle <- turtle.pitch(turtle,10)
    else if ( as.name(expr) == "up" )   turtle <- turtle.pitch(turtle,-10)
    else if ( as.name(expr) == "left" ) turtle <- turtle.head(turtle, 1)
    else if ( as.name(expr) == "right") turtle <- turtle.head(turtle,-1.5)
    else if ( as.name(expr) == "turnleft") turtle <- turtle.head(turtle,20)
    else if ( as.name(expr) == "turnright") turtle <- turtle.head(turtle,-20)
    else if ( as.name(expr) == "turn") turtle <- turtle.roll(turtle,180)
  }
  return(turtle)
}


#
# example
#

simple <- function(level=0) {
  grammar <- list(
    stem=lsystem.code(
      stem-(up-stem-leaf)-stem-(down-stem-leaf)-stem-leaf
    )
  )
  plant <- lsystem.gen(lsystem.code(stem), grammar, level )
  lsystem.plot(plant,level)
}

rgl.demo.lsystem <- function(level=0) {
  gen   <- list(
    stem=lsystem.code( 
      stem-left-stem-branch( turnleft-down-short-turnleft-down-stem-leaf)-right-right-stem--branch( turnright-up-short-turnright-up-short-turnright-short-stem-leaf)-left-left-left-stem-branch( turnleft-down-short-turnright-down-stem-leaf )-branch( up-turnright-short-up-turnleft-up-stem-leaf ) 
    )
  )    
  plant <- lsystem.gen(lsystem.code(stem), gen, level )
  lsystem.plot(plant,level)  
}

rgl.open()
rgl.demo.lsystem(level=1)
