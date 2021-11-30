# RGL-Demo: animal abundance
# Authors: Oleg Nenadic, Daniel Adler

rgl.demo.abundance <- function() {
  open3d()
  clear3d("all")               # remove all shapes, lights, bounding-box, and restore viewpoint
  
  # Setup environment:
  bg3d(col="#cccccc")     # setup background
  light3d()               # setup head-light
  
  # Importing animal data (created with wisp)
  terrain<-dget(system.file("demodata/region.dat",package="rgl"))
  pop<-dget(system.file("demodata/population.dat",package="rgl"))
  
  # Define colors for terrain
  zlim <- range(terrain)
  colorlut <- terrain.colors(82) 
  col1 <- colorlut[9*sqrt(3.6*(terrain-zlim[1])+2)]
  
  # Set color to (water-)blue for regions with zero 'altitude' 
  col1[terrain==0]<-"#0000FF"
  
  # Add terrain surface shape (i.e. population density):
  surface3d( 
      1:100,seq(1,60,length=100),terrain,
      col=col1,spec="#000000", ambient="#333333", back="lines"
  )
  
  # Define colors for simulated populations (males:blue, females:red):
  col2<-pop[,4]
  col2[col2==0]<-"#3333ff"
  col2[col2==1]<-"#ff3333"
  
  # Add simulated populations as sphere-set shape
  spheres3d(
    pop[,1],
    pop[,2],
    terrain[cbind( ceiling(pop[,1]),ceiling(pop[,2]*10/6) )]+0.5,
    radius=0.2*pop[,3], col=col2, alpha=(1-(pop[,5])/10 )
  )

}
rgl.demo.abundance()
