# RGL-Demo: environment mapping
# Author: Daniel Adler

rgl.demo.envmap <- function() {
  open3d()
  # Clear scene:
  clear3d("all")
  light3d()
  bg3d(sphere=TRUE, color="white", back="filled"
  , texture=system.file("textures/refmap.png",package="rgl")
  )
  data(volcano)

  surface3d( 10*seq_len(nrow(volcano)),10*seq_len(ncol(volcano)),5*volcano
  , texture=system.file("textures/refmap.png",package="rgl")
  , texenvmap=TRUE
  , color = "white"
  )
}
rgl.demo.envmap()
