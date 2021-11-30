# Simple example of using GdkRGB with RGtk2

IMAGE_WIDTH	<- 256
IMAGE_HEIGHT <- 256


rgb_example <- function()
{
  window <- gtkWindow("toplevel", show = F)
  darea <- gtkDrawingArea()
  darea$setSizeRequest(IMAGE_WIDTH, IMAGE_HEIGHT)
  window$add(darea)
  
  # Set up the RGB buffer.
  x <- rep(0:(IMAGE_WIDTH-1), IMAGE_HEIGHT)
  y <- rep(0:(IMAGE_HEIGHT-1), IMAGE_WIDTH, each = T)
  red <- x - x %% 32
  green <- (x / 32) * 4 + y - y %% 32
  blue <- y - y %% 32
  buf <- rbind(red, green, blue)
  
  # connect to expose event
  gSignalConnect(darea, "expose-event", on_darea_expose, buf)
  
  window$showAll()
}


on_darea_expose <- function(widget, event, buf)
{
  gdkDrawRgbImage(widget[["window"]],
                  widget[["style"]][["fgGc"]][[GtkStateType["normal"]+1]],
                  0, 0, IMAGE_WIDTH, IMAGE_HEIGHT, "max", buf, IMAGE_WIDTH * 3)
}
