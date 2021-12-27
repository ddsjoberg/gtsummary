expose_event_callback <- function(widget, event, data) {
  gdkDrawArc(widget[["window"]],
             widget[["style"]][["fgGc"]][[widget[["state"]]+1]],
             TRUE, 0, 0, widget[["allocation"]]$width,
             widget[["allocation"]]$height, 0, 64 * 360)
  return(TRUE)
}
[...]
  
drawing_area = gtkDrawingArea()
drawing_area$setSizeRequest(100, 100)
gSignalConnect(drawing_area, "expose_event", expose_event_callback)
