# Handling button-press events on a GtkImage
button_press_callback <- function(event_box, event, data) {
  print(paste("Event box clicked at coordinates ", event[["x"]], ",",
              event[["y"]], sep=""))

  ## Returning TRUE means we handled the event, so the signal 
  ## emission should be stopped (don't call any further 
  ## callbacks that may be connected). Return FALSE
  ## to continue invoking callbacks.
  
  return(TRUE)
}

create_image <- function() {
  image <- gtkImage(file="myfile.png")
  
  event_box <- gtkEventBox()
  event_box$add(image)
  
  
  gSignalConnect(event_box, "button_press_event", button_press_callback, image)
  
  return(image)
}
