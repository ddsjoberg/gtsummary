# The popup handler
my_popup_handler <- function(widget, event)
{
  stopifnot(widget != NULL)
  checkPtrType(widget, "GtkMenu")
  stopifnot(event != NULL)

  ## The "widget" is the menu that was supplied when 
  ## gSignalConnect() was called.
  menu <- widget

  if (event[["type"]] == "button-press") {
    if (event[["button"]] == 3) {
      menu$popup(button=event[["button"]], activate.time=event[["time"]])
      return(TRUE)
    }
  }

  return(FALSE)
}

