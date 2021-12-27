window <- NULL
# Pixmap for scribble area, to store current scribbles
pixmap <- NULL

# Create a new pixmap of the appropriate size to store our scribbles
scribble.configure.event <- function(widget, event, data)
{
  pixmap <<- gdkPixmapNew(widget[["window"]], widget[["allocation"]][["width"]],
               widget[["allocation"]][["height"]], -1)

  # Initialize the pixmap to white
  cr <- gdkCairoCreate(pixmap)
  cr$setSourceRgb(1, 1, 1)
  cr$paint()

  # We've handled the configure event, no need for further processing.
  return(TRUE)
}

# Redraw the screen from the pixmap
scribble.expose.event <- function(widget, event, data)
{
  # We use the "foreground GC" for the widget since it already exists,
  # but honestly any GC would work. The only thing to worry about
  # is whether the GC has an inappropriate clip region set.
  #

  cr <- gdkCairoCreate(widget[["window"]])
  gdkCairoSetSourcePixmap(cr, pixmap, 0, 0)
  gdkCairoRectangle(cr, event[["area"]])
  cr$fill()
  
  return(FALSE)
}

# Draw a rectangle on the screen
draw.brush <- function(widget, x, y)

{
  update.rect <- c(x=x-3, y=y-3, width=6, height=6)

  cr <- gdkCairoCreate(pixmap)
  gdkCairoRectangle(cr, update.rect)
  cr$fill()
  
  # Now invalidate the affected region of the drawing area. (so it will be updated)
  gdkWindowInvalidateRect(widget[["window"]], update.rect, FALSE)
}

scribble.button.press.event <- function(widget, event, data)
{
  if (is.null(pixmap))
    return(FALSE) # paranoia check, in case we haven't gotten a configure event

  if (event[["button"]] == 1) # left mouse button click
    draw.brush(widget, event[["x"]], event[["y"]])

  # We've handled the event, stop processing
  return(TRUE)
}

scribble.motion.notify.event <- function(widget, event, data)
{
  if (is.null(pixmap))
    return(FALSE) # paranoia check, in case we haven't gotten a configure event

  # This call is very important it requests the next motion event.
  # If you don't call gdkWindowGetPointer() you'll only get
  # a single motion event. The reason is that we specified
  # GDK_POINTER_MOTION_HINT_MASK to gtkWidgetSetEvents().
  # If we hadn't specified that, we could just use event[["x"]], event[["y"]]
  # as the pointer location. But we'd also get deluged in events.
  # By requesting the next event as we handle the current one,
  # we avoid getting a huge number of events faster than we
  # can cope.
  #

  pointer <- gdkWindowGetPointer(event[["window"]])

  # if button1 held down, draw
  if (as.flag(pointer$mask) & GdkModifierType["button1-mask"])
    draw.brush(widget, pointer$x, pointer$y)

  # We've handled it, stop processing
  return(TRUE)
}

checkerboard.expose <- function(da, event, data)
{

 CHECK.SIZE <- 10
 SPACING <- 2

  # At the start of an expose handler, a clip region of event[["area"]]
  # is set on the window, and event[["area"]] has been cleared to the
  # widget's background color. The docs for
  # gdkWindowBeginPaintRegion() give more details on how this
  # works.
  #
 
  cr <- gdkCairoCreate(da[["window"]])
  gdkCairoRectangle(cr, event[["area"]])
  cr$clip()
 
  xcount <- 0
  i <- SPACING
  while (i < da[["allocation"]][["width"]])
    {
      j <- SPACING
      ycount <- xcount %% 2 # start with even/odd depending on row
      while (j < da[["allocation"]][["height"]])
    {
      if (ycount %% 2)
        cr$setSourceRgb(0.45777, 0, 0.45777)
      else
        cr$setSourceRgb(1, 1, 1)

      # If we're outside event->area, this will do nothing.
      # It might be mildly more efficient if we handled
      # the clipping ourselves, but again we're feeling lazy.
      #

      cr$rectangle(i, j, CHECK.SIZE, CHECK.SIZE)
      cr$fill()
      
      j <- j + CHECK.SIZE + SPACING
      ycount <- ycount + 1
    }

      i <- i + CHECK.SIZE + SPACING
      xcount <- xcount + 1
    }

  # return TRUE because we've handled this event, so no
  # further processing is performed
  #

  return(TRUE)
}

window <- gtkWindowNew("toplevel", show=F)

window$setTitle("Drawing Area")
window$setBorderWidth(8)

vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(8)
window$add(vbox)

#
# Create the checkerboard area
#

label <- gtkLabelNew()
label$setMarkup("<u>Checkerboard pattern</u>")
vbox$packStart(label, FALSE, FALSE, 0)

frame <- gtkFrameNew()
frame$setShadowType("in")
vbox$packStart(frame, TRUE, TRUE, 0)

da <- gtkDrawingAreaNew(show=F)
# set a minimum size
da$setSizeRequest(100, 100)

frame$add(da)

gSignalConnect(da, "expose_event", checkerboard.expose)

#
# Create the scribble area
#

label <- gtkLabelNew()
label$setMarkup("<u>Scribble area</u>")
vbox$packStart(label, FALSE, FALSE, 0)

frame <- gtkFrameNew()
frame$setShadowType("in")
vbox$packStart(frame, TRUE, TRUE, 0)

da <- gtkDrawingAreaNew()
# set a minimum size
da$setSizeRequest(100, 100)

frame$add(da)

# Signals used to handle backing pixmap

gSignalConnect(da, "expose_event", scribble.expose.event)
gSignalConnect(da, "configure_event", scribble.configure.event)

# Event signals

gSignalConnect(da, "motion_notify_event", scribble.motion.notify.event)
gSignalConnect(da, "button_press_event", scribble.button.press.event)

# Ask to receive events the drawing area doesn't normally
# subscribe to
#
# we have to do this numerically, because the function takes gint, not GdkEventMask
da$setEvents(da$getEvents() + GdkEventMask["button-press-mask"] +
    GdkEventMask["pointer-motion-mask"] + GdkEventMask["pointer-motion-hint-mask"])

window$showAll()
