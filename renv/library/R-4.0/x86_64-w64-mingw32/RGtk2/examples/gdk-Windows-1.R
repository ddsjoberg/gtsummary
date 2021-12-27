# The expose event handler for the event box.
# 
# This function simply draws a transparency onto a widget on the area
# for which it receives expose events.  This is intended to give the
# event box a "transparent" background.
# 
# In order for this to work properly, the widget must have an RGBA
# colormap.  The widget should also be set as app-paintable since it
# doesn't make sense for GTK+ to draw a background if we are drawing it
# (and because GTK+ might actually replace our transparency with its
# default background color).
# 
transparent_expose <- function(widget, event)
{
  cr <- gdkCairoCreate(widget$window)
  cr$setOperator("clear")
  gdkCairoRegion(cr, event$region)
  cr$fill()

  return(FALSE)
}

# The expose event handler for the window.
# 
# This function performs the actual compositing of the event box onto
# the already-existing background of the window at 50% normal opacity.
# 
# In this case we do not want app-paintable to be set on the widget
# since we want it to draw its own (red) background. Because of this,
# however, we must ensure that we set after = TRUE when connecting to the signal
# so that this handler is called after the red has been drawn. If it was
# called before then GTK would just blindly paint over our work.
# 
# Note: if the child window has children, then you need a cairo 1.16
# feature to make this work correctly.
# 

window_expose_event <- function(widget, event)
{
  # get our child (in this case, the event box) 
  child <- widget$getChild()

  # create a cairo context to draw to the window 
  cr <- gdkCairoCreate(widget$window)

  # the source data is the (composited) event box
  gdkCairoSetSourcePixmap(cr, child$window, child$allocation$x, 
                          child$allocation$y)

  # draw no more than our expose event intersects our child
  region <- gdkRegionRectangle(child$allocation)
  region$intersect(event$region)
  gdkCairoRegion(cr, region)
  cr$clip()

  # composite, with a 50% opacity
  cr$setOperator("over")
  cr$paintWithAlpha(0.5)

  return(FALSE)
}

# Make the widgets
button <- gtkButton("A Button")
event <- gtkEventBox()
window <- gtkWindow()

# Put a red background on the window
red <- gdkColorParse("red")$color
window$modifyBg("normal", red)

# Set the colormap for the event box.
# Must be done before the event box is realized.
#
screen <- event$getScreen()
rgba <- screen$getRgbaColormap()
event$setColormap(rgba)

# Set our event box to have a fully-transparent background
# drawn on it. Currently there is no way to simply tell GTK+
# that "transparency" is the background color for a widget.
#
event$setAppPaintable(TRUE)
gSignalConnect(event, "expose-event", transparent_expose)

# Put them inside one another
window$setBorderWidth(10)
window$add(event)
event$add(button)

# Set the event box GdkWindow to be composited.
# Obviously must be performed after event box is realized.
#
event$window$setComposited(TRUE)

# Set up the compositing handler.
# Note that we do _after_ so that the normal (red) background is drawn
# by gtk before our compositing occurs.
#
gSignalConnect(window, "expose-event", window_expose_event, after = TRUE)
