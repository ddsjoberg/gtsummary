#source("/home/larman/research/RGtk/inst/demo/rotatedText.R")

window <- NULL

RADIUS <- 150
N.WORDS <- 10
FONT <- "Sans Bold 27"

rotated.text.expose.event <- function(widget, event, data)
{

    # matrix describing font transformation, initialize to identity
  matrix <- pangoMatrixInit()

  width <- widget[["allocation"]][["width"]]
  height <- widget[["allocation"]][["height"]]

  # Get the default renderer for the screen, and set it up for drawing
  renderer <- gdkPangoRendererGetDefault(widget$getScreen())
  renderer$setDrawable(widget[["window"]])
  renderer$setGc(widget[["style"]][["blackGc"]])

  # Set up a transformation matrix so that the user space coordinates for
  # the centered square where we draw are [-RADIUS, RADIUS], [-RADIUS, RADIUS]
  # We first center, then change the scale
  device.radius <- min(width, height) / 2.
  matrix$translate(device.radius + (width - 2 * device.radius) / 2,
              device.radius + (height - 2 * device.radius) / 2)
  matrix$scale(device.radius / RADIUS, device.radius / RADIUS)

  # Create a PangoLayout, set the font and text
  context <- widget$createPangoContext()
  layout <- pangoLayoutNew(context)
  layout$setText("Text")
  desc <- pangoFontDescriptionFromString(FONT)
  layout$setFontDescription(desc)

  # Draw the layout N.WORDS times in a circle
  for (i in 1:N.WORDS)
    {
      rotated.matrix <- matrix$copy()
      angle <- (360 * i) / N.WORDS

      color <- list()
      # Gradient from red at angle 60 to blue at angle 300
      color$red <- 65535 * (1 + cos((angle - 60) * pi / 180)) / 2
      color$green <- 0
      color$blue <- 65535 - color$red

      renderer$setOverrideColor("foreground", color)

      rotated.matrix$rotate(angle)

      context$setMatrix(rotated.matrix)

      # Inform Pango to re-layout the text with the new transformation matrix
      layout$contextChanged()

      size <- layout$getSize()
      renderer$drawLayout(layout, - size$width / 2, - RADIUS * 1024)
    }

  # Clean up default renderer, since it is shared
  renderer$setOverrideColor("foreground", NULL)
  renderer$setDrawable(NULL)
  renderer$setGc(NULL)

  return(FALSE)
}


white <- c( 0, "0xffff", "0xffff", "0xffff" )

window <- gtkWindowNew("toplevel")
window$setTitle("Rotated Text")
drawing.area <- gtkDrawingAreaNew()
window$add(drawing.area)

# This overrides the background color from the theme
drawing.area$modifyBg("normal", white)

gSignalConnect(drawing.area, "expose-event", rotated.text.expose.event)

window$setDefaultSize(2 * RADIUS, 2 * RADIUS)

window$showAll()
