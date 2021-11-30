RADIUS <- 150
N.WORDS <- 10
FONT <- "Sans Bold 27"

draw.text <- function(widget, event, data)
{
  width <- widget[["allocation"]][["width"]]
  height <- widget[["allocation"]][["height"]]
  
  device.radius <- min(width, height) / 2.
  
  cr <- gdkCairoCreate(widget[["window"]])
  
  ## Center coordinates on the middle of the region we are drawing
  cr$translate(device.radius + (width - 2 * device.radius) / 2,
               device.radius + (height - 2 * device.radius) / 2)
  cr$scale(device.radius / RADIUS, device.radius / RADIUS)
  
  ## Create a PangoLayout, set the font and text
  layout <- pangoCairoCreateLayout(cr)
  
  layout$setText("Text")
  desc <- pangoFontDescriptionFromString(FONT)
  layout$setFontDescription(desc)
  
  ## Draw the layout N.WORDS times in a circle
  for (i in 1:N.WORDS) {
    angle <- (360 * i) / N.WORDS
    
    cr$save()
    
    ## Gradient from red at angle 60 to blue at angle 300
    red <- (1 + cos((angle - 60) * pi / 180)) / 2
    cr$setSourceRgb(red, 0, 1.0 - red)
    
    cr$rotate(angle * pi / 180)
    
    ## Inform Pango to re-layout the text with the new transformation
    pangoCairoUpdateLayout(cr, layout)
    
    size <- layout$getSize()
    cr$moveTo(- (size$width / .PangoScale) / 2, - RADIUS)
    pangoCairoShowLayout(cr, layout)
    
    cr$restore()
  }
  return(FALSE)
}

white <- c( 0, "0xffff", "0xffff", "0xffff" )

window <- gtkWindow("toplevel", show = F)
window$setTitle("Rotated Text")
drawing.area <- gtkDrawingArea()
window$add(drawing.area)

# This overrides the background color from the theme
drawing.area$modifyBg("normal", white)

gSignalConnect(drawing.area, "expose-event", draw.text)

window$showAll()
