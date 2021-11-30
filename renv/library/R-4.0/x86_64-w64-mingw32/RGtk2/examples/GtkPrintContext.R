draw_page <- function(operation, context, page_nr)
{
  cr <- context$getCairoContext()

  # Draw a red rectangle, as wide as the paper (inside the margins)
  cr$setSourceRgb(1.0, 0, 0)
  cr$rectangle(0, 0, context$getWidth(), 50)
  
  cr$fill()

  # Draw some lines
  cr$moveTo(20, 10)
  cr$lineTo(40, 20)
  cr$arc(60, 60, 20, 0, pi)
  cr$lineTo(80, 20)
  
  cr$setSourceRgb(0, 0, 0)
  cr$setLineWidth(5)
  cr$setLineCap("round")
  cr$setLineJoin("round")
  
  cr$stroke()

  # Draw some text
  
  layout <- context$createLayout()
  layout$setText("Hello World! Printing is easy")
  desc <- pangoFontDescriptionFromString("sans 28")
  layout$setFontDescription(desc)
  
  cr$moveTo(30, 20)
  cr$layoutPath(layout)

  # Font Outline
  cr$setSourceRgb(0.93, 1.0, 0.47)
  cr$setLineWidth(0.5)
  cr$strokePreserve()

  # Font Fill
  cr$setSourceRgb(0, 0.0, 1.0)
  cr$fill()
}
