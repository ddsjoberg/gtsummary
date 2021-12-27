draw_page <- (operation, context, page_nr, user_data)
{
  cr <- context$getCairoContext()
  width <- context$getWidth()
  
  cr$rectangle(0, 0, width, HEADER_HEIGHT)
  
  cr$setSourceRgb(0.8, 0.8, 0.8)
  cr$fill()
  
  layout <- context$createPangoLayout()
  
  desc <- pangoFontDescriptionFromString("sans 14")
  layout$setFontDescription(desc)
  
  layout$setText("some text")
  layout$setWidth(width)
  layout$setAlignment(layout, "center")
     		      
  layout_height <- layout$getSize()$height
  text_height <- layout_height / PANGO_SCALE
  
  cr$moveTo(width / 2,  (HEADER_HEIGHT - text_height) / 2)
  pangoCairoShowLayout(cr, layout)
}
