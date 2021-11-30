## A simple demo of rendering a document and printing it with RGtk2

# In points
HEADER_HEIGHT <- (10*72/25.4)
HEADER_GAP <- (3*72/25.4)

begin_print <- function(operation, context, user_data)
{
  data <- operation$getData("print_data")
  
  height <- context$getHeight() - HEADER_HEIGHT - HEADER_GAP
  
  data$lines_per_page <- floor(height / data$font_size)
  
  data$lines <- readLines(data$filename)
  
  data$num_pages = round((length(data$lines) - 1) / data$lines_per_page)
  operation$setNPages(data$num_pages)
  
  operation$setData("print_data", data)
}

draw_page <- function(operation, context, page_nr, user_data)
{
  data <- operation$getData("print_data")

  cr <- context$getCairoContext()
  width <- context$getWidth()

  # shade / outline header using cairo
  cr$rectangle(0, 0, width, HEADER_HEIGHT)
  
  cr$setSourceRgb(0.8, 0.8, 0.8)
  cr$fillPreserve()
  
  cr$setSourceRgb(0, 0, 0)
  cr$setLineWidth(1)
  cr$stroke()

  # set up pango layout
  layout <- context$createPangoLayout()

  desc <- pangoFontDescriptionFromString("sans 14")
  layout$setFontDescription(desc)

  # write file name at top of page
  layout$setText(basename(data$filename))
  layout$setWidth(width)
  layout$setAlignment("center")
			      
  layout_height <- layout$getSize()$height
  text_height <- layout_height / PANGO_SCALE

  cr$moveTo(width / 2,  (HEADER_HEIGHT - text_height) / 2)
  pangoCairoShowLayout(cr, layout)

  # write page at top right
  page_str <- paste(page_nr + 1, "/", data$num_pages, sep="")
  layout$setText(page_str)
  layout$setAlignment("right")
			      
  cr$moveTo(width - 2, (HEADER_HEIGHT - text_height) / 2)
  pangoCairoShowLayout(cr, layout)
  
  # prepare to write text in different font / size
  layout <- context$createPangoLayout()
  
  desc <- pangoFontDescriptionFromString("monospace")
  desc$setSize(data$font_size * PANGO_SCALE)
  layout$setFontDescription(desc)
  
  # write text
  cr$moveTo(0, HEADER_HEIGHT + HEADER_GAP)
  first <- page_nr * data$lines_per_page
  sapply(data$lines[first:(first + data$lines_per_page - 1)], function(line) {
    if (is.na(line))
      return()
    layout$setText(line)
    pangoCairoShowLayout(cr, layout)
    cr$relMoveTo(0, data$font_size)
  })
}


operation <- gtkPrintOperation()

data <- list()
data$filename <- file.path(installed.packages()["RGtk2", "LibPath"], "RGtk2", 
  "demo", "printing.R")
data$font_size <- 12.0
operation$setData("print_data", data)

gSignalConnect(operation, "begin-print", begin_print)
gSignalConnect(operation, "draw-page", draw_page)

operation$setUseFullPage(FALSE)
operation$setUnit("points")
operation$setEmbedPageSetup(TRUE)

settings <- gtkPrintSettings()
uri <- file.path("file:/",  getwd(), "gtk-demo-printing-example.pdf")
settings$set("output-uri", uri)
operation$setPrintSettings(settings)

result <- operation$run("print-dialog", NULL)

if (!is.null(result$error))
{
  dialog <- gtkMessageDialog(NULL, 0, "error", "close", result$error$message)
  dialog$run()
  dialog$destroy()
}
