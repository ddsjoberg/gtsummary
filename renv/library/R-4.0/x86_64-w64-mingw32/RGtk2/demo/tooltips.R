### GTK+ 2.12 tooltip enhancements

query_tooltip_cb <- function(widget, x, y, keyboard_tip, tooltip) {
  tooltip$setMarkup(widget$getLabel())
  tooltip$setIconFromStock(GTK_STOCK_DELETE, "menu")
  return(TRUE)
}

query_tooltip_custom_cb <- function(widget, x, y, keyboard_tip, tooltip) {
  color <- as.GdkColor(c(0, 65535, 0))
  window <- widget$getTooltipWindow()
  window$modifyBg("normal", color)
  return(TRUE)
}

query_tooltip_text_view_cb <- function(widget, x, y, keyboard_tip, tooltip,
                                       data)
{
  if (keyboard_tip) {
    offset <- widget["buffer"]["cursor_position"]
    iter <- widget["buffer"]$getIterAtOffset(offset)$iter
  }
  else {
    coords <- widget$windowToBufferCoords("text", x, y)
    iter <- widget$getIterAtPosition(coords[[2]], coords[[3]])$iter
    if (iter$hasTag(data))
      tooltip$setText("Tooltip on text tag")
    else return(FALSE)
  }
  return(TRUE)
}
    
query_tooltip_tree_view_cb <- function(widget, x, y, keyboard_tip, tooltip)
{
  model <- widget$getModel()

  ctx <- widget$getTooltipContext(x, y, keyboard_tip)
  
  if (!ctx$retval)
    return(FALSE)

  tmp <- model$get(ctx$iter, 0)[[1]]
  pathstring <- ctx$path$toString()

  markup <- sprintf("<b>Path %s:</b> %s", pathstring, tmp)
  tooltip$setMarkup(markup)

  widget$setTooltipRow(tooltip, ctx$path)
  
  return(TRUE)
}
    
query_tooltip_drawing_area_cb <- function(widget, x, y, keyboard_tip, tooltip)
{
  if (keyboard_tip)
    return(FALSE)

  markup <- rects$x < x & y < rects$x + 50 & rects$y < y & y < rects$y + 50
  if (any(markup)) {
    tooltip$setMarkup(rects$tooltip[which(markup)[1]])
    return(TRUE)
  }

  return(FALSE)
}
    
selection_changed_cb <- function(self, selection, tree_view) {
  tree_view$triggerTooltipQuery()
}

create_model <- function() {
  store <- gtkTreeStore("character")
        
  ## A tree store with some random words ...
  store$set(store$append()$iter, 0, "File Manager")
  store$set(store$append()$iter, 0, "Gossip")
  store$set(store$append()$iter, 0, "System Settings")
  store$set(store$append()$iter, 0, "The GIMP")
  store$set(store$append()$iter, 0, "Terminal")
  store$set(store$append()$iter, 0, "Word Processor")
        
  return(store)
}
    
drawing_area_expose <- function(drawing_area, event)
{
  cr <- gdkCairoCreate(drawing_area[["window"]])
        
  cr$rectangle(0, 0, drawing_area[["allocation"]]$width,
                     drawing_area[["allocation"]]$height)
  cr$setSourceRgb(1.0, 1.0, 1.0)
  cr$fill()

  apply(rects, 1, function(rect) {
    cr$rectangle(rect["x"], rect["y"], 50, 50)
    cr$setSourceRgb(rect["r"], rect["g"], rect["b"])
    cr$stroke()
    cr$rectangle(rect["x"], rect["y"], 50, 50)
    cr$setSourceRgba(rect["r"], rect["g"], rect["b"], 0.5)
    cr$fill()
  })
  
  return(FALSE)
}

rects <- data.frame(x = c(10, 200, 100), y = c(10, 170, 50),
                    r = c(0, 1, .8), g = c(0, 0, .8), b = c(.9, 0, 0),
                    tooltip = c("Blue box!", "Red thing", "Yellow thing"))

win <- gtkWindow(show = FALSE)
win$setBorderWidth(10)

box <- gtkVBox(FALSE, 3)
win$add(box)

## A check button using the tooltip-markup property
button <- gtkCheckButton("This one uses the tooltip-markup property")
button$setTooltipText("Hello, I am a static tooltip.")

box$packStart(button, FALSE, FALSE, 0)
        
## A check button using the query-tooltip signal
button <- gtkCheckButton("I use the query-tooltip signal")
button["has_tooltip"] <- TRUE
gSignalConnect(button, "query-tooltip", query_tooltip_cb)
box$packStart(button, FALSE, FALSE, 0)
        
## A label
label <- gtkLabel("I am just a label")
label$setSelectable(FALSE)
label$setTooltipText("Label & and tooltip")
box$packStart(label, FALSE, FALSE, 0)
        
## A selectable label
label <- gtkLabel("I am a selectable label")
label$setSelectable(TRUE)
label$setTooltipMarkup("<b>Another</b> Label tooltip")
box$packStart(label, FALSE, FALSE, 0)
        
## Another one, with a custom tooltip window
button <- gtkCheckButton("This one has a custom tooltip window!")
box$packStart(button, FALSE, FALSE, 0)
        
tooltip_window <- gtkWindow("popup", show = FALSE)
tooltip_button <- gtkLabel("blaat!")
tooltip_window$add(tooltip_button)
        
button$setTooltipWindow(tooltip_window)
gSignalConnect(button, "query-tooltip", query_tooltip_custom_cb)
button["has_tooltip"] <- TRUE
        
## An insensitive button
button <- gtkButton("This one is insensitive")
button$setSensitive(FALSE)
button["tooltip_text"] <- "Insensitive!"
box$packStart(button, FALSE, FALSE, 0)
        
## Testcases from Kris without a tree view don't exist
tree_view <- gtkTreeView(create_model())
tree_view$setSizeRequest(200, 240)
        
tree_view$insertColumnWithAttributes(0, "Test", gtkCellRendererText(),
                                     text = 0)
        
tree_view["has_tooltip"] <- TRUE
gSignalConnect(tree_view, "query-tooltip", query_tooltip_tree_view_cb)
gSignalConnect(tree_view$getSelection(), "changed", selection_changed_cb,
               tree_view)
        
## We cannot get the button on the treeview column directly
## so we have to use a ugly hack to get it.
column <- tree_view$getColumn(0)
column$setClickable(TRUE)
label <- gtkLabel("Test")
column$setWidget(label)
button <- label$getParent()
button["tooltip_text"] <- "Header"

box$packStart(tree_view, FALSE, FALSE, 2)
        
## And a text view for Matthias
buffer <- gtkTextBuffer()

iter <- buffer$getEndIter()$iter
buffer$insert(iter, "Hello, the text ", -1)

tag <- buffer$createTag("bold")
tag["weight"] <- PangoWeight["bold"]

iter <- buffer$getEndIter()$iter
buffer$insertWithTags(iter, "in bold", tag)

iter <- buffer$getEndIter()$iter
buffer$insert(iter, " has a tooltip!", -1)

text_view <- gtkTextView(buffer)
text_view$setSizeRequest(200, 50)

text_view["has_tooltip"] <- TRUE
gSignalConnect(text_view, "query-tooltip", query_tooltip_text_view_cb, tag)

box$packStart(text_view, FALSE, FALSE, 2)

## Drawing area
drawing_area <- gtkDrawingArea()
drawing_area$setSizeRequest(320, 240)
drawing_area["has_tooltip"] <- TRUE
gSignalConnect(drawing_area, "expose_event", drawing_area_expose)
gSignalConnect(drawing_area, "query-tooltip",
               query_tooltip_drawing_area_cb)
box$packStart(drawing_area, FALSE, FALSE, 2)
        
## Done!
win$showAll()
