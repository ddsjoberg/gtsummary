#source("/home/larman/research/RGtk2/RGtk2/demo/iconview-dnd.R")

window <- NULL

fill_store <- function(store)
{
  text <- c("Red", "Green", "Blue", "Yellow")
  
  # First clear the store
  store$clear()
  
  sapply(text, function(color)
    {
      iter <- store$append()$iter
      store$set(iter, 0, color)
    })
}

create_store <- function()
{
  gtkListStore("character")
}

set_cell_color <- function(cell_layout, cell, tree_model, iter, data)
{
  text <- tree_model$get(iter, 0)[[1]]
  parse <- gdkColorParse(text)
  pixel <- 0
  if (parse[[1]])
    pixel <- sum(unlist(parse$color)[2:4]%/%(2^8)*2^(c(24, 16, 8)))

  pixbuf <- gdkPixbufNew("rgb", FALSE, 8, 24, 24)
  pixbuf$fill(pixel)

  cell["pixbuf"] <- pixbuf
}

#debug(set_cell_color)
#debug("[<-.GObject")

edited <- function(cell, path_string, text, data)
{
  model <- data$getModel()
  path <- gtkTreePathNewFromString(path_string)

  iter <- model$getIter(path)$iter
  model$set(iter, 0, text)
}

window <- gtkWindow("toplevel", show = F)
window$setTitle("Editing and Drag-and-Drop")

store <- create_store()
fill_store(store)

icon_view <- gtkIconView(store)
icon_view$setSelectionMode("single")
icon_view$setOrientation("horizontal")
icon_view$setColumns(2)
icon_view$setReorderable(TRUE)

renderer <- gtkCellRendererPixbuf()
icon_view$packStart(renderer, TRUE)
icon_view$setCellDataFunc(renderer, set_cell_color)

renderer <- gtkCellRendererText()
icon_view$packStart(renderer, TRUE)
renderer["editable"] <- TRUE
gSignalConnect(renderer, "edited", edited, icon_view)
icon_view$setAttributes(renderer, text = 0)

window$add(icon_view)
window$showAll()
