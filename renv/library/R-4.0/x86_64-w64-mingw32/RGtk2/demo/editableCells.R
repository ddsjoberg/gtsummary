# source("/home/larman/research/RGtk/inst/demo/editableCells.R")

COLUMN <- c(number = 0, product = 1, editable = 2)

articles <- NULL

add.items <- function()
{
  stopifnot(!is.null(articles))

  articles <<- c(articles, list(list(number = 3, product = "bottles of coke", editable = TRUE)))
  articles <<- c(articles, list(list(number = 5, product = "packages of noodles", editable = TRUE)))
  articles <<- c(articles, list(list(number = 2, product = "packages of chocolate chip cookies", editable = TRUE)))
  articles <<- c(articles, list(list(number = 1, product = "can vanilla ice cream", editable = TRUE)))
  articles <<- c(articles, list(list(number = 6, product = "eggs", editable = TRUE)))
}

create.model <- function()
{
  # create the array of data
  articles <<- list()

  add.items()

  # create list store
  model <- gtkListStoreNew("gint", "gchararray", "gboolean")
  
  # add items 
  for (i in 1:length(articles))
    {
      iter <- model$append()$iter

      model$set(iter,
			  COLUMN["number"], articles[[i]]$number,
			  COLUMN["product"], articles[[i]]$product,
			  COLUMN["editable"], articles[[i]]$editable)
    }

  return(model)
}

add.item <- function(button, data)
{
  stopifnot(!is.null(articles))

  foo <- list(number = 0, product = "Description here", editable = TRUE)
  articles <<- c(articles, foo)

  iter <- model$append()$iter
  model$set(iter, 
  			  COLUMN["number"], foo$number,
		      COLUMN["product"], foo$product,
		      COLUMN["editable"], foo$editable)
}

remove.item <- function(widget, data)
{
  checkPtrType(data, "GtkTreeView")
  treeview <- data
  model <- treeview$getModel()
  selection <- treeview$getSelection()

  selected <- selection$getSelected()
  if (selected[[1]])
    {
      iter <- selected$iter
	  
      path <- model$getPath(iter)
      i <- path$getIndices()[[1]]
      model$remove(iter)

      articles <<- articles[-i]
    }
}

cell.edited <- function(cell, path.string, new.text, data)
{
  checkPtrType(data, "GtkListStore")
  model <- data
  
  path <- gtkTreePathNewFromString(path.string)
  
  column <- cell$getData("column")

  iter <- model$getIter(path)$iter

  switch(column+1, {
	i <- path$getIndices()[[1]]+1
	articles[[i]]$number <<- as.integer(new.text)

	model$set(iter, column, articles[[i]]$number)
  }, {
	old.text <- model$get(iter, column)
	i <- path$getIndices()[[1]]+1
	articles[[i]]$product <<- new.text

	model$set(iter, column, articles[[i]]$product)
  })
}

add.columns <- function(treeview)
{
  model <- treeview$getModel()

  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", cell.edited, model)
  renderer$setData("column", COLUMN["number"])
  treeview$insertColumnWithAttributes(-1, "Number", renderer, text = COLUMN[["number"]],
					       editable = COLUMN[["editable"]])

  # product column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", cell.edited, model)
  renderer$setData("column", COLUMN["product"])
  treeview$insertColumnWithAttributes(-1, "Product", renderer,
					       text = COLUMN[["product"]], editable = COLUMN[["editable"]])
}

# create window, etc
window <- gtkWindowNew("toplevel", show = F)
window$setTitle("Shopping list")
window$setBorderWidth(5)

vbox <- gtkVBoxNew(FALSE, 5)
window$add(vbox)

vbox$packStart(gtkLabelNew("Shopping list (you can edit the cells!)"),
	  FALSE, FALSE, 0)

sw <- gtkScrolledWindowNew(NULL, NULL)
sw$setShadowType("etched-in")
sw$setPolicy("automatic", "automatic")
vbox$packStart(sw, TRUE, TRUE, 0)

# create model
model <- create.model()

# create tree view
treeview <- gtkTreeViewNewWithModel(model)

treeview$setRulesHint(TRUE)
treeview$getSelection()$setMode("single")

add.columns(treeview)

sw$add(treeview)

# some buttons
hbox <- gtkHBoxNew(TRUE, 4)
vbox$packStart(hbox, FALSE, FALSE, 0)

button <- gtkButtonNewWithLabel("Add item")
gSignalConnect(button, "clicked", add.item, model)
hbox$packStart(button, TRUE, TRUE, 0)

button <- gtkButtonNewWithLabel("Remove item")
gSignalConnect(button, "clicked", remove.item, treeview)
hbox$packStart(button, TRUE, TRUE, 0)

window$setDefaultSize(320, 200)

window$showAll()
