window <- NULL

FOLDER.NAME <- imagefile("gnome-fs-directory.png")
FILE.NAME <- imagefile("gnome-fs-regular.png")

file.pixbuf <- NULL
folder.pixbuf <- NULL
parent <- NULL
up.button <- NULL

iv <- c(
  path = 0,
  display.name = 1,
  pixbuf = 2,
  is.directory = 3,
  num = 4
)


# Loads the images for the demo and returns whether the operation succeeded
load.pixbufs <- function()
{
  if (!is.null(file.pixbuf))
    return() # already loaded earlier

  filename <- FILE.NAME
  if (!file.exists(filename))
    stop("File pixbuf does not exist")

  file.pixbuf.ret <- gdkPixbufNewFromFile(filename, .errwarn = F)
  file.pixbuf <<- file.pixbuf.ret[[1]]
  if (is.null(file.pixbuf))
    stop(file.pixbuf.ret$error["message"])

  filename <- FOLDER.NAME
  if (!file.exists(filename))
    stop("Folder pixbuf does not exist")

  folder.pixbuf <<- gdkPixbufNewFromFile(filename)[[1]]
}

fill.store <- function(store)
{
  # First clear the store
  store$clear()

  # Now go through the directory and extract all the file
  # information - hidden files are ignored

  sapply(dir(parent, full.names=T), function(path) {
      name <- basename(path)
      is.dir <- file.info(path)$isdir

      pixbuf <- file.pixbuf
      if (is.dir)
          pixbuf <- folder.pixbuf

      store$set(store$append()$iter,
                  iv["path"], path,
                  iv["display.name"], name,
                  iv["is.directory"], is.dir,
                  iv["pixbuf"], pixbuf)
    })
}

sort.func <- function(model, a, b, user.data)
{
  # We need this function because we want to sort
  # folders before files.
  #

  item.a <- model$get(a, iv["is.directory"], iv["display.name"])
  item.b <- model$get(b, iv["is.directory"], iv["display.name"])
  
  if (!item.a[[1]] && item.b[[1]])
    ret <- 1
  else if (item.a[[1]] && !item.b[[1]])
    ret <- -1
  else if ((!is.null(item.a[[2]]) && !is.null(item.b[[2]])) && item.a[[2]] < item.b[[2]])
      ret <- -1
  else ret <- 1

  return(as.integer(ret))
}

create.store <- function()
{
  store <- gtkListStoreNew("gchararray", "gchararray", "GdkPixbuf", "gboolean")

  # Set sort column and function
  store$setDefaultSortFunc(sort.func)
  store$setSortColumnId(-1, GtkSortType["ascending"])

  return(store)
}

item.activated <- function(icon.view, tree.path, user.data)
{
  checkPtrType(user.data, "GtkListStore")

  iter <- store$getIter(tree.path)$iter
  item <- store$get(iter, iv["path"], iv["is.directory"])

  if (!item[[2]]) # can't do anything with plain files
    return()

  # Replace parent with path and re-fill the model
  parent <<- item[[1]]

  fill.store(store)

  # Sensitize the up button
  up.button$setSensitive(TRUE)
}

up.clicked <- function(item, user.data)
{
  checkPtrType(user.data, "GtkListStore")

  parent <<- dirname(parent)

  fill.store(store)

  # Maybe de-sensitize the up button
  up.button$setSensitive(parent != "/")
}

home.clicked <- function(item, user.data)
{
  checkPtrType(user.data, "GtkListStore")

  parent <<- path.expand("~")

  fill.store(store)

  # Sensitize the up button
  up.button$setSensitive(TRUE)
}

window <- gtkWindowNew("toplevel", show=F)
window$setDefaultSize(650, 400)

window$setTitle("GtkIconView demo")

error <- try(load.pixbufs())
if (inherits(error, "try-error"))
{
    dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close", "Failed to load an image:", error)
    gSignalConnect(dialog, "response", gtkWidgetDestroy)
    dialog$show()
} else {
      vbox <- gtkVBoxNew(FALSE, 0)
      window$add(vbox)

      tool.bar <- gtkToolbarNew()
      vbox$packStart(tool.bar, FALSE, FALSE, 0)

      up.button <- gtkToolButtonNewFromStock("gtk-go-up")
      up.button$setIsImportant(TRUE)
      up.button$setSensitive(FALSE)
      tool.bar$insert(up.button, -1) # append

      home.button <- gtkToolButtonNewFromStock("gtk-home")
      home.button$setIsImportant(TRUE)
      tool.bar$insert(home.button, -1)

      sw <- gtkScrolledWindowNew()
      sw$setShadowType("etched-in")
      sw$setPolicy("automatic", "automatic")

      vbox$packStart(sw, TRUE, TRUE, 0)

      # Create the store and fill it with the contents of '/'
      parent <- "/"
      store <- create.store()
      fill.store(store)

      icon.view <- gtkIconViewNewWithModel(store)
      icon.view$setSelectionMode("multiple")

      # Connect to the "clicked" signal of the "Up" tool button
      gSignalConnect(up.button, "clicked", up.clicked, store)

      # Connect to the "clicked" signal of the "Home" tool button
      gSignalConnect(home.button, "clicked", home.clicked, store)

      # We now set which model columns that correspont to the text
      # and pixbuf of each item
      #
      icon.view$setTextColumn(iv["display.name"])
      icon.view$setPixbufColumn(iv["pixbuf"])

      # Connect to the "item.activated" signal
      gSignalConnect(icon.view, "item_activated", item.activated, store)
      sw$add(icon.view)

      icon.view$grabFocus()
}

window$showAll()
