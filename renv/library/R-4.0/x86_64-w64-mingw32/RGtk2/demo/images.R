# source("~/research/RGtk2/RGtk2/demo/images.R")

window <- NULL
pixbuf.loader <- NULL
load.timeout <- NULL
image.stream <- NULL

progressive.prepared.callback <- function(loader, data)
{
  checkPtrType(data, "GtkWidget")

  pixbuf <- loader$getPixbuf()

  # Avoid displaying random memory contents, since the pixbuf
  # isn't filled in yet.
  #
  pixbuf$fill("0xaaaaaaff")

  data$setFromPixbuf(pixbuf)
}

progressive.updated.callback <- function(loader, x, y, width, height, data)
{
  checkPtrType(data, "GtkWidget")

  # We know the pixbuf inside the GtkImage has changed, but the image
  # itself doesn't know this so queue a redraw.  If we wanted to be
  # really efficient, we could use a drawing area or something
  # instead of a GtkImage, so we could control the exact position of
  # the pixbuf on the display, then we could queue a draw for only
  # the updated area of the image.
  #

  data$queueDraw()
}

progressive.timeout <- function(data)
{
  checkPtrType(data, "GtkWidget")

  # This shows off fully-paranoid error handling, so looks scary.
  # You could factor out the error handling code into a nice separate
  # function to make things nicer.
  #

  if (!is.null(image.stream))
    {
        bytes.read <- try(readBin(image.stream, "integer", 256, 1, FALSE))

      if (inherits(bytes.read, "try-error"))
    {
      dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close",
                       "Failure reading image file 'alphatest.png':", bytes.read)

      gSignalConnect(dialog, "response", gtkWidgetDestroy)

      close(image.stream)
      image.stream <<- NULL

      load.timeout <<- NULL

      return(FALSE) # uninstall the timeout
    }

      if (length(bytes.read) == 0)
    {
      close(image.stream)
      image.stream <<- NULL

      # Errors can happen on close, e.g. if the image
      # file was truncated we'll know on close that
      # it was incomplete.
      #
      close.result <- pixbuf.loader$close()
      if (!close.result[[1]]) {
          dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close",
                       "Failed to load image:", close.result$error$message)

          gSignalConnect(dialog, "response", gtkWidgetDestroy)

          pixbuf.loader <<- NULL

          load.timeout <<- NULL

          return(FALSE)
        }

      pixbuf.loader <<- NULL
    } else {
        write.result <- pixbuf.loader$write(bytes.read, length(bytes.read))
        if (!write.result[[1]])
        {
            dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close",
                       "Failed to load image:", write.result$error$message)

            gSignalConnect(dialog, "response", gtkWidgetDestroy)

            close(image.stream)
            image.stream <<- NULL

            load.timeout <<- NULL

            return(FALSE) # uninstall the timeout
        }
    }
  } else
    {
      filename <- imagefile("alphatest.png")
      if (!file.exists(filename))
    {
        error.message <- "File 'alphatest.png' does not exist"
    } else {
      image.stream <<- try(file(filename, "rb"))

      if (inherits(image.stream, "try-error")) {
        error.message <- paste("Unable to open image file 'alphatest.png':", image.stream)
        image.stream <<- NULL
      }

    }

      if (is.null(image.stream))
    {

        dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close",
                       "Failed to load image:", error.message)

        gSignalConnect(dialog, "response", gtkWidgetDestroy)

        load.timeout <<- NULL

      return(FALSE)
    }

      if (!is.null(pixbuf.loader))
    {
      pixbuf.loader$close(NULL)
      pixbuf.loader <<- NULL
    }

      pixbuf.loader <<- gdkPixbufLoaderNew()

      gSignalConnect(pixbuf.loader, "area_prepared", progressive.prepared.callback, data)

      gSignalConnect(pixbuf.loader, "area_updated", progressive.updated.callback, data)
    }

  # leave timeout installed
  return(TRUE)
}

start.progressive.loading <- function(image)
{
  # This is obviously totally contrived (we slow down loading
   # on purpose to show how incremental loading works).
   # The real purpose of incremental loading is the case where
   # you are reading data from a slow source such as the network.
   # The timeout simply simulates a slow data source by inserting
   # pauses in the reading process.
   #
  load.timeout <<- gTimeoutAdd(150, progressive.timeout, image)
}

cleanup.callback <- function(object, data)
{
  if (!is.null(load.timeout))
    {
      gSourceRemove(load.timeout)
      load.timeout <<- NULL
    }

  if (!is.null(pixbuf.loader))
    {
      pixbuf.loader$close()
      pixbuf.loader <<- NULL
    }

  if (!is.null(image.stream))
    close(image.stream)
  image.stream <<- NULL
}

toggle.sensitivity.callback <- function(togglebutton, user.data)
{
  container <- user.data

  list <- container$getChildren()

  sapply(list, function(child) {
      # don't disable our toggle
      if (!inherits(child, "GtkToggleButton"))
        child$setSensitive(!togglebutton$getActive())
    })
}

window <- gtkWindowNew("toplevel", show=F)
window$setTitle("Images")

gSignalConnect(window, "destroy", cleanup.callback)

window$setBorderWidth(8)

vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(8)
window$add(vbox)

label <- gtkLabelNew()
label$setMarkup("<u>Image loaded from a file</u>")
vbox$packStart(label, FALSE, FALSE, 0)

frame <- gtkFrameNew()
frame$setShadowType("in")
# The alignment keeps the frame from growing when users resize
# the window
#
align <- gtkAlignmentNew(0.5, 0.5, 0, 0)
align$add(frame)
vbox$packStart(align, FALSE, FALSE, 0)

pixbuf.result <- NULL
pixbuf <- NULL
filename <- imagefile("rgtk-logo.gif")
if (file.exists(filename))
{
    pixbuf.result <- gdkPixbufNewFromFile(filename)
    pixbuf <- pixbuf.result[[1]]
}
if (is.null(pixbuf.result) || !is.null(pixbuf.result$error))
{
    # This code shows off error handling. You can just use
    # gtkImageNewFromFile() instead if you don't want to report
    # errors to the user. If the file doesn't load when using
    # gtkImageNewFromFile(), a "missing image" icon will
    # be displayed instead.
    #
    dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close",
                   "Failed to load 'rgtk-logo.gif':", pixbuf.result$error$message)

    gSignalConnect(dialog, "response", gtkWidgetDestroy)
}

image <- gtkImageNewFromPixbuf(pixbuf)

frame$add(image)

# Animation #

label <- gtkLabelNew()
label$setMarkup("<u>Animation loaded from a file</u>")
vbox$packStart(label, FALSE, FALSE, 0)

frame <- gtkFrameNew()
frame$setShadowType("in")
align <- gtkAlignmentNew(0.5, 0.5, 0, 0)
align$add(frame)
vbox$packStart(align, FALSE, FALSE, 0)

filename <- "/usr/share/gtk-2.0/demo/floppybuddy.gif"
image <- gtkImageNewFromFile(filename)
frame$add(image)

# Progressive

label <- gtkLabelNew()
label$setMarkup("<u>Progressive image loading</u>")
vbox$packStart(label, FALSE, FALSE, 0)

frame <- gtkFrameNew()
frame$setShadowType("in")
align <- gtkAlignmentNew(0.5, 0.5, 0, 0)
align$add(frame)
vbox$packStart(align, FALSE, FALSE, 0)

# Create an empty image for now the progressive loader
# will create the pixbuf and fill it in.
#

image <- gtkImageNewFromPixbuf(NULL)
frame$add(image)

start.progressive.loading(image)

# Sensitivity control
button <- gtkToggleButtonNewWithMnemonic("_Insensitive")
vbox$packStart(button, FALSE, FALSE, 0)

gSignalConnect(button, "toggled", toggle.sensitivity.callback, vbox)

button <- gtkButtonNewWithLabel("Quit")
vbox$packStart(button, FALSE, FALSE, 0)
gSignalConnect(button, "clicked", function(...) { gtkMainQuit(); window$destroy() })


window$showAll()

gtkMain()

