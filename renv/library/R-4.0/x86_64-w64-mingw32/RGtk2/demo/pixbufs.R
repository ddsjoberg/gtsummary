# source("~/research/RGtk/inst/demo/pixbufs.R")

FRAME.DELAY <- 50
BACKGROUND.NAME <- "background.jpg"

image.names <- c(
  "apple-red.png",
  "gnome-applets.png",
  "gnome-calendar.png",
  "gnome-foot.png",
  "gnome-gmush.png",
  "gnome-gimp.png",
  "gnome-gsame.png",
  "gnu-keys.png"
)

N.IMAGES <- length(image.names)

# demo window
window <- NULL

# Current frame
frame <- NULL

# Background image
background <- NULL
back.width <- NULL
back.height <- NULL

# Images
images <- list(N.IMAGES)

# Widgets
da <- NULL

# Loads the images for the demo, throwing an error if there's a problem
load.pixbufs <- function()
{
  if (!is.null(background))
    return() # already loaded earlier

  filename <- imagefile(BACKGROUND.NAME)
  if (!file.exists(filename))
    stop("Could not find background image file:", filename)

  background.result <- gdkPixbufNewFromFile(filename)

  if (is.null(background.result[[1]]))
    stop("Could not load background image:", background.result$error$message)
  background <<- background.result[[1]]

  back.width <<- background$getWidth()
  back.height <<- background$getHeight()

  for (i in 1:N.IMAGES)
    {
      filename <- imagefile(image.names[i])
      if (!file.exists(filename))
          stop("Could not find image file:", image.names[i])

      image.result <- gdkPixbufNewFromFile(filename)

      if (is.null(image.result[[1]]))
          stop("Could not load image", image.names[i], ":", image.result$error$message)
      images[[i]] <<- image.result[[1]]
    }

  return()
}

# Expose callback for the drawing area
expose.cb <- function(widget, event, data)
{
  #rowstride <- frame$getRowstride()

  #pixels <- frame$getPixels() # get the image data that was exposed
  #pixels <- pixels[(rowstride * event[["area"]][["y"]] + event[["area"]][["x"]] * 3):length(pixels)]

  cr <- gdkCairoCreate(widget[["window"]])
  gdkCairoSetSourcePixbuf(cr, frame, 0, 0)
  gdkCairoRectangle(cr, event[["area"]])
  cr$fill()
  
  return(TRUE)
}

CYCLE.LEN <- 60

frame.num <- 0

# Timeout handler to regenerate the frame
timeout <- function(data)
{
  background$copyArea(0, 0, back.width, back.height, frame, 0, 0)

  f <- (frame.num %% CYCLE.LEN) / CYCLE.LEN

  xmid <- back.width / 2
  ymid <- back.height / 2

  radius <- min(xmid, ymid) / 2

  for (i in 1:N.IMAGES)
    {
      ang <- 2 * pi * i / N.IMAGES - f * 2 * pi

      iw <- images[[i]]$getWidth()
      ih <- images[[i]]$getHeight()

      r <- radius + (radius / 3) * sin(f * 2.0 * pi)

      xpos <- floor(xmid + r * cos(ang) - iw / 2 + 0.5)
      ypos <- floor(ymid + r * sin(ang) - ih / 2 + 0.5)

      k <- sin(f * 2.0 * pi)
      alpha <- max(127, abs(255 * sin(f * 2.0 * pi)))
      if (i %% 2 == 0) {
          k <- cos(f * 2.0 * pi)
          alpha <- max(127, abs(255 * cos(f * 2.0 * pi)))
      }
      k <- 2.0 * k * k
      k <- max(0.25, k)

      r1 <- c(xpos, ypos, iw * k, ih * k)

      r2 <- c(0, 0, back.width, back.height)

      inter <- gdkRectangleIntersect(r1, r2)
      if (inter[[1]]) {
          dest <- inter$dest
          images[[i]]$composite(frame, dest$x, dest$y, dest$width, dest$height, xpos, ypos, k, k, "nearest", alpha)
      }
    }

  da$queueDraw()

  frame.num <<- frame.num + 1
  return(TRUE)
}

timeout.id <- NULL

cleanup.callback <- function(object, data)
{
  gSourceRemove(timeout.id)
  timeout.id <<- NULL
}

window <- gtkWindowNew("toplevel", show = F)
window$setTitle("Pixbufs")
window$setResizable(FALSE)

gSignalConnect(window, "destroy", cleanup.callback)

error <- try(load.pixbufs())
if (inherits(error, "try-error"))
{
      dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "error", "close",
                       "Problem loading images:", error)
      gSignalConnect(dialog, "response", gtkWidgetDestroy)
} else {
      window$setSizeRequest(back.width, back.height)

      frame <- gdkPixbufNew("rgb", FALSE, 8, back.width, back.height)

      da <- gtkDrawingAreaNew()

      gSignalConnect(da, "expose_event", expose.cb)

      window$add(da)

      timeout.id <- gTimeoutAdd(FRAME.DELAY, timeout)

      window$showAll()
	  
	  print("TIP: Hold down the spacebar for the objects to move")
}

