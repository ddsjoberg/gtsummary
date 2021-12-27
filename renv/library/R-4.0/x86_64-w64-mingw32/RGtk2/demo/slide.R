##### PUZZLE CLASS ######
# warning: not yet stable
#source("/home/larman/research/RGtk2/RGtk2/demo/slide.R")
gClass("Puzzle", "GtkDrawingArea",
  .props = list( # note the flags parameter is missing (read/write/construct)
    gParamSpec("integer", "rows", "r", "Number of rows",, 0, 20, 0),
    gParamSpec("integer", "cols", "c", "Number of columns",, 0, 20, 0)
  ),
  .signals = list(list("puzzle_solved", flags = "first")),
  .private = list(
    pushBlock = function(self, block, xdelta, ydelta) {
      x <- round(self$x, 4)
      y <- round(self$y, 4)
      
      if (floor(y[block])-y[block] == 0 && xdelta != 0.0) {
        xdelta <- max(min(xdelta, .9), -0.9)
        pushed_x <- which(abs((x[block] + xdelta) - x[-length(x)]) < 1 & 
          abs(y[-length(y)] - y[block]) < 1)
        pushed_x <- pushed_x[pushed_x != block]
        if (!length(pushed_x)) {
          x[block] <- x[block] + xdelta
          x[block] <- max(min(x[block], self$cols-1), 0)
        } else {
          self$pushBlock(pushed_x, xdelta, 0)
          if (abs(x[block] - x[pushed_x])>1.0001) {
            if (x[block] < x[pushed_x])
              x[block] <- x[pushed_x] - 1
            else
              x[block] <- x[pushed_x] + 1
          }
        }
      }
    
      if (floor(x[block])-x[block] == 0 && ydelta != 0) {
        ydelta <- max(min(ydelta, .9), -0.9)
        pushed_y <- which(abs((y[block] + ydelta) - y[-length(y)]) < 1 & 
          abs(x[-length(x)] - x[block]) < 1)
        pushed_y <- pushed_y[pushed_y != block]
        if (!length(pushed_y)) {
          y[block] <- y[block] + ydelta
          y[block] <- max(min(y[block], self$rows-1), 0)
        } else {
          self$pushBlock(pushed_y, 0, ydelta)
          if (abs(y[block] - y[pushed_y])>1.0001) {
            if (y[block] < y[pushed_y])
              y[block] <- y[pushed_y] - 1
            else
              y[block] <- y[pushed_y] + 1
          }
        }
      }
      self$x <- x
      self$y <- y
    },
    boardInit = function(self) {
      pos <- sample(1:(self$rows * self$cols))
      y <- ceiling(pos/self$rows)
      x <- pos - ((y - 1) * self$rows)
      self$y <- matrix(y, ncol = self$cols)
      self$x <- matrix(x, ncol = self$cols)
      print(self$x)
      print(self$y)
      self$labels <- matrix(c(1:(self$cols*self$rows-1), "-"), ncol = self$cols)
      self$queueDraw()
    },
    queryPos = function(self, xpos, ypos) {
      x <- self$x
      y <- self$y
      
      cr <- gdkCairoCreate(self$window)
      cr$save()
      cr$scale(self$ratio_x, self$ratio_y)
      cr$translate(0.5, 0.5)
      for (item in seq(along = self$x)) {
        cr$save()
        cr$rectangleRound(x[item]-1.4, y[item]-1.4, 0.8, 0.8, 0.4)
        if (cr$inFill(xpos/self$ratio_x-0.5, ypos/self$ratio_y-0.5)) {
          cr$restore()
          cr$restore()
          return(item)
        }
        cr$restore()
      }
    
      cr$restore()
      return(-1)
    },
    boardSolved = function(self) {
      rows <- seq(length = self$rows)
      cols <- seq(length = self$cols)
      all(apply(self$x, 1, "==", cols)) && all(apply(self$y, 2, "==", rows))
    },
    drawItem = function(self, cr, style, item, grabbed)
    {
      x <- self$x
      y <- self$y
      aligned <- abs(floor(x[item])-x[item]) < 0.01 && 
        abs(floor(y[item])-y[item]) < 0.01

      cr$rectangleRound(x[item]-1.36, y[item]-1.36, 0.8, 0.8, 0.4)
      cr$setLineWidth(0.07)
      
      if (grabbed)
        gdkCairoSetSourceColor(cr, style$dark[[GtkStateType["normal"]+1]])
      else if (aligned)
        gdkCairoSetSourceColor(cr, style$dark[[GtkStateType["normal"]+1]])
      else gdkCairoSetSourceColor(cr, style$dark[[GtkStateType["active"]+1]])
    
      cr$stroke()
    
      cr$rectangleRound(x[item]-1.4, y[item]-1.4, 0.8, 0.8, 0.4)
      cr$save()
    
      if (grabbed)
        gdkCairoSetSourceColor(cr, style$base[[GtkStateType["selected"]+1]])
      else gdkCairoSetSourceColor(cr, style$base[[GtkStateType["normal"]+1]])
       
      cr$fill()
      cr$restore()
    
      if (grabbed)
        gdkCairoSetSourceColor(cr, style$mid[[GtkStateType["selected"]+1]])
      else if (aligned)
        gdkCairoSetSourceColor(cr, style$mid[[GtkStateType["normal"]+1]])
      else gdkCairoSetSourceColor(cr, style$dark[[GtkStateType["active"]+1]])
      
      cr$setLineWidth(0.05)
      cr$stroke()

      extents <- cr$textExtents(self$labels[item])$extents
      cr$moveTo(x[item]-1-extents$width/2 - extents$xBearing, 
        y[item] - 1 - extents$height/2 - extents$yBearing)
        
      if (grabbed)
        gdkCairoSetSourceColor(cr, style$text[[GtkStateType["selected"]+1]])
      else gdkCairoSetSourceColor(cr, style$text[[GtkStateType["normal"]+1]])
      
      cr$showText(self$labels[item])
    },
    drawBoard = function(self, cr) {
      cr$save()
      cr$selectFontFace("sans", "normal", "normal")
      cr$setFontSize(0.35)
      cr$scale(self$ratio_x, self$ratio_y)
      cr$translate(0.5, 0.5)
      style <- self$style
      grabbed <- self$grabbed
      x <- self$x
      sapply(seq(along = x[-length(x)]), function(item) 
        self$drawItem(cr, style, item, item == grabbed))
      cr$restore()
    },
    # Note that these event callbacks have a 'self' pointer that preceeds
    # the event handler signature - this is so we can store these as methods
    # and thus access the private/protected members of 'self'
    event_press = function(self, wid, bev) {
      self$cursorx <- bev$x
      self$cursory <- bev$y
      if (bev$button == 1) {
        self$grabbed <- self$queryPos(bev$x, bev$y)
        # request a redraw, since we've changed the state of our widget
        self$queueDraw()
      }
      return(FALSE)
    },
    event_release = function(self, wid, bev) {
      if (bev$button == 1) {
        self$grabbed <- -1
        # request a redraw, since we've changed the state of our widget 
        self$queueDraw()
      }
      return(FALSE)
    },
    event_motion = function(self, wid, mev) {
      if (self$grabbed >= 0) {
        xdelta <- (mev$x-self$cursorx) / self$ratio_x
        ydelta <- (mev$y-self$cursory) / self$ratio_y
      
        self$pushBlock(self$grabbed, xdelta, ydelta)
      
        if (self$boardSolved())
          gSignalEmit(self, "puzzle-solved")
      
        # request a redraw, since we've changed the state of our widget 
        self$queueDraw()
      
        self$cursorx <- mev$x
        self$cursory <- mev$y
      }
      return(FALSE)
    },
    paint = function(self, wid, eev) {
      cr <- gdkCairoCreate(self$window)
      self$ratio_x <- self$allocation$width/self$cols
      self$ratio_y <- self$allocation$height/self$rows
    
      self$drawBoard(cr)
      
      if (cr$status())
        stop("Cairo is unhappy: ", cr$status())
    
      # since we are requesting motion hints,. make sure another motion
      # event is delivered after redrawing the ui
      self$window$getPointer()
      return(FALSE)
    }
  ),
  GObject = list(
    set_property = function(self, id, value, pspec) {
      assignProp(self, pspec, value)
      self$boardInit()
    }
  ),
  .initialize = function(self) {
    self$cursorx <- self$cursory <- 0
    self$rows <- 4
    self$cols <- 4
    self$grabbed <- -1
    self$ratio_x <- self$ratio_y <- 1
    self$boardInit()
    self$showAll()
    
    self$setEvents(GdkEventMask["exposure-mask"] + GdkEventMask["pointer-motion-hint-mask"] +
      GdkEventMask["button1-motion-mask"] + GdkEventMask["button2-motion-mask"] +
      GdkEventMask["button3-motion-mask"] + GdkEventMask["button-press-mask"] +
      GdkEventMask["button-release-mask"])
    self$setSizeRequest(256, 256)
    gSignalConnect(self, "expose-event", self$paint)
    gSignalConnect(self, "motion-notify-event", self$event_motion)
    gSignalConnect(self, "button-press-event", self$event_press)
    gSignalConnect(self, "button-release-event", self$event_release)
  })

#### END PUZZLE CLASS, BEGIN APPLICATION CODE ####

INITIAL_WIDTH  <- 200
INITIAL_HEIGHT <- 210

puzzle <- NULL
puzzle_window <- NULL

rows <- 3
cols <- 3

puzzle_window_new <- function() {
  self <- gtkWindow("toplevel")
  self$setTitle("Sliding Gtk Puzzle")
  
  vbox <- gtkVBox(FALSE, 0)
  vbox$setBorderWidth(0)

  puzzle <<- gObject("Puzzle", rows = rows, cols = cols)

  puzzle$setSizeRequest(INITIAL_WIDTH, INITIAL_HEIGHT)

  gSignalConnect(puzzle, "puzzle-solved", puzzle_solved)
  
  vbox$add(puzzle)

  self$add(vbox)
  vbox$showAll()

  return(self)
}

puzzle_solved <- function(widget)
{
  parent <- widget$getTopLevel()

  dialog <- gtkDialog("GtkSlide - solved", parent, "modal", GTK_STOCK_OK, 
    GtkResponseType["accept"])

  label <- gtkLabel("You solved the puzzle!")
  label$setPadding(20, 20)
  label$show()

  dialog$vbox$add(label)
  dialog$show()

  dialog$run()
}

##### UTILITY FOR DRAWING ROUNDED RECTANGLES WITH CAIRO  #####

cairoRectangleRound <- function(cr, x0, y0, width, height, radius)
{
  x1 <- x0 + width
  y1 <- y0 + height

  if (!width || !height)
    return()
  if (width/2<radius) {
    if (height/2<radius) {
      cr$moveTo( x0, (y0 + y1)/2)
      cr$curveTo( x0 ,y0, x0, y0, (x0 + x1)/2, y0)
      cr$curveTo( x1, y0, x1, y0, x1, (y0 + y1)/2)
      cr$curveTo( x1, y1, x1, y1, (x1 + x0)/2, y1)
      cr$curveTo( x0, y1, x0, y1, x0, (y0 + y1)/2)
    } else {
      cr$moveTo( x0, y0 + radius)
      cr$curveTo( x0 ,y0, x0, y0, (x0 + x1)/2, y0)
      cr$curveTo( x1, y0, x1, y0, x1, y0 + radius)
      cr$lineTo(x1 , y1 - radius)
      cr$curveTo( x1, y1, x1, y1, (x1 + x0)/2, y1)
      cr$curveTo( x0, y1, x0, y1, x0, y1- radius)
    }
  } else {
    if (height/2<radius) {
      cr$moveTo( x0, (y0 + y1)/2)
      cr$curveTo( x0 , y0, x0 , y0, x0 + radius, y0)
      cr$lineTo(x1 - radius, y0)
      cr$curveTo( x1, y0, x1, y0, x1, (y0 + y1)/2)
      cr$curveTo( x1, y1, x1, y1, x1 - radius, y1)
      cr$lineTo(x0 + radius, y1)
      cr$curveTo( x0, y1, x0, y1, x0, (y0 + y1)/2)
    } else {
      cr$moveTo( x0, y0 + radius)
      cr$curveTo( x0 , y0, x0 , y0, x0 + radius, y0)
      cr$lineTo(x1 - radius, y0)
      cr$curveTo( x1, y0, x1, y0, x1, y0 + radius)
      cr$lineTo(x1 , y1 - radius)
      cr$curveTo( x1, y1, x1, y1, x1 - radius, y1)
      cr$lineTo(x0 + radius, y1)
      cr$curveTo( x0, y1, x0, y1, x0, y1- radius)
    }
  }

  cr$closePath()
}

#### RUN THE GAME ####
puzzle_window <- puzzle_window_new()
puzzle_window$show()
