
copyFile <- function(uri) {
  gfile <- gFileNewForUri(uri)
  fetched <- 0
  total <- 0
  
  output <- gfile$getBasename()
  con <- file(output, "wb")

  if (output == "/")
    output <- "index.html"

  streamReadCallback <- function(stream, result) {
    if (cancellable$isCancelled())
      return()
    data <- stream$readFinish(result)
    if (!length(data))
      dataFinished()
    else {
      dataRead(data)
      stream$readAsync(4096, cancellable = cancellable,
                       callback = streamReadCallback)
    }
  }

  readCallback <- function(gfile, result) {
    stream <- gfile$readFinish(result)
    info <- gfile$queryInfo(GFileAttributeStandard["size"])
    total <<- info$getAttributeUint64(GFileAttributeStandard["size"])
    stream$readAsync(4096, cancellable = cancellable,
                     callback = streamReadCallback)
  }

  dataRead <- function(data) {
    Sys.sleep(0.1)
    writeBin(data, con)
    fetched <<- fetched + length(data)
    if (!cancellable$isCancelled()) {
      progressBar$setFraction(fetched / total)
      bytesLabel$setText(paste("Bytes read:", fetched))
      ## Often, we never get around to updating the progress bar, due
      ## to the I/O events, which seem to take priority
      while(gtkEventsPending())
        gtkMainIteration()
    }
  }
  
  dataFinished <- function() {
    dialog$setTitle("Copy complete")
    close(con)
  }

  dialog <- gtkDialog("Copying...", NULL, 0,
                      GTK_STOCK_CANCEL, GtkResponseType["cancel"],
                      GTK_STOCK_CLOSE, GtkResponseType["none"])
  
  contentArea <- dialog$getContentArea()
  vbox <- gtkVBox()
  contentArea$add(vbox)
  progressBar <- gtkProgressBar()
  vbox$packStart(progressBar, FALSE, FALSE)
  srcLabel <- gtkLabel(paste("Source:", uri))
  srcLabel["max-width-chars"] <- 40
  srcLabel["ellipsize"] <- "middle"
  srcLabel["xalign"] <- 0
  vbox$packStart(srcLabel, FALSE, FALSE)
  destLabel <- gtkLabel(paste("Destination:", output))
  destLabel["max-width-chars"] <- 40
  destLabel["ellipsize"] <- "middle"
  destLabel["xalign"] <- 0
  vbox$packStart(destLabel, FALSE, FALSE)
  bytesLabel <- gtkLabel(paste("Bytes read:", fetched))
  bytesLabel["xalign"] <- 0
  vbox$packStart(bytesLabel, FALSE, FALSE)
  
  gSignalConnect(dialog, "response", function(dialog, response, data) {
    if (response == GtkResponseType["cancel"]) {
      cancellable$cancel()
    }
    dialog$destroy()
  })

  cancellable <- gCancellable()
  gfile$readAsync(cancellable = cancellable, callback = readCallback)
  
  invisible(dialog)
}

library(RGtk2)
options("RGtk2::newErrorHandling" = TRUE)

uri <- "file:///home/larman/projects/R/motifRG_0.0.1.tar.gz"
##uri <- "http://cran.r-project.org/src/contrib/RGtk2_2.12.18.tar.gz"
copyFile(uri)
