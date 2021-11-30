require(cairoDevice)

pixmap <- gdkPixmapNew(w=500, h=500, depth=24)
asCairoDevice(pixmap)

x <- rnorm(50)

opar <- c(par(bg = "white"))

plot(x, ann = FALSE, type = "n")

abline(h = 0, col = gray(0.9))

lines(x, col = "green4", lty = "dotted")
points(x, bg = "limegreen", pch = 21)

title(main = "Simple Use of Image Compositing", xlab = "RGtk2 does graphics", 
    col.main = "blue", col.lab = gray(0.8), cex.main = 1.2, cex.lab = 1, 
    font.main = 4, font.lab = 3)
 
filename <- imagefile("rgtk-logo.gif")
pixbuf <- gdkPixbufNewFromFile(filename)[[1]]
pixbuf <- pixbuf$addAlpha(TRUE, 248, 248, 248)

plot_pixbuf <- gdkPixbufGetFromDrawable(NULL, pixmap, pixmap$getColormap(), 
  0, 0, 0, 0, 500, 500)

w <- pixbuf$getWidth()
h <- pixbuf$getHeight()
dw <- (plot_pixbuf$getWidth() - w) / 2 
dh <- (plot_pixbuf$getHeight() - h) / 2

pixbuf$composite(plot_pixbuf, dw, dh, w, h, dw, dh, 1.0, 1.0, "bilinear", 100)

plot_pixbuf$save("myplot.jpg", "jpeg")
