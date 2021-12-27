expose_cb <- function(widget, event, data)
{
  dest <- gdkPixbuf(color = "rgb", has.alpha = FALSE, bits = 8, 
                    w = event[["area"]]$width, h = event[["area"]]$height)

  area <- event[["area"]]
  pixbuf$compositeColor(dest, 0, 0, area$width, area$height,
                        -area$x, -area$y,
                        widget[["allocation"]]$width / pixbuf$getWidth(),
                        widget[["allocation"]]$height / pixbuf$getHeight(),
                        "bilinear", 255,
                        area$x, area$y, 16, 0xaaaaaa, 0x555555)

  dest$renderToDrawable(widget[["window"]],
                        widget[["style"]][["fgGc"]][[GtkStateType["normal"]+1]],
                        0, 0, area$x, area$y,
                        area$width, area$height,
                        "normal", area$x, area$y)

  return(TRUE)
}
