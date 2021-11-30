tform_scale_type <- gClass("RTransformedHScale", "GtkHScale",
  .props = list(
    gParamSpec("R", "expr", "e", "Transformation of scale value",
      default.value = expression(x))
  ),
  .public = list(
    getExpr = function(self) self["expr"],
    getTransformedValue = function(self) self$transformValue(self$getValue())
  ),
  .private = list(
    transformValue = function(self, x) eval(self$expr, list(x = x))
  ),
  GtkScale = list(
    format_value = function(self, x)
      as.character(self$transformValue(x))
  )
)


adj <- gtkAdjustment(0.5, 0.15, 1.00, 0.05, 0.5, 0)
s <- gObject(tform_scale_type, adjustment = adj, expr = expression(x^3))

gSignalConnect(s, "value-changed", function(scale) {
  plot(ma_data, col = rgb(0,0,0,scale$getTransformedValue()),
    xlab = "Replicate 1", ylab = "Replicate 2", 
    main = "Expression levels of WT at time 0",  pch = 19)
})

#s <- gtkHScale(,0.15, 1.00, 0.05)

n <- 5000
backbone <- rnorm(n)
ma_data <- cbind(backbone, backbone+rnorm(n,,0.3))
library(cairoDevice)
win <- gtkWindow(show = F)
da <- gtkDrawingArea()
asCairoDevice(da)

vbox <- gtkVBox()
vbox$packStart(da)
vbox$packStart(s, FALSE)
win$add(vbox)
win$setDefaultSize(400,400)
win$showAll()

par(pty = "s")

#debug("[[.RGtkObject")
s$setValue(0.7)
