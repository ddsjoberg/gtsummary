create.bbox <- function(horizontal, title, spacing, layout)
{
  frame <- gtkFrameNew(title)

  if (horizontal)
    bbox <- gtkHButtonBoxNew()
  else
    bbox <- gtkVButtonBoxNew()

  bbox$setBorderWidth(5)
  frame$add(bbox)

  bbox$setLayout(layout)
  bbox$setSpacing(spacing)

  button <- gtkButtonNewFromStock("gtk-ok")
  bbox$add(button)

  button <- gtkButtonNewFromStock("gtk-cancel")
  bbox$add(button)

  button <- gtkButtonNewFromStock("gtk-help")
  bbox$add(button)

  frame
}

window <- gtkWindowNew("toplevel", show=F)
window$setTitle("Button Boxes")
window$setBorderWidth(10)

main.vbox <- gtkVBoxNew (FALSE, 0)
window$add(main.vbox)

frame.horz <- gtkFrameNew("Horizontal Button Boxes")

# expand, fill (expand greedily), with 10 padding
main.vbox$packStart(frame.horz, TRUE, TRUE, 10)

vbox <- gtkVBoxNew(FALSE, 0)
vbox$setBorderWidth(10)
frame.horz$add(vbox)

vbox$packStart(create.bbox(TRUE, "Spread", 40, "spread"), TRUE, TRUE, 0)
vbox$packStart(create.bbox(TRUE, "Edge", 40, "edge"), TRUE, TRUE, 5)
vbox$packStart(create.bbox(TRUE, "Start", 40, "start"), TRUE, TRUE, 5)
vbox$packStart(create.bbox(TRUE, "End", 40, "end"), TRUE, TRUE, 5)

frame.vert <- gtkFrameNew("Vertical Button Boxes")
main.vbox$packStart(frame.vert, TRUE, TRUE, 10)

hbox <- gtkHBoxNew(FALSE, 0)
hbox$setBorderWidth(10)
frame.vert$add(hbox)

hbox$packStart(create.bbox(FALSE, "Spread", 30, "spread"), TRUE, TRUE, 0)
hbox$packStart(create.bbox(FALSE, "Edge", 30, "edge"), TRUE, TRUE, 5)
hbox$packStart(create.bbox(FALSE, "Start", 30, "start"), TRUE, TRUE, 5)
hbox$packStart(create.bbox(FALSE, "End", 30, "end"), TRUE, TRUE, 5)

window$showAll()
