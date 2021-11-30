hpaned <- gtkHPaned()
frame1 <- gtkFrame()
frame2 <- gtkFrame()
frame1$setShadowType("in")
frame2$setShadowType("in")

hpaned$setSizeRequest(200 + hpaned$styleGet("handle-size"), -1)

hpaned$pack1(frame1, TRUE, FALSE)
frame1$setSizeRequest(50, -1)

hpaned$pack2(frame2, FALSE, FALSE)
frame2$setSizeRequest(50, -1)

