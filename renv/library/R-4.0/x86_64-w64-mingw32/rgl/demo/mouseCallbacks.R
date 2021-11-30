xprod <- function(a, b) 
    c(a[2]*b[3] - a[3]*b[2],
       a[3]*b[1] - a[1]*b[3],
       a[1]*b[2] - a[2]*b[1])
       
vlen <- function(a) sqrt(sum(a^2))

angle <- function(a,b) {
    dot <- sum(a*b)
    acos(dot/vlen(a)/vlen(b))
}
 
clamp <- function(x, min, max)  pmin(pmax(x, min), max)

mouseNone <- function(button = 1, dev = cur3d() ) {

    cur <- cur3d()
    
    for (i in dev) {
        set3d(i, TRUE)
        rgl.setMouseCallbacks(button, begin = NULL, update = NULL, end = NULL, dev = dev)
    }
    set3d(cur)
}

mouseTrackball <- function(button = 1, dev = cur3d() ) {
    width <- height <- rotBase <- NULL
    userMatrix <- list()
    cur <- cur3d()
    
    screenToVector <- function(x, y) {
      radius <- max(width, height)/2
      centre <- c(width, height)/2
      pt <- (c(x, y) - centre)/radius
      len <- vlen(pt)

      if (len > 1.e-6) pt <- pt/len

      maxlen <- sqrt(2)
      angle <- (maxlen - len)/maxlen*pi/2
      z <- sin(angle)
      len <- sqrt(1 - z^2)
      pt <- pt * len
      return(c(pt, z))
    }
    
    trackballBegin <- function(x, y) {
        vp <- par3d("viewport")
        width <<- vp[3]
        height <<- vp[4]
        cur <<- cur3d()
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else userMatrix[[i]] <<- par3d("userMatrix")
        }
        set3d(cur, TRUE)
        rotBase <<- screenToVector(x, height - y)
    }
    
    trackballUpdate <- function(x,y) {
        rotCurrent <- screenToVector(x, height - y)
        angle <- angle(rotBase, rotCurrent)
        axis <- xprod(rotBase, rotCurrent)
        mouseMatrix <- rotationMatrix(angle, axis[1], axis[2], axis[3])
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else par3d(userMatrix = mouseMatrix %*% userMatrix[[i]])
        }
        set3d(cur, TRUE)
    }
    
    for (i in dev) {
        set3d(i, TRUE)
        rgl.setMouseCallbacks(button, begin = trackballBegin, update = trackballUpdate, end = NULL,
                              dev = dev)
    }
    set3d(cur, TRUE)
}

mouseXAxis<- function(button = 1, dev = cur3d() , left=TRUE) {
    mouseOneAxis(button, dev, axis=c(1,0,0), left=left) 
}

mouseYAxis<- function(button = 1, dev = cur3d(), left = TRUE ) {
    mouseOneAxis(button, dev, axis=c(0,1,0), left=left) 
}

mouseZAxis<- function(button = 1, dev = cur3d(), left=TRUE) {
    mouseOneAxis(button, dev, axis=c(0,0,1), left=left) 
}

mouseOneAxis <- function(button = 1, dev = cur3d(), axis = c(1,0,0), left = TRUE ) {
    width <- height <- rotBase <- NULL
    userMatrix <- list()
    cur <- cur3d()
    
    screenToVector <- function(x, y) {
      radius <- max(width, height)/2
      centre <- c(width, height)/2
      pt <- (c(x, y) - centre)/radius
      len <- vlen(pt)

      if (len > 1.e-6) pt <- pt/len

      maxlen <- sqrt(2)
      angle <- (maxlen - len)/maxlen*pi/2
      z <- sin(angle)
      len <- sqrt(1 - z^2)
      pt <- pt * len
      return(c(pt, z))
    }

    oneAxisBegin <- function(x, y) {
        vp <- par3d("viewport")
        width <<- vp[3]
        height <<- vp[4]
        cur <<- cur3d()
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else userMatrix[[i]] <<- par3d("userMatrix")
        }
        set3d(cur, TRUE)
        rotBase <<- screenToVector(x, height/2)
    }
    
    oneAxisUpdate <- function(x,y) {
        rotCurrent <- screenToVector(x, height/2)
        angle <- rotCurrent[1] - rotBase[1]
        mouseMatrix <- rotationMatrix(angle, axis[1], axis[2], axis[3])
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else {
		if (left)
		    par3d(userMatrix = mouseMatrix %*% userMatrix[[i]])
		else
		    par3d(userMatrix = userMatrix[[i]] %*% mouseMatrix)
	    }
        }
        set3d(cur, TRUE)
    }
    
    for (i in dev) {
        set3d(i, TRUE)
        rgl.setMouseCallbacks(button, begin = oneAxisBegin, update = oneAxisUpdate, end = NULL, dev = dev)
    }
    set3d(cur, TRUE)
}

mousePolar <- function(button = 1, dev = cur3d()) {
   
    screenToPolar <- function(x,y) {
    	r <- min(width, height)/2
    	dx <- clamp(x - width/2, -r, r)
    	dy <- clamp(y - height/2, -r, r)
    	return( asin( c(dx, -dy)/r ) )
    }

    cur <- cur3d()
    width <- height <- dragBase <- dragCurrent <- NULL
    camBase <- list()
    
    polarBegin <- function(x, y) {
        vp <- par3d("viewport")
        width <<- vp[3]
        height <<- vp[4]
        dragBase <<- screenToPolar(x, y)
    	cur <<- cur3d()
    	for (i in dev) {
	    if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else {
		m <- par3d("userMatrix")
		svd <- svd(m[1:3, 1:3])
		m[1:3, 1:3] <- svd$u %*% t(svd$v)
		theta <- atan2(-m[1,3], m[1,1])
		m <-  m %*% rotationMatrix(theta, 0,1,0)
		svd <- svd(m[1:3, 1:3])
		m[1:3,1:3] <- svd$u %*% t(svd$v)	
		phi <- atan2(-m[2,3], m[3,3])
		camBase[[i]] <<- c(theta, phi)
	    }
	}
        set3d(cur, TRUE)
    }   
    
    polarUpdate <- function(x,y) {
        dragCurrent <<- screenToPolar(x, y)        
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else {
		newpos <- camBase[[i]] - ( dragCurrent - dragBase )
		newpos[2] <- clamp(newpos[2], -pi/2, pi/2)
		mouseMatrix <- rotationMatrix(newpos[2], 1, 0, 0) %*% rotationMatrix(-newpos[1], 0, 1, 0)
		par3d(userMatrix = mouseMatrix) 
	    }
        }
        set3d(cur, TRUE)
    }
    
    for (i in dev) {
	set3d(i, TRUE)
	rgl.setMouseCallbacks(button, begin = polarBegin, update = polarUpdate, end = NULL, dev = dev)
    }
    set3d(cur, TRUE)
}

# Set background colour based on x,y position in the window

mouseBG <- function(button = 1, dev = cur3d(), init = "white", rate = cbind(c(1,0,1),c(0,1,1)), space = c("rgb", "hsv")) {
    cur <- cur3d()
    space <- match.arg(space)
    init <- col2rgb(init)/255
    if (space == "hsv")
    	init <- rgb2hsv(init*255)

    width <- height <- lambda0 <- lambda <- NULL
    
    bgBegin <- function(x, y) {
        lambda0 <<- c(x/width, 1-y/height)    # nolint
        vp <- par3d("viewport")
    	width <<- vp[3]
    	height <<- vp[4]
    }
        
    bgUpdate <- function(x,y) {
        lambda <<- c(x/width, 1-y/height) - lambda0
        color <- clamp(init + rate %*% lambda, 0, 1)
        x <- color[1]
        y <- color[2]
        z <- color[3]
        if (space == "rgb")
            color <- rgb(x,y,z)    
        else
            color <- hsv(x,y,z)
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else bg3d(color=color)  
        }
        set3d(cur, TRUE)
    }
    
    bgEnd <- function() {
        init <<- clamp(init + rate %*% lambda, 0, 1)
    }
    
    for (i in dev) {
        set3d(i, TRUE)
        rgl.setMouseCallbacks(button, begin = bgBegin, update = bgUpdate, end = bgEnd, dev = dev)
    }
    set3d(cur, TRUE)
}

# Set time using an arbitrary par3dinterp function

mouseInterp <- function(button = 1, dev = cur3d(), fn, init = 0, range = NULL, direction=c(1,0)) {
    cur <- cur3d()
    time <- init
    x0 <- width <- NULL
    
    interpBegin <- function(x, y) {
    	vp <- par3d("viewport")
        width <<- vp[3]
        x0 <<- sum(direction*c(x,y))
    }
        
    interpUpdate <- function(x,y) {
        time <<- init + (sum(direction*c(x,y)) - x0)/width
        if (!is.null(range)) time <<- clamp(time, range[1], range[2])
        for (i in dev) {
            if (inherits(try(set3d(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else par3d(fn(time))
        }
        set3d(cur, TRUE)
    }
    
    interpEnd <- function() {
        init <<- time
    }
    
    for (i in dev) {
        set3d(i, TRUE)
        rgl.setMouseCallbacks(button, begin = interpBegin, update = interpUpdate, end = interpEnd, dev = dev)
    }
    set3d(cur, TRUE)
}

mouseZoom <- function(button = 1, dev = cur3d()) 
    mouseInterp(button,dev=dev,fn=par3dinterp(times=c(-4,4)/4, zoom=c(10^(-4),10^4),method="linear"),
                      init=log10(par3d("zoom"))/4,range=c(-4,4)/4,direction=c(0,-1))
 
mouseFOV <- function(button = 1, dev = cur3d())
    mouseInterp(button,dev=dev,fn=par3dinterp(times=c(1,179)/180, FOV=c(1,179),method="linear"), 
                      init=par3d("FOV")/180, range = c(1,179)/180, direction=c(0,1))
                      
# Synchronize mouse control of two windows for stereo view
example(surface3d, echo = FALSE)
par3d(windowRect= c(0,32,512,544), userMatrix = rotationMatrix(5*pi/180, 0,1,0) %*% par3d("userMatrix") )
w1 <- cur3d()
example(surface3d, echo = FALSE)
par3d(windowRect = c(512,32,1024,544))
w2 <- cur3d()
mouseTrackball(dev=c(w1,w2))
mouseZoom(2,dev=c(w1,w2))
mouseFOV(3,dev=c(w1,w2))
