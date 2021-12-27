randomDot <- function(left, right, rightOffset=c(200, 0), n=3000, ...) {
  old <- cur3d()
  on.exit(set3d(old))  
  force(left)
  force(right)
  set3d(left)
  leftViewport <- par3d("viewport")
  leftSize <- leftViewport[3:4]
  leftProj <- rgl.projection()
  leftDepth <- rgl.pixels("depth")  
  leftUR <- leftViewport[1:2] + leftSize - 1
  set3d(right)
  rightViewport <- par3d("viewport")
  rightSize <- rightViewport[3:4]
  rightProj <- rgl.projection()
  rightDepth <- rgl.pixels("depth")
  rightUR <- rightViewport[1:2] + rightSize - 1

  size <- pmax(leftViewport[3:4], rightViewport[3:4]+rightOffset)
  
  pts <- matrix(c(sample(leftSize[1], n, replace=TRUE),
                  sample(leftSize[2], n, replace=TRUE)), n, 2)
  cols <- 1:n
  startpt <- pts
  startcols <- cols
  keep <- startpt[,1] < leftSize[1] & startpt[,2] < leftSize[2]
  pt <- startpt[keep,,drop=FALSE]
  cl <- startcols[keep]
  while (length(pt)) {
    depth <- leftDepth[pt]

    user <- rgl.window2user((pt[,1]-0.5)/leftSize[1], (pt[,2]-0.5)/leftSize[2],
                          depth, projection=leftProj)
    win <- rgl.user2window(user, projection=rightProj)
    bkgd <- cbind((pt[,1] - 0.5)/rightSize[1], (pt[,2] - 0.5)/rightSize[2], 1)
    usewin <- rep(depth < 1, 3)
    rightPt <- structure(ifelse(usewin, win, bkgd), dim=dim(win))
    rightPti <- round(cbind(rightSize[1]*rightPt[,1], rightSize[2]*rightPt[,2]) + 0.5)
    
    keep <- rightPti[,1] >= 1 & rightPti[,1] <= rightUR[1] &
            rightPti[,2] >= 1 & rightPti[,2] <= rightUR[2]
            
    rightPti <- rightPti[keep,,drop=FALSE]
    rightPt <- rightPt[keep,,drop=FALSE]
    cl <- cl[keep]            
    keep <- TRUE | rightPt[,3] <= rightDepth[ rightPti ]+0.001
    rightPti <- rightPti[keep,,drop=FALSE]            
    cl <- cl[keep]

    pt <- cbind(rightPti[,1] + rightOffset[1], rightPti[,2] + rightOffset[2])
    pts <- rbind(pts, pt)
    cols <- c(cols, cl)
    keep <- apply(pt, 1, min) >= 1 &
            pt[,1] <= leftUR[1] &
            pt[,2] <= leftUR[2] 
    pt <- pt[keep,,drop=FALSE]
    cl <- cl[keep]
  } 
  
  pt <- cbind(startpt[,1] - rightOffset[1], startpt[,2] - rightOffset[2])
  keep <- pt[,1] > 1 & pt[,1] < rightSize[1] & 
          pt[,2] > 1 & pt[,2] < rightSize[2] 
  pt <- pt[ keep,,drop=FALSE ]
  cl <- startcols[ keep ]
  
  while (length(pt)) {
    depth <- rightDepth[pt]
    user <- rgl.window2user((pt[,1]-0.5)/rightSize[1], (pt[,2]-0.5)/rightSize[2],
                          depth, projection=rightProj)
    win <- rgl.user2window(user, projection=leftProj)
    bkgd <- cbind((pt[,1] - 0.5)/leftSize[1], (pt[,2] - 0.5)/leftSize[2], 1)
    usewin <- rep(depth < 1, 3)
    leftPt <- structure(ifelse(usewin, win, bkgd), dim=dim(win))
    leftPti <- round(cbind(leftSize[1]*leftPt[,1], leftSize[2]*leftPt[,2]) + 0.5)

    keep <- leftPti[,1] >= 1 & leftPti[,1] <= leftUR[1] &
            leftPti[,2] >= 1 & leftPti[,2] <= leftUR[2]
            
    leftPti <- leftPti[keep,,drop=FALSE]
    leftPt <- leftPt[keep,,drop=FALSE]
    cl <- cl[keep]            
    keep <- TRUE | leftPt[,3] <= leftDepth[ leftPti ]+0.001
    leftPti <- leftPti[keep,,drop=FALSE]            
    cl <- cl[keep]
    
    pt <- leftPti
    pts <- rbind(pts, pt)
    cols <- c(cols, cl)
    pt <- cbind(pt[,1] - rightOffset[1], pt[,2] - rightOffset[2])
    keep <- apply(pt, 1, min) >= 1 &
            pt[,1] <= rightUR[1] &
            pt[,2] <= rightUR[2]
    pt <- pt[keep,,drop=FALSE]
    cl <- cl[keep] 
  }
  plot(pts, col = cols, pch=16, axes=FALSE,cex=0.25+cols/n/2,xlab="",ylab="",...)
  rug((size[1] + c(-1,1)*rightOffset[1])/2, side=1)
  rug((size[1] + c(-1,1)*rightOffset[1])/2, side=3)
  rug((size[2] + c(-1,1)*rightOffset[2])/2, side=2)
  rug((size[2] + c(-1,1)*rightOffset[2])/2, side=4)
}

                                                #red                   #cyan 
anaglyph <- function(left, right, leftColor = c(1,0,0), rightColor = c(0,1,1),
                     dimens = dim(leftPixels)) {
  old <- cur3d()
  on.exit(set3d(old))  
  force(left)
  force(right)

  set3d(left)
  vp <- par3d("viewport")
  leftPixels <- rgl.pixels(viewport=vp)
  leftPixels <- t((leftPixels[,,1]+leftPixels[,,2]+leftPixels[,,3])/3)
  leftPixels <- leftPixels[rev(seq_len(dimens[1])), seq_len(dimens[2])]
  set3d(right)
  rightPixels <- rgl.pixels(viewport=vp)
  rightPixels <- t((rightPixels[,,1]+rightPixels[,,2]+rightPixels[,,3])/3)
  rightPixels <- rightPixels[rev(seq_len(dimens[1])), seq_len(dimens[2])]
  red <- pmin(leftPixels*leftColor[1] + rightPixels*rightColor[1], 1)
  green <- pmin(leftPixels*leftColor[2] + rightPixels*rightColor[2], 1)
  blue <- pmin(leftPixels*leftColor[3] + rightPixels*rightColor[3], 1)
  z <- as.raster(array(c(red, green, blue), dim = c(dimens, 3)))
  if (length(z)) {
    par(mar = c(0,0,0,0))
    plot(z)
  } else {
    cat("Unable to read pixels:\nstr(leftPixels):\n")
    str(leftPixels)
    cat("str(rightPixels):\n")
    str(rightPixels)
  }
}

if (!rgl.useNULL()) {
  source(system.file("demo/mouseCallbacks.R", package="rgl"), echo=FALSE )

# This version assumes the eyes diverge for the stereo view.  
# Reverse the two arguments for the cross-eyed view.

  dev.new(width=9, height=7)
  randomDot(cur3d()-1, cur3d())

# A red-cyan anaglyph (for 3D glasses).  Use optional args to anaglyph for other glasses.

  dev.new()
  anaglyph(cur3d()-1, cur3d())
  
} else
  cat("Can't read pixels from a NULL device\n")
