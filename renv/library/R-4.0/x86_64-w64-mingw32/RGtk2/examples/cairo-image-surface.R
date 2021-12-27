stride <- format$strideForWidth(width)
data <- raw(stride * height)
surface <- cairoImageSurfaceCreateForData(data, format, width, height, stride)
