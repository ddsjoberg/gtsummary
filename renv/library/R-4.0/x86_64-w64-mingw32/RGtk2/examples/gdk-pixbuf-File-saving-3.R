contents <- readBin("/home/hughsie/.color/icc/L225W.icm", what="raw")
contents_encode <- RCurl::base64(contents)
pixbuf$save(handle, "png", "icc-profile", contents_encode)
