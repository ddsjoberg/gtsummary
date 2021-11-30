## this demo show you how to download images from the Internet and
## create an animation using the animation package

library(animation)
saveHTML({
  ## a custom file list on the Internet
  extList = c('http://i.imgur.com/rJ7xF.jpg',
              'http://i.imgur.com/Lyr9o.jpg',
              'http://i.imgur.com/18Qrb.jpg')
  for (i in 1:length(extList)) {
    download.file(url = extList[i], destfile = sprintf(ani.options('img.fmt'), i), mode = 'wb')
  }
}, use.dev = FALSE, ani.width = 640, ani.height = 480, ani.type = 'jpg',
         interval = 2, single.opts = "'dwellMultiplier': 1")
