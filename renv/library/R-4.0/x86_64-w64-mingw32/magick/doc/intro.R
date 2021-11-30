## ---- echo = FALSE------------------------------------------------------------
dev.off <- function(){
  invisible(grDevices::dev.off())
}

## -----------------------------------------------------------------------------
library(magick)
str(magick::magick_config())

## ---- eval = require(rsvg, quietly = TRUE)------------------------------------
library(magick)
tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 350)
print(tiger)

## ---- eval = require(rsvg, quietly = TRUE)------------------------------------
tiger_png <- image_convert(tiger, "png")
image_info(tiger_png)

## -----------------------------------------------------------------------------
# Example image
frink <- image_read("https://jeroen.github.io/images/frink.png")

## -----------------------------------------------------------------------------

print(frink)

# Add 20px left/right and 10px top/bottom
image_border(image_background(frink, "hotpink"), "#000080", "20x10")

# Trim margins
image_trim(frink)

# Passport pica
image_crop(frink, "100x150+50")

# Resize
image_scale(frink, "300") # width: 300px
image_scale(frink, "x300") # height: 300px

# Rotate or mirror
image_rotate(frink, 45)
image_flip(frink)
image_flop(frink)

# Brightness, Saturation, Hue
image_modulate(frink, brightness = 80, saturation = 120, hue = 90)

# Paint the shirt orange
image_fill(frink, "orange", point = "+100+200", fuzz = 20)

## -----------------------------------------------------------------------------
# Add randomness
image_blur(frink, 10, 5)
image_noise(frink)

# Silly filters
image_charcoal(frink)
image_oilpaint(frink)
image_negate(frink)

## -----------------------------------------------------------------------------
kern <- matrix(0, ncol = 3, nrow = 3)
kern[1, 2] <- 0.25
kern[2, c(1, 3)] <- 0.25
kern[3, 2] <- 0.25
kern

## -----------------------------------------------------------------------------
img <- image_resize(logo, "300x300")
img_blurred <- image_convolve(img, kern)
image_append(c(img, img_blurred))

## -----------------------------------------------------------------------------
img %>% image_convolve('Sobel') %>% image_negate()
img %>% image_convolve('DoG:0,0,2') %>% image_negate()

## -----------------------------------------------------------------------------
# Add some text
image_annotate(frink, "I like R!", size = 70, gravity = "southwest", color = "green")

# Customize text
image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
  degrees = 60, location = "+50+100")

# Fonts may require ImageMagick has fontconfig
image_annotate(frink, "The quick brown fox", font = 'Times', size = 30)

## -----------------------------------------------------------------------------
frink <- image_read("https://jeroen.github.io/images/frink.png")
frink2 <- image_scale(frink, "100")
image_info(frink)
image_info(frink2)

## -----------------------------------------------------------------------------
test <- image_rotate(frink, 90)
test <- image_background(test, "blue", flatten = TRUE)
test <- image_border(test, "red", "10x10")
test <- image_annotate(test, "This is how we combine transformations", color = "white", size = 30)
print(test)

## -----------------------------------------------------------------------------
image_read("https://jeroen.github.io/images/frink.png") %>%
  image_rotate(270) %>%
  image_background("blue", flatten = TRUE) %>%
  image_border("red", "10x10") %>%
  image_annotate("The same thing with pipes", color = "white", size = 30)

## -----------------------------------------------------------------------------
# Download earth gif and make it a bit smaller for vignette
earth <- image_read("https://jeroen.github.io/images/earth.gif") %>%
  image_scale("200x") %>%
  image_quantize(128)

length(earth)
earth
head(image_info(earth))

rev(earth) %>% 
  image_flip() %>% 
  image_annotate("meanwhile in Australia", size = 20, color = "white")

## -----------------------------------------------------------------------------
bigdata <- image_read('https://jeroen.github.io/images/bigdata.jpg')
frink <- image_read("https://jeroen.github.io/images/frink.png")
logo <- image_read("https://jeroen.github.io/images/Rlogo.png")
img <- c(bigdata, logo, frink)
img <- image_scale(img, "300x300")
image_info(img)

## -----------------------------------------------------------------------------
image_mosaic(img)

## -----------------------------------------------------------------------------
image_flatten(img)

## -----------------------------------------------------------------------------
image_flatten(img, 'Add')
image_flatten(img, 'Modulate')
image_flatten(img, 'Minus')

## -----------------------------------------------------------------------------
image_append(image_scale(img, "x200"))

## -----------------------------------------------------------------------------
image_append(image_scale(img, "100"), stack = TRUE)

## -----------------------------------------------------------------------------
bigdatafrink <- image_scale(image_rotate(image_background(frink, "none"), 300), "x200")
image_composite(image_scale(bigdata, "x400"), bigdatafrink, offset = "+180+100")

## ---- eval = require(pdftools, quietly = TRUE)--------------------------------
manual <- image_read_pdf('https://cloud.r-project.org/web/packages/magick/magick.pdf', density = 72)
image_info(manual)
manual[1]

## -----------------------------------------------------------------------------
image_animate(image_scale(img, "200x200"), fps = 1, dispose = "previous")

## -----------------------------------------------------------------------------
newlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo.png"))
oldlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo-old.png"))
image_resize(c(oldlogo, newlogo), '200x150!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate(optimize = TRUE)

## -----------------------------------------------------------------------------
# Foreground image
banana <- image_read("https://jeroen.github.io/images/banana.gif")
banana <- image_scale(banana, "150")
image_info(banana)

## -----------------------------------------------------------------------------
# Background image
background <- image_background(image_scale(logo, "200"), "white", flatten = TRUE)

# Combine and flatten frames
frames <- image_composite(background, banana, offset = "+70+30")

# Turn frames into animation
animation <- image_animate(frames, fps = 10, optimize = TRUE)
print(animation)

## -----------------------------------------------------------------------------
# Produce image using graphics device
fig <- image_graph(width = 400, height = 400, res = 96)
ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
dev.off()

## -----------------------------------------------------------------------------
# Combine
out <- image_composite(fig, frink, offset = "+70+30")
print(out)

## -----------------------------------------------------------------------------
# Or paint over an existing image
img <- image_draw(frink)
rect(20, 20, 200, 100, border = "red", lty = "dashed", lwd = 5)
abline(h = 300, col = 'blue', lwd = '10', lty = "dotted")
text(30, 250, "Hoiven-Glaven", family = "monospace", cex = 4, srt = 90)
palette(rainbow(11, end = 0.9))
symbols(rep(200, 11), seq(0, 400, 40), circles = runif(11, 5, 35),
  bg = 1:11, inches = FALSE, add = TRUE)
dev.off()

## -----------------------------------------------------------------------------
print(img)

## -----------------------------------------------------------------------------
library(gapminder)
library(ggplot2)
img <- image_graph(600, 340, res = 96)
datalist <- split(gapminder, gapminder$year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
    scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) + 
    scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 2, optimize = TRUE)
print(animation)

## -----------------------------------------------------------------------------
plot(as.raster(frink))

## ---- fig.width=7, fig.height=5-----------------------------------------------
# Print over another graphic
plot(cars)
rasterImage(frink, 21, 0, 25, 80)

## ---- fig.width=5, fig.height=3-----------------------------------------------
library(ggplot2)
library(grid)
qplot(speed, dist, data = cars, geom = c("point", "smooth"))
grid.raster(frink)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("tesseract")

## ---- eval = require(tesseract, quietly = TRUE)-------------------------------
img <- image_read("http://jeroen.github.io/images/testocr.png")
print(img)

# Extract text
cat(image_ocr(img))

