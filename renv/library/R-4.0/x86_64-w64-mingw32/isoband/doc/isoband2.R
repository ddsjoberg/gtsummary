## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(isoband)
library(ggplot2)
suppressWarnings(library(sf))

m <- volcano

# make isobands
b <- isobands((1:ncol(m))/(ncol(m)+1), (nrow(m):1)/(nrow(m)+1), m, 10*(9:19), 10*(10:20))
bands <- iso_to_sfg(b)
data_bands <- st_sf(
  level = 1:length(bands),
  geometry = st_sfc(bands)
)

# make isolines
l <- isolines((1:ncol(m))/(ncol(m)+1), (nrow(m):1)/(nrow(m)+1), m, 10*(10:19))
lines <- iso_to_sfg(l)
data_lines <- st_sf(
  level = 2:(length(lines)+1),
  geometry = st_sfc(lines)
)

# plot with geom_sf()
ggplot() +
  geom_sf(data = data_bands, aes(fill = level), color = NA, alpha = 0.7) +
  geom_sf(data = data_lines, color = "black") +
  scale_fill_viridis_c(guide = "none") +
  coord_sf(expand = FALSE)

## -----------------------------------------------------------------------------
suppressMessages(library(magick))

# helper function to convert a raster image into isobands
sf_from_image <- function(image) {
  image_gray <- image %>% image_quantize(colorspace = "gray")
  image_raster <- as.raster(image_gray)
  d <- dim(image_raster)
  m <- matrix(c((255-col2rgb(image_raster)[1,])), nrow = d[1], ncol = d[2], byrow = TRUE)
  b <- isobands(1:d[2], d[1]:1, m, 20*(0:13), 20*(1:14))
  bands <- iso_to_sfg(b)
  data <- st_sf(
    level = letters[1:length(bands)],
    geometry = st_sfc(bands)
  )
}

# load the image, convert, and plot
img <- image_resize(image_read(system.file("extdata", "ocean-cat.jpg", package = "isoband")), "200x200")
img_sf <- sf_from_image(img)

ggplot(img_sf) + 
  geom_sf(color = "blue", fill = NA, size = 0.05) + 
  coord_sf(expand = FALSE) +
  theme_gray() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = grid::unit(0, "pt"),
    plot.margin = margin(0, 0, 0, 0)
  )


ggplot(img_sf) + 
  geom_sf(aes(fill = level, color = level)) + 
  coord_sf(expand = FALSE) +
  theme_void() + 
  scale_fill_viridis_d(
    aesthetics = c("color", "fill"), option = "B", guide = "none",
    direction = -1
  )

## -----------------------------------------------------------------------------
m <- matrix(
  c(1.5, 1.5, 1.5, 1.5, 0.6, 0,
    0.5, 1.5, 1.5,   0,   0, 0,
      0,   1,   0,   1,   1, 0,
      0,   1,   0, 0.7,   0, 0,
    0.9, 1.3, 1.8, 1.4, 0.4, 0
  ),
  nrow = 5, ncol = 6, byrow = TRUE
)

plot_iso(m, 1, 2)

## -----------------------------------------------------------------------------
b <- isobands(x = 1:6, y = 5:1, z = m, levels_low = 0:1, levels_high = 1:2)
bands <- iso_to_sfg(b)

iso <- st_sf(
  id = factor(1:length(bands)),
  geometry = st_sfc(bands)
)

st_is_valid(iso, reason = TRUE)

## -----------------------------------------------------------------------------
ggplot(iso, aes(fill = id)) + 
  geom_sf() +
  theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
if (sf_extSoftVersion()["GEOS"] >= "3.8.0") { # requires GEOS >= 3.8.0
  iso_valid <- st_make_valid(iso)
  st_is_valid(iso_valid, reason=TRUE)
}

## -----------------------------------------------------------------------------
if (sf_extSoftVersion()["GEOS"] >= "3.8.0") { # requires GEOS >= 3.8.0
  ggplot(iso_valid, aes(fill = id)) +
    geom_sf() +
    theme(legend.position = "bottom")
}

## -----------------------------------------------------------------------------
b <- isobands(x = 1:6, y = 5:1, z = m + 1e-10, levels_low = 0:1, levels_high = 1:2)
bands <- iso_to_sfg(b)
iso <- st_sf(id = factor(1:length(bands)), geometry = st_sfc(bands))
st_is_valid(iso, reason = TRUE)

ggplot(iso, aes(fill = id)) +
  geom_sf() +
  theme(legend.position = "bottom")

