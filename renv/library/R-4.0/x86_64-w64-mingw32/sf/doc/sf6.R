## ----echo=FALSE, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.height = 4.5)
knitr::opts_chunk$set(fig.width = 6)
knitr::opts_chunk$set(collapse = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  rgdal::make_EPSG()

## -----------------------------------------------------------------------------
library(sf)
demo(nc, ask = FALSE, echo = FALSE)
nc$geom2 = st_centroid(st_geometry(nc))
print(nc, n = 2)

## -----------------------------------------------------------------------------
plot(st_geometry(nc))
st_geometry(nc) <- "geom2"
plot(st_geometry(nc))

## ----eval=FALSE---------------------------------------------------------------
#  i = sf::st_intersects(sf1, sf2)

## ----eval=FALSE---------------------------------------------------------------
#  library(sf)

## -----------------------------------------------------------------------------
polygon = st_sfc(st_polygon(list(rbind(c(0,80), c(120,80), c(240,80), c(0,80)))), 
		crs = 4326)
pole = st_sfc(st_point(c(0,90)), crs = 4326)
st_intersects(polygon, pole)

## -----------------------------------------------------------------------------
st_centroid(polygon)[[1]]

## -----------------------------------------------------------------------------
pt = st_sfc(st_point(c(0,0)), crs = 4326)
buf = st_buffer(polygon, 1)
buf = st_buffer(polygon, units::set_units(1, degree))

