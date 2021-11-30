## ----setup, include=FALSE-----------------------------------------------------
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available("1.14")) {
  warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14.  These were not found. Older versions will not work.")
  knitr::knit_exit()
}
knitr::opts_chunk$set(echo = TRUE)
library(rgl)
options(rgl.useNULL = TRUE)
setupKnitr(autoprint = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github(c("r-lib/downlit",
#                            "r-lib/pkgdown",
#                            "dmurdoch/rgl",
#                            "dmurdoch/htmlwidgets@rglpatch"))

## ----eval=FALSE---------------------------------------------------------------
#  setupKnitr(autoprint = TRUE)

## -----------------------------------------------------------------------------
# Show regression plane with z as dependent variable
library(rgl)
open3d()
x <- rnorm(100)
y <- rnorm(100)
z <- 0.2*x - 0.3*y + rnorm(100, sd = 0.3)
fit <- lm(z ~ x + y)

plot3d(x, y, z, type = "s", col = "red", size = 1)

# No plot here, because of the planes3d() call below

coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.5)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages(c("downlit", "pkgdown", "htmlwidgets", "rgl"))

