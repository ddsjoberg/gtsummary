## ----setup, include=FALSE-----------------------------------------------------
library(knitr)

## ----eval=FALSE---------------------------------------------------------------
#  help(package = 'YourPackage', help_type = 'html')
#  # or see a standalone list of vignettes
#  browseVignettes('YourPackage')

## ----hello, results='asis'----------------------------------------------------
if (TRUE) cat('_hello_ **markdown**!', '\n')

## ----test, collapse=TRUE------------------------------------------------------
1+1
10:1
rnorm(5)^2
strsplit('hello, markdown vignettes', '')

## -----------------------------------------------------------------------------
n=300; set.seed(123)
par(mar=c(4,4,.1,.1))
plot(rnorm(n), rnorm(n), pch=21, cex=5*runif(n), col='white', bg='gray')

## ----css, eval=FALSE----------------------------------------------------------
#  options(markdown.HTML.stylesheet = 'path/to/a/custom/style.css')

