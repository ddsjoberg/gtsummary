## ----hello, results='asis'----------------------------------------------------
cat('_hello_ **markdown**!', '\n')

## -----------------------------------------------------------------------------
1+1
10:1
rnorm(5)^2
strsplit('hello, markdown vignettes', '')

## -----------------------------------------------------------------------------
n=300; set.seed(123)
par(mar=c(4,4,.1,.1))
plot(rnorm(n), rnorm(n), pch=21, cex=5*runif(n), col='white', bg='gray')

## -----------------------------------------------------------------------------
head(knitr::rocco, 5)

## ----setup, echo=FALSE, results='asis'----------------------------------------
x = readLines('docco-linear.Rmd')[-(1:7)]
x = gsub('linear', 'classic', x)
i = grep('^knit2html[(][.]{3}', x)
x[i - 1] = '```{r}'
x[i] = 'head(knitr::rocco, 5)'
library(knitr)
cat(knit_child(text = x, quiet = TRUE), sep = '\n')

