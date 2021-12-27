## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(smd)

## ----numeric------------------------------------------------------------------
set.seed(123)
xn <- rnorm(90)
gg2 <- rep(LETTERS[1:2], each = 45)
gg3 <- rep(LETTERS[1:3], each = 30)

smd(x = xn, g = gg2)
smd(x = xn, g = gg3)
smd(x = xn, g = gg2, std.error = TRUE)
smd(x = xn, g = gg3, std.error = TRUE)

## ----integer------------------------------------------------------------------
xi <- sample(1:20, 90, replace = TRUE)
smd(x = xi, g = gg2)

## ----character----------------------------------------------------------------
xc <- unlist(replicate(2, sort(sample(letters[1:3], 45, replace = TRUE)), simplify = FALSE))
smd(x = xc, g = gg2)

## ----factor-------------------------------------------------------------------
xf <- factor(xc)
smd(x = xf, g = gg2)

## ----logical------------------------------------------------------------------
xl <- as.logical(rbinom(90, 1, prob = 0.5))
smd(x = xl, g = gg2)

## ----matrix-------------------------------------------------------------------
mm <- cbind(xl, xl, xl, xl)
smd(x = mm, g = gg3, std.error = FALSE)

## ----list---------------------------------------------------------------------
ll <- list(xn = xn, xi = xi, xf = xf, xl = xl)
smd(x = ll, g = gg3)

## ----data.frame---------------------------------------------------------------
df <- data.frame(xn, xi, xc, xf, xl)
smd(x = df, g = gg3)

## ----dplyr--------------------------------------------------------------------
library(dplyr, verbose = FALSE)
df$g <- gg2
df %>%
  summarize_at(
    .vars = vars(dplyr::matches("^x")),
    .funs = list(smd = ~ smd(., g = g)$estimate))

