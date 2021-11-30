## ----numbers-1, include = FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  cache = TRUE,
  comment = "#>"
)

## ----numbers-2----------------------------------------------------------------
library(pillar)
library(tibble)

## ----numbers-5----------------------------------------------------------------
num(-1:3, notation = "sci")

tibble(
  x4 = num(8:12 * 100 + 0.5, digits = 4),
  x1 = num(8:12 * 100 + 0.5, digits = -1),
  usd = num(8:12 * 100 + 0.5, digits = 2, label = "USD"),
  percent = num(8:12 / 100 + 0.0005, label = "%", scale = 100),
  eng = num(10^(-3:1), notation = "eng", fixed_exponent = -Inf),
  si = num(10^(-3:1) * 123, notation = "si")
)

## ----numbers-13---------------------------------------------------------------
num(1) + 2
1 + num(2)
1L + num(2)
num(3.23456, sigfig = 4) - num(2)
num(4, sigfig = 2) * num(3, digits = 2)
num(3, digits = 2) * num(4, sigfig = 2)
-num(2)

## ----numbers-15---------------------------------------------------------------
min(num(1:3, label = "$"))
mean(num(1:3, notation = "eng"))
sin(num(1:3, label = "%", scale = 100))

## ----numbers-16---------------------------------------------------------------
x <- num(c(1, 2, 4), notation = "eng")
var(x)

## ----numbers-16a--------------------------------------------------------------
num(var(x), notation = "eng")

## ----numbers-16b--------------------------------------------------------------
var_ <- function(x, ...) {
  out <- var(x, ...)
  vctrs::vec_restore(out, x)
}
var_(x)

## ----numbers-16c--------------------------------------------------------------
make_restore <- function(fun) {
  force(fun)
  function(x, ...) {
    out <- fun(x, ...)
    vctrs::vec_restore(out, x)
  }
}

var_ <- make_restore(var)
sd_ <- make_restore(sd)

var_(x)
sd_(x)

