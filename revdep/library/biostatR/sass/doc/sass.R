## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  fig.align = "center",
  out.width = "80%",
  class.output = "css",
  comment = ""
)

## -----------------------------------------------------------------------------
library(sass)
variable <- "$body-bg: red;"
rule <- "body { background-color: $body-bg; }"
sass(input = list(variable, rule))

## -----------------------------------------------------------------------------
user_default <- list("body-bg" = "blue !default")
default <- list("body-bg" = "red !default")
sass(input = list(user_default, default, rule))

## -----------------------------------------------------------------------------
variable <- list("body-bg" = "rgba(black, 0.8)")
sass(input = list(variable, rule))

## -----------------------------------------------------------------------------
sass(
  list(
    variable,
    sass_file("color-contrast.scss"),
    "body { 
      background-color: $body-bg;
      color: color-contrast($body-bg); 
    }"
  )
)

## ---- eval=FALSE, ref.label='bs_sass'-----------------------------------------
#  NA

## ---- echo = FALSE, out.width='50%'-------------------------------------------
knitr::include_graphics('my-style.png')

## -----------------------------------------------------------------------------
layer1 <- sass_layer(
  defaults = list("body-bg" = "white !default"),
  declarations = sass_file("color-contrast.scss"),
  rules = "body{background-color: $body-bg; color: color-contrast($body-bg)}"
)
sass(layer1)

## -----------------------------------------------------------------------------
layer2 <- sass_layer(
  defaults = list("body-bg" = "white !default")
)
sass(sass_layer_merge(layer1, layer2))

## -----------------------------------------------------------------------------
sass(
  sass_file("my-style.scss"),
  options = sass_options(output_style = "compressed")
)

## ---- echo = FALSE------------------------------------------------------------
knitr::include_graphics("https://i.imgur.com/5cUEifg.gif")

