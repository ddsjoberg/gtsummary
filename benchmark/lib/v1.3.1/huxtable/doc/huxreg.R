## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(huxtable)

is_latex <- guess_knitr_output_format() == "latex"
knitr::knit_hooks$set(
  barrier = function(before, options, envir) {
    if (! before && is_latex) knitr::asis_output("\\FloatBarrier")
  }
)

if (is_latex) knitr::opts_chunk$set(barrier = TRUE)

## -----------------------------------------------------------------------------
data(diamonds, package = "ggplot2")
diamonds <- diamonds[1:100,]

lm1 <- lm(price ~ carat + depth, diamonds)
lm2 <- lm(price ~ depth + factor(color, ordered = FALSE), diamonds)
lm3 <- lm(log(price) ~ carat + depth, diamonds)

## -----------------------------------------------------------------------------

huxreg(lm1, lm2, lm3)

## -----------------------------------------------------------------------------
color_names <- grep("factor", names(coef(lm2)), value = TRUE)
names(color_names) <- gsub(".*)(.)", "Color: \\1", color_names)

huxreg(lm1, lm2, lm3, coefs = c("Carat" = "carat", "Depth" = "depth", color_names))

## -----------------------------------------------------------------------------
diamond_regs <- huxreg(lm1, lm2, lm3)
diamond_regs[seq(8, 18, 2), 1] <- paste("Color:", LETTERS[5:10])

# prints the same as above


## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))

diamond_regs %>% 
      map_background_color(-1, -1, by_regex(
        "\\*" = "yellow"
      )) %>% 
      set_italic(final(1), 1) %>% 
      set_caption("Linear regressions of diamond prices")


## -----------------------------------------------------------------------------
huxreg(lm1, lm3, error_pos = "right")

## -----------------------------------------------------------------------------
huxreg(lm1, lm3, error_pos = "same")

## -----------------------------------------------------------------------------
huxreg("Price" = lm1, "Log price" = lm3)

## -----------------------------------------------------------------------------
gl <- as_hux(broom::glance(lm1))

gl %>% 
      restack_down(cols = 3, on_remainder = "fill") %>% 
      set_bold(odds, everywhere)

## -----------------------------------------------------------------------------
huxreg(lm1, lm3, statistics = c("N. obs." = "nobs", 
      "R squared" = "r.squared", "F statistic" = "statistic",
      "P value" = "p.value"))

## -----------------------------------------------------------------------------
huxreg(lm1, lm3, stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01)) # a little boastful?

## -----------------------------------------------------------------------------
# Another useful column: p.value
huxreg(
        lm1, lm3, 
        error_format = "[{statistic}]", 
        note         = "{stars}. T statistics in brackets."
      )


## -----------------------------------------------------------------------------
huxreg(lm1, lm3, ci_level = .99, error_format = "({conf.low} -- {conf.high})")

## -----------------------------------------------------------------------------
huxreg(lm1, lm3, number_format = 2)

## -----------------------------------------------------------------------------
huxreg(lm1, lm3, bold_signif = 0.05)

## -----------------------------------------------------------------------------
library(lmtest)
library(sandwich)
lm_robust <- coeftest(lm1, vcov = vcovHC)
huxreg("Normal SEs" = lm1, "Robust SEs" = lm_robust)

## -----------------------------------------------------------------------------
lm_fixed <- tidy_override(lm1, p.value = c(0.5, 0.2, 0.06))
huxreg("Normal p values" = lm1, "Supplied p values" = lm_fixed)

## -----------------------------------------------------------------------------
mnl <- nnet::multinom(gear ~ mpg, mtcars)
tidied <- broom::tidy(mnl)
models <- list()
models[["4 gears"]] <- tidy_replace(mnl, tidied[tidied$y.level == 4, ])
models[["5 gears"]] <- tidy_replace(mnl, tidied[tidied$y.level == 5, ])
huxreg(models, statistics = "AIC")

