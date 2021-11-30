## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(comment = ">")
options(digits = 3)

pkgs <- c("effectsize", "BayesFactor")
if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}
set.seed(7)

## -----------------------------------------------------------------------------
library(effectsize)
library(BayesFactor)

## -----------------------------------------------------------------------------
t.test(mpg ~ am, data = mtcars, var.equal = TRUE)

cohens_d(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
hedges_g(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
t.test(mpg ~ am, data = mtcars, var.equal = FALSE)

cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)

hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE)

## -----------------------------------------------------------------------------
glass_delta(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
t.test(extra ~ group, data = sleep, paired = TRUE)

cohens_d(extra ~ group, data = sleep, paired = TRUE)

hedges_g(extra ~ group, data = sleep, paired = TRUE)

## -----------------------------------------------------------------------------
(BFt <- ttestBF(mtcars$mpg[mtcars$am == 0], mtcars$mpg[mtcars$am == 1]))

effectsize(BFt, test = NULL)

## ---- message=FALSE-----------------------------------------------------------
onew <- oneway.test(mpg ~ gear, data = mtcars, var.equal = TRUE)

eta_squared(onew)

## -----------------------------------------------------------------------------
(Music <- matrix(
  c(
    150, 130, 35, 55,
    100, 50, 10, 40,
    165, 65, 2, 25
  ),
  byrow = TRUE, nrow = 3,
  dimnames = list(
    Study = c("Psych", "Econ", "Law"),
    Music = c("Pop", "Rock", "Jazz", "Classic")
  )
))

chisq.test(Music)

cramers_v(Music)

phi(Music)

## -----------------------------------------------------------------------------
O <- c(89, 37, 30, 28, 2) # observed group sizes
E <- c(40, 20, 20, 15, 6) # expected group sizes

chisq.test(O, p = E, rescale.p = TRUE)

cramers_v(O, p = E, rescale.p = TRUE)

phi(O, p = E, rescale.p = TRUE)

## -----------------------------------------------------------------------------
(BFX <- contingencyTableBF(Music, sampleType = "jointMulti"))

effectsize(BFX, type = "cramers_v", test = NULL)

effectsize(BFX, type = "phi", test = NULL)

## -----------------------------------------------------------------------------
(RCT <- matrix(
  c(
    71, 30,
    50, 100
  ),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    Diagnosis = c("Sick", "Recovered"),
    Group = c("Treatment", "Control")
  )
))

chisq.test(RCT) # or fisher.test(RCT)

oddsratio(RCT)

## -----------------------------------------------------------------------------
riskratio(RCT)

## -----------------------------------------------------------------------------
cohens_h(RCT)

## -----------------------------------------------------------------------------
(Performance <- matrix(
  c(
    794, 86,
    150, 570
  ),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    "1st Survey" = c("Approve", "Disapprove"),
    "2nd Survey" = c("Approve", "Disapprove")
  )
))

mcnemar.test(Performance)

cohens_g(Performance)

## ---- warning=FALSE-----------------------------------------------------------
A <- c(48, 48, 77, 86, 85, 85)
B <- c(14, 34, 34, 77)

wilcox.test(A, B) # aka Mann–Whitney U test

rank_biserial(A, B)

## -----------------------------------------------------------------------------
x <- c(1.15, 0.88, 0.90, 0.74, 1.21)

wilcox.test(x, mu = 1) # aka Signed-Rank test

rank_biserial(x, mu = 1)


x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.test(x, y, paired = TRUE) # aka Signed-Rank test

rank_biserial(x, y, paired = TRUE)

## -----------------------------------------------------------------------------
group_data <- list(
  g1 = c(2.9, 3.0, 2.5, 2.6, 3.2), # normal subjects
  g2 = c(3.8, 2.7, 4.0, 2.4), # with obstructive airway disease
  g3 = c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
)

kruskal.test(group_data)

rank_epsilon_squared(group_data)

## -----------------------------------------------------------------------------
# Subjects are COLUMNS
(ReactionTimes <- matrix(
  c(398, 338, 520,
    325, 388, 555,
    393, 363, 561,
    367, 433, 470,
    286, 492, 536,
    362, 475, 496,
    253, 334, 610),
  nrow = 7, byrow = TRUE,
  dimnames = list(
    paste0("Subject", 1:7),
    c("Congruent", "Neutral", "Incongruent")
  )
))

friedman.test(ReactionTimes)

kendalls_w(ReactionTimes)

