## ----opts, echo = FALSE, message = FALSE--------------------------------------
library("knitr")
knitr::opts_chunk$set(
  eval = FALSE
)

## ----loadpkg,message=FALSE----------------------------------------------------
#  library("lme4")

## ----noderivs,eval=FALSE------------------------------------------------------
#  lmer(y ~ service * dept + (1|s) + (1|d), InstEval,
#       control = lmerControl(calc.derivs = FALSE))

## ----nlminbfit,eval=FALSE-----------------------------------------------------
#  library("optimx")
#  lmer(y ~ service * dept + (1|s) + (1|d), InstEval,
#       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

## ----nlopt,eval=FALSE---------------------------------------------------------
#  nlopt <- function(par, fn, lower, upper, control) {
#      .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper,
#          opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
#          maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
#      list(par = res$solution,
#           fval = res$objective,
#           conv = if (res$status > 0) 0 else res$status,
#           message = res$message
#      )
#  }
#  lmer(y ~ service * dept + (1|s) + (1|d), InstEval,
#      control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))

