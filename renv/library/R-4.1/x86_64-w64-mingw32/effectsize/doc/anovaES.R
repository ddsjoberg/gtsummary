## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = "")
options(digits = 2)
knitr::opts_chunk$set(comment = ">", warning = FALSE)

set.seed(1)
pkgs <- c("effectsize", "parameters", "car", "afex")
if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
data(obk.long, package = "afex")
# modify the data slightly for the demonstration:
obk.long <- obk.long[1:240 %% 3 == 0, ]
obk.long$id <- seq_len(nrow(obk.long))

m <- lm(value ~ treatment, data = obk.long)

parameters::model_parameters(m)

## -----------------------------------------------------------------------------
parameters::model_parameters(anova(m))

## -----------------------------------------------------------------------------
library(effectsize)

eta_squared(m, partial = FALSE)

## -----------------------------------------------------------------------------
m <- lm(value ~ gender + phase + treatment, data = obk.long)

eta_squared(m, partial = FALSE)

eta_squared(m) # partial = TRUE by default

## -----------------------------------------------------------------------------
eta_squared(car::Anova(m, type = 2), partial = FALSE)

eta_squared(car::Anova(m, type = 3)) # partial = TRUE by default

## -----------------------------------------------------------------------------
# compare
m_interaction1 <- lm(value ~ treatment * gender, data = obk.long)

# to:
m_interaction2 <- lm(
  value ~ treatment * gender,
  data = obk.long,
  contrasts = list(
    treatment = "contr.sum",
    gender = "contr.sum"
  )
)

eta_squared(car::Anova(m_interaction1, type = 3))
eta_squared(car::Anova(m_interaction2, type = 3))

## -----------------------------------------------------------------------------
library(afex)
m_afex <- aov_car(value ~ treatment * gender + Error(id), data = obk.long)

eta_squared(m_afex)

## -----------------------------------------------------------------------------
omega_squared(m_afex)

epsilon_squared(m_afex)

## -----------------------------------------------------------------------------
eta_squared(m_afex, generalized = "gender")

## -----------------------------------------------------------------------------
cohens_f(m_afex)

## ---- eval=require(lmerTest)--------------------------------------------------
library(lmerTest)

fit_lmm <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova(fit_lmm) # note the type-3 errors

F_to_eta2(45.8, df = 1, df_error = 17)

## ---- eval=require(lmerTest)--------------------------------------------------
eta_squared(fit_lmm)
epsilon_squared(fit_lmm)
omega_squared(fit_lmm)

