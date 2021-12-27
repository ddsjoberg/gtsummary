## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
knitr::opts_chunk$set(comment = ">",
                      warning = FALSE,
                      message = FALSE)
options(digits = 2)
options(knitr.kable.NA = '')

pkgs <- c("effectsize", "parameters", "correlation")
if (!all(sapply(pkgs, requireNamespace, quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(333)

## -----------------------------------------------------------------------------
library(effectsize)

m <- lm(rating ~ complaints, data = attitude)

standardize_parameters(m)

## -----------------------------------------------------------------------------
r <- cor.test(attitude$rating, attitude$complaints)

effectsize(r)

## ----include=FALSE------------------------------------------------------------
mtcars <- datasets::mtcars

## -----------------------------------------------------------------------------
# Select portion of data containing the two levels of interest
mtcars$am <- factor(mtcars$am, labels = c("Manual", "Automatic"))

m <- lm(mpg ~ am, data = mtcars)
standardize_parameters(m)

## -----------------------------------------------------------------------------
cohens_d(mpg ~ am, data = mtcars) 

## -----------------------------------------------------------------------------
coef(m)[2] / sigma(m)

## -----------------------------------------------------------------------------
parameters::model_parameters(m)

t_to_d(4.11, df_error = 30)

## -----------------------------------------------------------------------------
m <- lm(mpg ~ am, data = mtcars)

standardize_parameters(m, method = "smart")

glass_delta(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
data("hardlyworking", package = "effectsize")

head(hardlyworking)

correlation::correlation(data = hardlyworking[,1], # the outcome of salary
                         data2 = hardlyworking[,-1], # the predictors
                         partial = TRUE) # get partial correlations

## -----------------------------------------------------------------------------
mod <- lm(salary ~ xtra_hours + n_comps + age + seniority,
          data = hardlyworking)

standardize_parameters(mod)

## -----------------------------------------------------------------------------
params <- parameters::model_parameters(mod)

t_to_r(params$t[-1], df_error = params$df_error[-1])

## -----------------------------------------------------------------------------
hardlyworking$age_g <- cut(hardlyworking$age,
                           breaks = c(25,30,35,45))

mod <- lm(salary ~ xtra_hours + n_comps + age_g + seniority,
          data = hardlyworking)

parameters::model_parameters(mod)

## -----------------------------------------------------------------------------
standardize_parameters(mod, method = "refit")

## -----------------------------------------------------------------------------
standardize_parameters(mod, method = "refit", robust = TRUE)

## -----------------------------------------------------------------------------
standardize_parameters(mod, method = "refit", two_sd = TRUE)

## -----------------------------------------------------------------------------
mod_z <- standardize(mod, two_sd = FALSE, robust = FALSE)
mod_z

parameters::model_parameters(mod_z)

## -----------------------------------------------------------------------------
standardize_parameters(mod, method = "posthoc")

## -----------------------------------------------------------------------------
standardize_parameters(mod, method = "smart")

## -----------------------------------------------------------------------------
standardize_parameters(mod, method = "basic")

## ---- eval=knitr::opts_chunk$get("eval") && require(lme4) && require(lmerTest), warning=FALSE----
m <- lme4::lmer(mpg ~ cyl + am + vs + (1|cyl), mtcars)

standardize_parameters(m, method = "pseudo", df_method = "satterthwaite")

# compare to:
standardize_parameters(m, method = "basic", df_method = "satterthwaite")

## -----------------------------------------------------------------------------
mod_b <- glm(am ~ mpg + factor(cyl),
             data = mtcars,
             family = binomial())

standardize_parameters(mod_b, method = "refit", two_sd = TRUE)
# standardize_parameters(mod_b, method = "posthoc", two_sd = TRUE)
# standardize_parameters(mod_b, method = "basic")

## -----------------------------------------------------------------------------
std <- standardize_parameters(mod_b, method = "refit", two_sd = TRUE)
exp(std$Std_Coefficient)

## -----------------------------------------------------------------------------
standardize_parameters(mod_b, method = "refit", two_sd = TRUE, exponentiate = TRUE)

## -----------------------------------------------------------------------------
m1 <- lm(salary ~ xtra_hours, data = hardlyworking)
m2 <- lm(salary ~ xtra_hours + n_comps + seniority, data = hardlyworking)

cohens_f_squared(m1, model2 = m2)

