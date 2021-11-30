## ---- include=FALSE-----------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  out.width = "100%"
)

pkgs <- c(
  "datawizard",
  "dplyr",
  "tidyr",
  "see",
  "ggplot2",
  "parameters",
  "performance",
  "lme4",
  "lfe"
)

if (!all(sapply(pkgs, requireNamespace, quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

if (!packageVersion("parameters") >= "0.14.1") {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(333)

## -----------------------------------------------------------------------------
#  library(parameters)
#  data("qol_cancer")

## -----------------------------------------------------------------------------
#  check_heterogeneity_bias(qol_cancer, select = c("phq4", "education"), group = "ID")

## -----------------------------------------------------------------------------
#  qol_cancer <- cbind(
#    qol_cancer,
#    datawizard::demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
#  )

## -----------------------------------------------------------------------------
#  fe_model1 <- lm(
#    QoL ~ 0 + time + phq4_within + ID,
#    data = qol_cancer
#  )
#  # we use only the first two rows, because the remaining rows are
#  # the estimates for "ID", which is not of interest here...
#  model_parameters(fe_model1)[1:2, ]
#  
#  
#  # instead of removing the intercept, we could also use the
#  # de-meaned response...
#  fe_model2 <- lm(
#    QoL_within ~ time + phq4_within + ID,
#    data = qol_cancer
#  )
#  model_parameters(fe_model2)[2:3, ]
#  
#  # we compare the results with those from the "lfe"-package for panel data
#  library(lfe)
#  fe_model3 <- felm(
#    QoL ~ time + phq4 | ID,
#     data = qol_cancer
#  )
#  model_parameters(fe_model3)

## -----------------------------------------------------------------------------
#  library(lme4)
#  mixed_1 <- lmer(
#    QoL ~ time + phq4_within + phq4_between + (1 | ID),
#    data = qol_cancer
#  )
#  model_parameters(mixed_1)
#  
#  # compare to FE-model
#  model_parameters(fe_model1)[1:2, ]

## -----------------------------------------------------------------------------
#  mixed_2 <- lmer(
#    QoL ~ time + phq4_within + phq4_between + education + (1 + time | ID),
#    data = qol_cancer
#  )
#  # effects = "fixed" will not display random effects, but split the
#  # fixed effects into its between- and within-effects components.
#  model_parameters(mixed_2, effects = "fixed")

## ----echo=FALSE---------------------------------------------------------------
#  f <- "y<sub>it</sub> = &beta;<sub>0</sub> + &beta;<sub>1W</sub> (x<sub>it</sub> - &#x035E;x<sub>i</sub>) + &beta;<sub>2B</sub> &#x035E;x<sub>i</sub> + &beta;<sub>3</sub> z<sub>i</sub> + &upsilon;<sub>i0</sub> + &upsilon;<sub>i1</sub> (x<sub>it</sub> - &#x035E;x<sub>i</sub>) + &epsilon;<sub>it</sub>"
#  knitr::asis_output(f)

## ----echo=FALSE---------------------------------------------------------------
#  f <- "<ul><li>x<sub>it</sub> - &#x035E;x<sub>i</sub> is the de-meaned predictor, <em>phq4_within</em></li><li>&#x035E;x<sub>i</sub> is the group-meaned predictor, <em>phq4_between</em></li><li>&beta;<sub>1W</sub> is the coefficient for phq4_within (within-subject)</li><li>&beta;<sub>2B</sub> is the coefficient for phq4_between (bewteen-subject)</li><li>&beta;<sub>3</sub> is the coefficient for time-constant predictors, such as `hospital` or `education` (bewteen-subject)</li></ul>"
#  knitr::asis_output(f)

## -----------------------------------------------------------------------------
#  rewb <- lmer(
#    QoL ~ time + phq4_within + phq4_between + education +
#      (1 + time | ID) + (1 + phq4_within | ID),
#    data = qol_cancer
#  )

## -----------------------------------------------------------------------------
#  model_parameters(rewb, effects = "fixed")

## -----------------------------------------------------------------------------
#  random_parameters(rewb)

## -----------------------------------------------------------------------------
#  library(ggplot2)
#  library(dplyr)
#  library(see)
#  
#  set.seed(123)
#  n <- 5
#  b <- seq(1, 1.5, length.out = 5)
#  x <- seq(2, 2 * n, 2)
#  
#  d <- do.call(rbind, lapply(1:n, function(i) {
#    data.frame(
#      x = seq(1, n, by = .2),
#      y = 2 * x[i] + b[i] * seq(1, n, by = .2) + rnorm(21),
#      grp = as.factor(2 * i)
#    )
#  }))
#  
#  d <- d %>%
#    group_by(grp) %>%
#    mutate(x = rev(15 - (x + 1.5 * as.numeric(grp)))) %>%
#    ungroup()
#  
#  labs <- c("very slow", "slow", "average", "fast", "very fast")
#  levels(d$grp) <- rev(labs)
#  
#  d <- cbind(d, datawizard::demean(d, c("x", "y"), group = "grp"))

## ----echo=FALSE---------------------------------------------------------------
#  ggplot(d, aes(x, y)) +
#    geom_point(colour = "#555555", size = 2.5, alpha = .5) +
#    see::theme_modern() +
#    labs(x = "Typing Speed", y = "Typing Errors", colour = "Type Experience")

## ----echo=FALSE---------------------------------------------------------------
#  ggplot(d, aes(x, y)) +
#    geom_point(colour = "#555555", size = 2.5, alpha = .5) +
#    geom_smooth(method = "lm", se = F, colour = "#555555") +
#    see::theme_modern() +
#    labs(x = "Typing Speed", y = "Typing Errors", colour = "Type Experience")

## -----------------------------------------------------------------------------
#  m1 <- lm(y ~ x, data = d)
#  model_parameters(m1)

## ----echo=FALSE---------------------------------------------------------------
#  ggplot(d, aes(x, y)) +
#    geom_point(mapping = aes(colour = grp), size = 2.5, alpha = .5) +
#    geom_smooth(method = "lm", se = F, colour = "#555555") +
#    see::scale_color_flat() +
#    see::theme_modern() +
#    labs(x = "Typing Speed", y = "Typing Errors", colour = "Type Experience")

## ----echo=FALSE---------------------------------------------------------------
#  ggplot(d, aes(x, y)) +
#    geom_smooth(mapping = aes(colour = grp), method = "lm", se = FALSE) +
#    geom_point(mapping = aes(colour = grp), size = 2.2, alpha = .6) +
#    see::scale_color_flat() +
#    see::theme_modern() +
#    labs(x = "Typing Speed", y = "Typing Errors", colour = "Type Experience")

## -----------------------------------------------------------------------------
#  m2 <- lm(y ~ 0 + x_within + grp, data = d)
#  model_parameters(m2)[1, ]

## ----echo=FALSE---------------------------------------------------------------
#  ggplot(d, aes(x, y)) +
#    geom_point(mapping = aes(colour = grp), size = 2.2, alpha = .6) +
#    geom_smooth(mapping = aes(x = x_between, y = y_between), method = "lm", se = F, colour = "#444444") +
#    see::scale_color_flat() +
#    see::theme_modern() +
#    labs(x = "Typing Speed", y = "Typing Errors", colour = "Type Experience")

## -----------------------------------------------------------------------------
#  m3 <- lm(y ~ x_between, data = d)
#  model_parameters(m3)

## ----echo=FALSE---------------------------------------------------------------
#  ggplot(d, aes(x, y)) +
#    geom_smooth(mapping = aes(colour = grp), method = "lm", se = FALSE) +
#    geom_point(mapping = aes(colour = grp), size = 2.2, alpha = .6) +
#    geom_smooth(mapping = aes(x = x_between, y = y_between), method = "lm", se = F, colour = "#444444") +
#    see::scale_color_flat() +
#    see::theme_modern() +
#    labs(x = "Typing Speed", y = "Typing Errors", colour = "Type Experience")

## -----------------------------------------------------------------------------
#  m4 <- lmer(y ~ x_between + x_within + (1 | grp), data = d)
#  model_parameters(m4)

## -----------------------------------------------------------------------------
#  m5 <- lmer(y ~ x_between + x_within + (1 + x_within | grp), data = d)
#  model_parameters(m5)

## -----------------------------------------------------------------------------
#  set.seed(123)
#  n <- 5
#  b <- seq(1, 1.5, length.out = 5)
#  x <- seq(2, 2 * n, 2)
#  
#  d <- do.call(rbind, lapply(1:n, function(i) {
#    data.frame(
#      x = seq(1, n, by = .2),
#      y = 2 * x[i] + b[i] * seq(1, n, by = .2) + rnorm(21),
#      grp = as.factor(2 * i)
#    )
#  }))
#  
#  # create imbalanced groups
#  d$grp[sample(which(d$grp == 8), 10)] <- 6
#  d$grp[sample(which(d$grp == 4), 8)] <- 2
#  d$grp[sample(which(d$grp == 10), 9)] <- 6
#  
#  d <- d %>%
#    group_by(grp) %>%
#    mutate(x = rev(15 - (x + 1.5 * as.numeric(grp)))) %>%
#    ungroup()
#  
#  labs <- c("very slow", "slow", "average", "fast", "very fast")
#  levels(d$grp) <- rev(labs)
#  
#  d <- cbind(d, datawizard::demean(d, c("x", "y"), group = "grp"))
#  
#  # Between-subject effect of typing speed
#  m1 <- lm(y ~ x_between, data = d)
#  model_parameters(m1)
#  
#  # Between-subject effect of typing speed, accounting for group structure
#  m2 <- lmer(y ~ x_between + (1 | grp), data = d)
#  model_parameters(m2)

