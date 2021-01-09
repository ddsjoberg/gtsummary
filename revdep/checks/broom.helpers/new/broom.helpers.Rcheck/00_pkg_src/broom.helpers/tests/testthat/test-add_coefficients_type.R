library(survival)
library(gtsummary)

test_that("tidy_add_coefficients_type() works for common models", {
  mod <- lm(Sepal.Length ~ Sepal.Width, iris)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "generic")
  expect_equivalent(attr(res, "coefficients_label"), "Beta")

  mod <- glm(Sepal.Length ~ Sepal.Width, iris, family = gaussian)
  res <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "generic")
  expect_equivalent(attr(res, "coefficients_label"), "exp(Beta)")

  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "logistic")
  expect_equivalent(attr(res, "coefficients_label"), "log(OR)")
  res <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "logistic")
  expect_equivalent(attr(res, "coefficients_label"), "OR")

  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = binomial(probit))
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "generic")
  expect_equivalent(attr(res, "coefficients_label"), "Beta")
  res <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "generic")
  expect_equivalent(attr(res, "coefficients_label"), "exp(Beta)")

  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "poisson")
  expect_equivalent(attr(res, "coefficients_label"), "log(IRR)")
  res <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "poisson")
  expect_equivalent(attr(res, "coefficients_label"), "IRR")

  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson("identity"))
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type()
  expect_equivalent(attr(res, "coefficients_type"), "generic")
  expect_equivalent(attr(res, "coefficients_label"), "Beta")

  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = quasipoisson)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type(exponentiate = TRUE)
  expect_equivalent(attr(res, "coefficients_type"), "poisson")
  expect_equivalent(attr(res, "coefficients_label"), "IRR")

  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = quasibinomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_coefficients_type(exponentiate = TRUE)
  expect_equivalent(attr(res, "coefficients_type"), "logistic")
  expect_equivalent(attr(res, "coefficients_label"), "OR")
})

test_that("test tidy_add_coefficients_type() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_add_coefficients_type(exponentiate = TRUE))

  # expect an error if no value for exponentiate
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_coefficients_type(exponentiate = NULL))
  expect_error(mod %>% broom::tidy() %>% tidy_attach_model(mod) %>% tidy_add_coefficients_type())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_add_coefficients_type() %>% tidy_add_coefficients_type(),
    NA
  )
})

test_that("model_get_coefficients_type() works with lme4::lmer", {
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "generic")
})


test_that("model_identify_variables() works with lme4::glmer", {
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial, data = lme4::cbpp
  )
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")

  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     family = binomial("probit"), data = lme4::cbpp
  )
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "generic")

  mod <- lme4::glmer(response ~ trt + (1 | grade), gtsummary::trial, family = poisson)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "poisson")
})


test_that("model_get_coefficients_type() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "prop_hazard")
})

test_that("model_get_coefficients_type() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian,
    dist = "exponential"
  )
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "generic")
})


test_that("model_get_coefficients_type() works with survival::clogit", {
  resp <- levels(survival::logan$occupation)
  n <- nrow(survival::logan)
  indx <- rep(1:n, length(resp))
  logan2 <- data.frame(survival::logan[indx,],
                       id = indx,
                       tocc = factor(rep(resp, each=n)))
  logan2$case <- (logan2$occupation == logan2$tocc)
  mod <- survival::clogit(case ~ tocc + tocc:education + strata(id), logan2)

  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")
})


test_that("model_get_coefficients_type() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")
})

test_that("model_get_coefficients_type() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")
})


test_that("model_get_coefficients_type() works with survey::svycoxph", {
  dpbc <- survey::svydesign(id = ~ 1, prob = ~ 1, strata = ~ edema, data = survival::pbc)
  mod <- survey::svycoxph(Surv(time, status>0) ~ log(bili) + protime + albumin, design = dpbc)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "prop_hazard")
})

test_that("tidy_plus_plus() works with survey::svyolr", {
  data(api, package = "survey")
  fpc <- survey::svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
  fpc <- update(fpc, mealcat=cut(meals,c(0,25,50,75,100)))
  mod <- survey::svyolr(mealcat~avg.ed+mobility+stype, design = fpc)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")
})

test_that("model_get_coefficients_type() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~contact)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")
})


test_that("model_get_coefficients_type() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")
})


test_that("model_get_coefficients_type() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")

  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing, method = "probit")
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "generic")
})


test_that("model_get_coefficients_type() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("log"), corstr = "ar1")
  )

  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "poisson")
})


test_that("model_get_coefficients_type() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "logistic")

  mod <- suppressWarnings(gam::gam(
    Ozone^(1 / 3) ~ gam::lo(Solar.R) + gam::lo(Wind, Temp),
    data = datasets::airquality, na = gam::na.gam.replace
  ))
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "generic")
})


test_that("model_get_coefficients_type() works with lavaan::lavaan", {
  df <- lavaan::HolzingerSwineford1939
  df$grade <- factor(df$grade, ordered = TRUE)
  HS.model <- "visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6 + grade
               speed   =~ x7 + x8 + x9 "
  mod <- lavaan::lavaan(HS.model,
    data = df,
    auto.var = TRUE, auto.fix.first = TRUE,
    auto.cov.lv.x = TRUE
  )
  res <- mod %>% model_get_coefficients_type()
  expect_equivalent(res, "generic")
})
