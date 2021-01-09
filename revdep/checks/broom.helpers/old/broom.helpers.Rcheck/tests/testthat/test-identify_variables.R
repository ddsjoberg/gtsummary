library(survival)
library(gtsummary)

test_that("model_list_variables() tests", {
  mod <- glm(response ~ age + grade * trt + death, gtsummary::trial, family = binomial)
  res <- mod %>% model_list_variables()
  expect_equivalent(
    res$variable,
    c("response", "age", "grade", "trt", "death", "grade:trt")
  )
  expect_equivalent(
    res$variable,
    mod %>% model_list_variables(only_variable = TRUE)
  )
  expect_equivalent(
    res$var_class,
    c(response = "integer", age = "numeric", grade = "factor", trt = "character",
      death = "integer", NA)
  )

  mod <- lm(marker ~ as.logical(response), gtsummary::trial)
  res <- mod %>%
    model_list_variables(
      labels = list(marker = "MARKER", "as.logical(response)" = "RESPONSE"))
  expect_equivalent(
    res$var_class,
    c("numeric", "logical")
  )
  expect_equivalent(
    res$var_label,
    c("MARKER", "RESPONSE")
  )
})

test_that("tidy_identify_variables() works for common models", {
  mod <- glm(response ~ age + grade * trt + death, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("(Intercept)", "age", "grade", "grade", "trt", "death", "grade:trt", "grade:trt")
  )
  expect_equivalent(
    res$var_class,
    c(NA, "numeric", "factor", "factor", "character", "integer", NA, NA)
  )
  expect_equivalent(
    res$var_type,
    c(
      "intercept", "continuous", "categorical", "categorical", "dichotomous",
      "continuous", "interaction", "interaction"
    )
  )
  expect_equivalent(
    res$var_nlevels,
    c(NA, NA, 3L, 3L, 2L, NA, NA, NA)
  )
})

test_that("test tidy_identify_variables() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_identify_variables())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_identify_variables() %>% tidy_identify_variables(),
    NA
  )
  res <- mod %>% tidy_and_attach() %>% tidy_identify_variables() %>% tidy_identify_variables()
  expect_true(
    all(c("variable", "var_type", "var_class", "var_nlevels")
        %in% names(res))
  )

  # cannot be applied after tidy_add_header_rows
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_add_header_rows() %>% tidy_identify_variables()
  )
})

test_that("model_identify_variables() works with different contrasts", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.SAS)
  )
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      NA, "stage", "stage", "stage", "grade", "grade", "trt", "grade:trt",
      "grade:trt"
    )
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)

  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.helmert, trt = contr.sum)
  )
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "stage", "stage", "stage", "grade", "grade", "trt", "grade:trt", "grade:trt")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})


test_that("model_identify_variables() works with stats::poly()", {
  mod <- lm(Sepal.Length ~ poly(Sepal.Width, 3) + poly(Petal.Length, 2), iris)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      NA, "Sepal.Width", "Sepal.Width", "Sepal.Width",
      "Petal.Length", "Petal.Length"
    )
  )
  expect_error(tb <- mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
  expect_equivalent(
    tb$variable,
    c(
      "(Intercept)", "Sepal.Width", "Sepal.Width", "Sepal.Width", "Petal.Length",
      "Petal.Length"
    )
  )
})


test_that("tidy_identify_variables() works with variables having non standard name", {
  # cf. https://github.com/ddsjoberg/gtsummary/issues/609
  df <- gtsummary::trial %>% dplyr::mutate(`grade of kids` = grade)
  mod <- lm(age ~ marker * `grade of kids`, df)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      "(Intercept)", "marker", "grade of kids", "grade of kids", "marker:grade of kids",
      "marker:grade of kids"
    )
  )
  expect_equivalent(
    res$var_class,
    c(NA, "numeric", "factor", "factor", NA, NA)
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)

  # interaction only term
  mod <- lm(age ~ marker : `grade of kids`, df)
  expect_equivalent(
    mod %>% model_list_variables(only_variable = TRUE),
    c("age", "marker", "grade of kids", "marker:grade of kids")
  )
  expect_equivalent(
    mod %>% model_identify_variables() %>% purrr::pluck("variable"),
    c(NA, "marker:grade of kids", "marker:grade of kids", "marker:grade of kids")
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("(Intercept)", "marker:grade of kids", "marker:grade of kids", "marker:grade of kids")
  )


  trial2 <-
    gtsummary::trial %>%
    dplyr::mutate(
      `treatment +name` = trt,
      `disease stage` = stage
    )
  mod <- glm(
    response ~ `treatment +name` + `disease stage`,
    trial2,
    family = binomial(link = "logit")
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables() %>%
    tidy_remove_intercept()
  expect_equivalent(
    res$variable,
    c("treatment +name", "disease stage",
      "disease stage", "disease stage")
  )
  expect_equivalent(
    res$var_type,
    c("dichotomous", "categorical", "categorical", "categorical")
  )

  mod <- lm(
    hp ~ factor(`number + cylinders`) : `miles :: galon` + factor(`type of transmission`),
    mtcars %>% dplyr::rename(
      `miles :: galon` = mpg, `type of transmission` = am,
      `number + cylinders` = cyl
    )
  )
  res <- tidy_plus_plus(mod)
  expect_equivalent(
    res$variable,
    c("factor(`type of transmission`)",
      "factor(`type of transmission`)",
      "factor(`number + cylinders`):miles :: galon",
      "factor(`number + cylinders`):miles :: galon",
      "factor(`number + cylinders`):miles :: galon")
  )
})

test_that("model_identify_variables() works with lme4::lmer", {
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "Days")
  )
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_identify_variables(), NA)
})


test_that("model_identify_variables() works with lme4::glmer", {
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial, data = lme4::cbpp
  )
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "period", "period", "period")
  )
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_identify_variables(), NA)
})


test_that("model_identify_variables() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c("ph.ecog", "age", "sex")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})

test_that("model_identify_variables() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian,
    dist = "exponential"
  )
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "ecog.ps", "rx")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})

test_that("model_identify_variables() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "stage", "stage", "stage", "marker", "age")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)

  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      "(Intercept)", "stage", "stage", "stage", "marker", "age", "(Intercept)", "stage",
      "stage", "stage", "marker", "age"
    )
  )

  # should work also with sum/SAS contrasts
  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.sum)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      "(Intercept)", "stage", "stage", "stage", "marker", "age", "(Intercept)", "stage",
      "stage", "stage", "marker", "age"
    )
  )

  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.SAS)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      "(Intercept)", "stage", "stage", "stage", "marker", "age", "(Intercept)", "stage",
      "stage", "stage", "marker", "age"
    )
  )

  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.helmert)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c(
      "(Intercept)", "stage", "stage", "stage", "marker", "age",
      "(Intercept)", "stage", "stage", "stage", "marker", "age"
    )
  )
})

test_that("model_identify_variables() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "age", "grade", "grade", "trt", "grade:trt", "grade:trt")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})

test_that("model_identify_variables() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  res <- mod %>% tidy_and_attach() %>% tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("1|2", "2|3", "3|4", "4|5", "temp", "contact", "temp:contact")
  )

  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, threshold = "symmetric")
  res <- mod %>% tidy_and_attach() %>% tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("central.1", "central.2", "spacing.1", "temp", "contact", "temp:contact")
  )

  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, threshold = "symmetric2")
  res <- mod %>% tidy_and_attach() %>% tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("spacing.1", "spacing.2", "temp", "contact", "temp:contact")
  )

  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, threshold = "equidistant")
  res <- mod %>% tidy_and_attach() %>% tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("threshold.1", "spacing", "temp", "contact", "temp:contact")
  )

  # wait for https://github.com/runehaubo/ordinal/issues/37
  # before testing nominal predictors

  # mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~contact)
  # res <- mod %>% tidy_and_attach() %>% tidy_identify_variables()
  # expect_equivalent(
  #   res$variable,
  #   c("1|2.(Intercept)", "2|3.(Intercept)", "3|4.(Intercept)", "4|5.(Intercept)",
  #     "contact", "contact", "contact", "contact", "temp", "contactyes",
  #     "temp:contact")
  # )


})


test_that("model_identify_variables() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  res <- mod %>% tidy_and_attach() %>% tidy_identify_variables()
  expect_equivalent(
    res$variable,
    c("1|2", "2|3", "3|4", "4|5", "temp", "contact", "temp:contact")
  )
})


test_that("model_identify_variables() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "Infl", "Infl", "Type", "Type", "Type", "Cont")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})


test_that("model_identify_variables() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )

  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "Cu", "Cu", "Time", "Cu:Time", "Cu:Time")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})


test_that("model_identify_variables() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "gam::s(Age, 4)", "Number")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)

  mod <- suppressWarnings(gam::gam(
    Ozone^(1 / 3) ~ gam::lo(Solar.R) + gam::lo(Wind, Temp),
    data = datasets::airquality, na = gam::na.gam.replace
  ))
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    c(NA, "gam::lo(Solar.R)", "gam::lo(Wind, Temp)", "gam::lo(Wind, Temp)")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})


test_that("model_identify_variables() works with lavaan::lavaan", {
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
  res <- mod %>% model_identify_variables()
  expect_equivalent(
    res$variable,
    mod@ParTable$lhs
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_identify_variables(), NA)
})

test_that("model_identify_variables() strict argument", {
  df_models <-
    tibble::tibble(grade = c("I", "II", "III")) %>%
    dplyr::mutate(df_model = purrr::map(grade, ~trial %>% dplyr::filter(grade == ..1))) %>%
    dplyr::mutate(
      mv_formula_char = "Surv(ttdeath, death) ~ trt + age + marker",
      mv_formula = purrr::map(mv_formula_char, as.formula),
      mv_model_form =
        purrr::map2(
          mv_formula, df_model,
          ~ coxph(..1, data = ..2)
        )
    )
  expect_error(
    df_models %>%
      dplyr::mutate(
        mv_tbl_form =
          purrr::map(
            mv_model_form,
            ~tidy_and_attach(.x) %>% tidy_identify_variables(strict = TRUE)
          )
      )
  )
})

