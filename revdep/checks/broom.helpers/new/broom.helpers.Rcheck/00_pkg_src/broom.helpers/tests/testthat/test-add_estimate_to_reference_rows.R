test_that("tidy_add_estimate_to_reference_rows() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res$estimate[res$reference_row & !is.na(res$reference_row)],
    c(0, 0, 0)
  )

  res <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res$estimate[res$reference_row & !is.na(res$reference_row)],
    c(1, 1, 1)
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment(4, base = 3), grade = contr.treatment(3, base = 2), trt = contr.SAS)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res$estimate[res$reference_row & !is.na(res$reference_row)],
    c(0, 0, 0)
  )

  res <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res$estimate[res$reference_row & !is.na(res$reference_row)],
    c(1, 1, 1)
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.sum, grade = contr.sum, trt = contr.sum)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  # should be -1 * sum of other coefficients when sum contrasts
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "stage" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "stage"], na.rm = TRUE) * -1
  )
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "grade" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "grade"], na.rm = TRUE) * -1
  )
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "trt" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "trt"], na.rm = TRUE) * -1
  )

  res2 <- mod %>%
    tidy_and_attach(exponentiate = TRUE) %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res2$estimate[res2$reference_row & res2$variable == "stage" & !is.na(res2$reference_row)],
    exp(sum(res$estimate[!res$reference_row & res$variable == "stage"], na.rm = TRUE) * -1)
  )
  expect_equivalent(
    res2$estimate[res2$reference_row & res2$variable == "grade" & !is.na(res2$reference_row)],
    exp(sum(res$estimate[!res$reference_row & res$variable == "grade"], na.rm = TRUE) * -1)
  )
  expect_equivalent(
    res2$estimate[res2$reference_row & res2$variable == "trt" & !is.na(res2$reference_row)],
    exp(sum(res$estimate[!res$reference_row & res$variable == "trt"], na.rm = TRUE) * -1)
  )

  ## works also when there is an interaction term
  mod <- glm(response ~ stage * grade * trt, gtsummary::trial,
             family = binomial,
             contrasts = list(stage = contr.sum, grade = contr.sum, trt = contr.sum)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  # should be -1 * sum of other coefficients when sum contrasts
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "stage" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "stage"], na.rm = TRUE) * -1
  )
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "grade" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "grade"], na.rm = TRUE) * -1
  )
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "trt" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "trt"], na.rm = TRUE) * -1
  )
})


test_that("test tidy_add_estimate_to_reference_rows() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_add_estimate_to_reference_rows(exponentiate = TRUE))

  # expect an error if no value for exponentiate
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(exponentiate = NULL))
  expect_error(mod %>% broom::tidy() %>% tidy_attach_model(mod) %>% tidy_add_estimate_to_reference_rows())

  # expect a message if this is a model not covered by emmeans
  mod <- glm(
    response ~ stage + grade + trt, gtsummary::trial,
    family = binomial, contrasts = list(grade = contr.sum)
  )
  res <- mod %>% tidy_and_attach() %>% tidy_add_reference_rows()
  class(mod) <- "unknown"
  expect_message(
    res %>% tidy_add_estimate_to_reference_rows(model = mod)
  )
})

test_that("tidy_add_estimate_to_reference_rows() works with character variables", {
  df <- gtsummary::trial %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))
  mod <- glm(response ~ stage + grade + trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res$estimate[res$reference_row & !is.na(res$reference_row)],
    c(0, 0, 0)
  )

  mod <- glm(response ~ stage + grade + trt, df,
    family = binomial,
    contrasts = list(stage = contr.treatment(4, base = 3), grade = contr.treatment(3, base = 2), trt = contr.SAS)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  expect_equivalent(
    res$estimate[res$reference_row & !is.na(res$reference_row)],
    c(0, 0, 0)
  )

  mod <- glm(response ~ stage + grade + trt, df,
    family = binomial,
    contrasts = list(stage = contr.sum, grade = contr.sum, trt = contr.sum)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_estimate_to_reference_rows()
  # should be -1 * sum of other coefficients when sum contrasts
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "stage" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "stage"], na.rm = TRUE) * -1
  )
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "grade" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "grade"], na.rm = TRUE) * -1
  )
  expect_equivalent(
    res$estimate[res$reference_row & res$variable == "trt" & !is.na(res$reference_row)],
    sum(res$estimate[!res$reference_row & res$variable == "trt"], na.rm = TRUE) * -1
  )
})


test_that("tidy_add_estimate_to_reference_rows() handles variables having non standard name", {
  df <- gtsummary::trial %>% dplyr::mutate(`grade of kids` = grade)
  mod <- glm(response ~ stage + `grade of kids` + trt, df,
    family = binomial,
    contrasts = list(`grade of kids` = contr.sum)
  )
  expect_message(
    res <- mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(),
    NA
  )
  expect_equivalent(
    res$estimate[res$variable == "grade of kids" & !is.na(res$variable)] %>% sum(),
    0
  )
})

test_that("tidy_add_estimate_to_reference_rows() preserve estimates of continuous variables", {
  mod <- glm(response ~ poly(age, 3) + ttdeath, na.omit(gtsummary::trial), family = binomial)
  res1 <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows()
  res2 <- res1 %>% tidy_add_estimate_to_reference_rows()
  expect_equivalent(res1$estimate, res2$estimate)
})


test_that("tidy_add_estimate_to_reference_rows() works with lme4::lmer", {
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  mod <- lme4::lmer(marker ~ stage + grade + (1 | group), df)
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with lme4::glmer", {
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  suppressMessages(
    mod <- lme4::glmer(response ~ stage + grade + (1 | group), df, family = binomial)
  )
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})

test_that("tidy_add_estimate_to_reference_rows() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ factor(ecog.ps) + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})

test_that("tidy_add_estimate_to_reference_rows() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)

  # no dummy coef for multinom
  # should return a warning but not an error
  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.sum)
  )
  expect_message(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows())
})

test_that("tidy_add_estimate_to_reference_rows() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})

test_that("tidy_add_estimate_to_reference_rows() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~contact)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})


test_that("tidy_add_estimate_to_reference_rows() works with lavaan::lavaan", {
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
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_estimate_to_reference_rows(), NA)
})

