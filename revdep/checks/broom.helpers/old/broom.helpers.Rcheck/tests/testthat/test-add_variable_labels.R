test_that("tidy_add_variable_labels() works for basic models", {
  # if no variable labels, variable names
  # term for intercept
  df <- gtsummary::trial
  labelled::var_label(df) <- NULL
  mod <- glm(response ~ age + grade + trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c("(Intercept)", "age", "grade", "grade", "trt")
  )

  # if variable labels defined in data, variable labels
  df <- gtsummary::trial
  mod <- glm(response ~ age + grade + trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c("(Intercept)", "Age", "Grade", "Grade", "Chemotherapy Treatment")
  )

  # if labels provided in `labels`, taken into account
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels(
      labels = list(`(Intercept)` = "custom intercept", grade = "custom label")
    )
  expect_equivalent(
    res$var_label,
    c(
      "custom intercept", "Age", "custom label", "custom label",
      "Chemotherapy Treatment"
    )
  )

  # labels can also be a named vector
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels(
      labels = c(`(Intercept)` = "custom intercept", grade = "custom label")
    )
  expect_equivalent(
    res$var_label,
    c(
      "custom intercept", "Age", "custom label", "custom label",
      "Chemotherapy Treatment"
    )
  )

  # no error if providing labels not corresponding to an existing variable
  # but display a message
  expect_error(
    mod %>%
      tidy_and_attach() %>%
      tidy_add_variable_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44)
      ),
    NA
  )
  expect_message(
    mod %>%
      tidy_and_attach() %>%
      tidy_add_variable_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44)
      )
  )
  expect_error(
    mod %>%
      tidy_and_attach() %>%
      tidy_add_variable_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44),
        strict = TRUE
      )
  )

  # model with only an interaction term
  mod <- lm(age ~ factor(response):marker, gtsummary::trial)
  res <- mod %>% tidy_and_attach() %>% tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c(
      "(Intercept)",
      "factor(response) * Marker Level (ng/mL)",
      "factor(response) * Marker Level (ng/mL)"
    )
  )

  # custom label for interaction term
  mod <- glm(response ~ age + grade * trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels(labels = c("grade:trt" = "custom label"))
  expect_equivalent(
    res$var_label,
    c("(Intercept)", "Age", "Grade", "Grade", "Chemotherapy Treatment",
      "custom label", "custom label")
  )
})



test_that("test tidy_add_variable_labels() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_add_variable_labels())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_add_variable_labels() %>% tidy_add_variable_labels(),
    NA
  )

  # cannot be applied after tidy_add_header_rows()
  expect_error(
    mod %>% tidy_and_attach() %>%
      tidy_add_header_rows() %>%
      tidy_add_variable_labels()
  )
})


test_that("tidy_add_variable_labels() correctly manages interaction terms", {
  df <- gtsummary::trial
  mod <- glm(response ~ age * grade * trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c(
      "(Intercept)", "Age", "Grade", "Grade", "Chemotherapy Treatment",
      "Age * Grade", "Age * Grade", "Age * Chemotherapy Treatment",
      "Grade * Chemotherapy Treatment", "Grade * Chemotherapy Treatment",
      "Age * Grade * Chemotherapy Treatment", "Age * Grade * Chemotherapy Treatment"
    )
  )

  # custom separator and custom labels for certain interaction terms
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels(
      interaction_sep = ":::",
      labels = c(
        "age:grade" = "custom interaction label",
        "grade:trt" = "a second custom label"
      )
    )
  expect_equivalent(
    res$var_label,
    c(
      "(Intercept)", "Age", "Grade", "Grade", "Chemotherapy Treatment",
      "custom interaction label", "custom interaction label", "Age:::Chemotherapy Treatment",
      "a second custom label", "a second custom label", "Age:::Grade:::Chemotherapy Treatment",
      "Age:::Grade:::Chemotherapy Treatment"
    )
  )
})


test_that("tidy_add_variable_labels() works with variables having non standard name", {
  df <- gtsummary::trial %>% dplyr::mutate(`grade of kids` = grade)
  mod <- lm(age ~ marker * `grade of kids`, df)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c(
      "(Intercept)", "Marker Level (ng/mL)", "Grade", "Grade", "Marker Level (ng/mL) * Grade",
      "Marker Level (ng/mL) * Grade"
    )
  )
})


test_that("tidy_add_variable_labels() works with stats::poly()", {
  df <- iris %>% labelled::set_variable_labels(Petal.Length = "Length of petal")
  mod <- lm(Sepal.Length ~ poly(Sepal.Width, 3) + poly(Petal.Length, 2), df)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels(labels = c(Sepal.Width = "Width of sepal"))
  expect_equivalent(
    res$var_label,
    c(
      "(Intercept)", "Width of sepal", "Width of sepal", "Width of sepal",
      "Petal.Length", "Petal.Length"
    )
  )
})


test_that("tidy_add_variable_labels() works with lme4::lmer", {
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with lme4::glmer", {
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial, data = lme4::cbpp
  )
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)


  # check that label attribute in original dataset is preserved
  mod <- survival::coxph(survival::Surv(ttdeath, death) ~ grade, gtsummary::trial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables() %>%
    tidy_add_reference_rows() %>%
    tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c("Grade", "Grade", "Grade")
  )
})

test_that("tidy_add_variable_labels() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)

  # check that label attribute in original dataset is preserved
  mod <- survival::survreg(survival::Surv(ttdeath, death) ~ grade, gtsummary::trial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables() %>%
    tidy_add_reference_rows() %>%
    tidy_add_variable_labels()
  expect_equivalent(
    res$var_label,
    c("(Intercept)", "Grade", "Grade", "Grade", "Log(scale)")
  )
})

test_that("tidy_add_variable_labels() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})

test_that("tidy_add_variable_labels() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})

test_that("tidy_add_variable_labels() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~contact)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)

  mod <- suppressWarnings(gam::gam(
    Ozone^(1 / 3) ~ gam::lo(Solar.R) + gam::lo(Wind, Temp),
    data = datasets::airquality, na = gam::na.gam.replace
  ))
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})


test_that("tidy_add_variable_labels() works with lavaan::lavaan", {
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
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_variable_labels(), NA)
})
