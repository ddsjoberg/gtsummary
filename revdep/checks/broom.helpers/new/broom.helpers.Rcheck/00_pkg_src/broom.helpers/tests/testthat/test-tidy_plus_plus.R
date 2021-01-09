test_that("tidy_plus_plus() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_error(
    mod %>% tidy_plus_plus(add_header_rows = TRUE, include = c(stage, grade)),
    NA
  )

  # combining custom variable labels with categorical_terms_pattern
  # check that the custom variable labels are passed to model_list_terms_levels()
  res <- mod %>%
    tidy_plus_plus(
      variable_labels = c(grade = "custom"),
      add_reference_rows = FALSE,
      categorical_terms_pattern = "{var_label}:{level}/{reference_level}"
    )
  expect_equivalent(
    res$label,
    c("T Stage:T2/T1", "T Stage:T3/T1", "T Stage:T4/T1", "custom:II/I",
      "custom:III/I", "Chemotherapy Treatment:Drug B/Drug A")
  )
})


test_that("tidy_plus_plus() and functionnal programming", {
  # works with glm
  expect_error(
    res <- dplyr::tibble(grade = c("I", "II", "III")) %>%
      dplyr::mutate(df_model = purrr:::map(grade, ~ gtsummary::trial %>% dplyr::filter(grade == ..1))) %>%
      dplyr::mutate(
        mv_formula_char = "response ~ trt + age + marker",
        mv_formula = purrr::map(mv_formula_char, ~ as.formula(.x)),
        mv_model_form =
          purrr::map2(
            mv_formula, df_model,
            ~ glm(..1, data = ..2)
          ),
        mv_tbl_form =
          purrr::map(
            mv_model_form,
            ~ tidy_plus_plus(..1, exponentiate = TRUE, add_header_rows = TRUE)
          )
      ),
    NA
  )

  # for coxph, identification of variables will not work
  # will display a message
  # but a result should be returned
  expect_message(
    res <- dplyr::tibble(grade = c("I", "II", "III")) %>%
      dplyr::mutate(df_model = purrr:::map(grade, ~ gtsummary::trial %>% dplyr::filter(grade == ..1))) %>%
      dplyr::mutate(
        mv_formula_char = "survival::Surv(ttdeath, death) ~ trt + age + marker",
        mv_formula = purrr::map(mv_formula_char, ~ as.formula(.x)),
        mv_model_form =
          purrr::map2(
            mv_formula, df_model,
            ~ survival::coxph(..1, data = ..2)
          ),
        mv_tbl_form =
          purrr::map(
            mv_model_form,
            ~ tidy_plus_plus(..1, exponentiate = TRUE)
          )
      )
  )
})


test_that("tidy_plus_plus() with mice objects", {
  # impute missing values
  imputed_trial <-
    suppressWarnings(mice::mice(gtsummary::trial, maxit = 2, m = 2))
  # build regression model
  mod <- with(imputed_trial, lm(age ~ marker + grade))

  # testing pre-pooled results
  expect_error(
    tidy_plus_plus(
      mod,
      exponentiate = FALSE,
      tidy_fun = function(x, ...) mice::pool(x) %>% mice::tidy(...)
    ),
    NA
  )
})


test_that("tidy_plus_plus() with tidyselect", {
  # build regression model
  mod <- lm(age ~ trt + marker + grade, gtsummary::trial)

  expect_error(
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = trt,
      no_reference_row = grade
    ),
    NA
  )

  expect_equal(
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = "trt",
      no_reference_row = "grade"
    ),
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = trt,
      no_reference_row = grade
    )
  )
})

test_that("tidy_plus_plus() works with stats::aov", {
  mod <- aov(yield ~ block + N*P*K, npk)
  expect_error(
    res <- tidy_plus_plus(mod),
    NA
  )
  expect_equivalent(
    res$variable,
    c("block", "N", "P", "K", "N:P", "N:K", "P:K")
  )
})

test_that("tidy_plus_plus() works with lme4::lmer", {
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  skip_if_not_installed("broom.mixed")
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lme4::glmer", {
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     family = binomial, data = lme4::cbpp
  )
  skip_if_not_installed("broom.mixed")
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     family = binomial("probit"), data = lme4::cbpp
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lme4::glmer.nb", {
  dd <- expand.grid(
    f1 = factor(1:3),
    f2 = LETTERS[1:2], g = 1:9, rep = 1:15,
    KEEP.OUT.ATTRS = FALSE
  )
  mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2)))
  dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
  skip_if_not_installed("lme4")
  library(lme4)
  mod <- lme4::glmer.nb(y ~ f1*f2 + (1|g), data = dd)
  skip_if_not_installed("broom.mixed")
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with survival::clogit", {
  library(survival)
  resp <- levels(survival::logan$occupation)
  n <- nrow(survival::logan)
  indx <- rep(1:n, length(resp))
  logan2 <- data.frame(survival::logan[indx,],
                       id = indx,
                       tocc = factor(rep(resp, each=n)))
  logan2$case <- (logan2$occupation == logan2$tocc)
  mod <- survival::clogit(case ~ tocc + tocc:education + strata(id), logan2)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survey::svycoxph", {
  dpbc <- survey::svydesign(id = ~ 1, prob = ~ 1, strata = ~ edema, data = survival::pbc)
  mod <- survey::svycoxph(Surv(time, status>0) ~ log(bili) + protime + albumin, design = dpbc)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survey::svyolr", {
  data(api, package = "survey")
  fpc <- survey::svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
  fpc <- update(fpc, mealcat=cut(meals,c(0,25,50,75,100)))
  mod <- survey::svyolr(mealcat~avg.ed+mobility+stype, design = fpc)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with MASS::glm.nb", {
  mod <- MASS::glm.nb(Days ~ Sex / (Age + Eth * Lrn), data = MASS::quine)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("log"), corstr = "ar1")
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lavaan::lavaan", {
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
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() error messaging", {
  # does not allow for exponentiate, conf.inf, conf.level arguments
  bad_tidy <- function(x) {
    broom::tidy
  }

  expect_error(
    lm(mpg ~ cyl, mtcars) %>%
      tidy_plus_plus(tidy_fun = bad_tidy)
  )
})

