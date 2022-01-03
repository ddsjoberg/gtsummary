skip_on_cran()
skip_if_not(requireNamespace("lme4"))
library(survival)
library(lme4)


test_that("lm: no errors/warnings with standard use", {
  expect_error(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = mpg
    ), NA)
  expect_error(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = "mpg"
    ), NA)
  expect_warning(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = mpg
    ), NA)

  expect_false(
    "ci" %in%
      names(tbl_uvregression(mtcars, method = lm, y = mpg, conf.int = FALSE) %>%
          as_tibble(col_labels = FALSE))
  )
})

test_that("geeglm: no errors/warnings with standard use", {
  skip_if_not(requireNamespace("geepack"))

  expect_error(
    tbl_uvregression(
      na.omit(trial),
      y = age,
      method = geepack::geeglm,
      method.args = list(
        id = response,
        corstr = "exchangeable"
      ),
      include = -response
    ), NA
  )
  expect_warning(
    tbl_uvregression(
      na.omit(trial),
      y = age,
      method = geepack::geeglm,
      method.args = list(
        id = response,
        corstr = "exchangeable"
      ),
      include = -response
    ), NA
  )
})

test_that("lm specifying tidy_fun: no errors/warnings with standard use", {
  expect_error(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = mpg,
      tidy_fun = broom::tidy
    ), NA)
  expect_warning(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = mpg,
      tidy_fun = broom::tidy
    ), NA)
})

test_that("coxph: no errors/warnings with standard use", {
  expect_error(
    coxph_uv <-
      lung %>%
      tbl_uvregression(
        method = coxph,
        y = Surv(time, status)
      ), NA
  )
  expect_warning(
    lung %>%
      tbl_uvregression(
        method = coxph,
        y = Surv(time, status)
      ), NA
  )

  expect_identical(
    coxph_uv$meta_data$variable,
    setdiff(names(lung), c("time", "status"))
  )
})


test_that("glmer: no errors/warnings with standard use", {
  expect_error(
    lme4_uv <-
      mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial),
        label = "cyl" ~ "No. Cylinders",
        hide_n = TRUE
      ), NA
  )
  expect_warning(
    mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial)
      ), NA
  )

  expect_identical(
    lme4_uv$meta_data$variable,
    c("hp", "cyl")
  )

  expect_error(
    mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial),
        label = "cyl" ~ "No. Cylinders",
        hide_n = TRUE,
        include = c("am", "gear", "hp", "cyl"),
      ), NA
  )
  expect_warning(
    mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial),
        include = c("am", "gear", "hp", "cyl"),
      ), NA
  )
})

test_that("tbl_uvregression x= argument tests", {
  expect_error(
    ux_x <- tbl_uvregression(
      data = trial[c("age", "marker", "response")],
      method = lm,
      label = list(vars(`age`) ~ "PATIENT AGE"),
      x = response
    ),
    NA
  )

  expect_equal(
    ux_x %>%
      as_tibble() %>%
      names() %>%
      purrr::pluck(1),
    "**Outcome**"
  )

  expect_equal(
    ux_x$meta_data$label[1] %>% .[[1]],
    "PATIENT AGE"
  )

  expect_identical(
    ux_x$tbls$age$model_obj %>% coef(),
    lm(age ~ response, trial) %>% coef()
  )
})

test_that("tbl_uvregression creates errors with bad inputs", {
  expect_error(
    tbl_uvregression(
      data = mtcars,
      method = coxph,
      y = Surv(time, status),
      pvalue_fun = mtcars
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      estimate_fun = mtcars
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = mtcars,
      method = coxph,
      y = Surv(time, status),
      tidy_fun = mtcars
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      label = "Labels! YAY"
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      label = list("Age")
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      label = list("age" ~ c("Age", "Two"))
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = list(lung),
      method = coxph,
      y = Surv(time, status)
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      formula = "y ~ x"
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = lm,
      y = age,
      x = marker
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = lm,
      y = c(age, sex)
    ),
    NULL
  )
})


test_that("tbl_uvregression estimate_fun and pvalue_fun respected", {
  tbl_fmt <- tbl_uvregression(
    data = lung %>% select(age, inst),
    method = lm,
    y = age,
    pvalue_fun = ~ style_pvalue(.x, digits = 3),
    estimate_fun = ~ style_number(.x, digits = 3)
  )

  expect_equal(
    tbl_fmt %>% as_tibble(col_labels = FALSE) %>% purrr::map_chr(I) %>% as.vector(),
    c("inst", "227", "0.001", "-0.143, 0.144", "0.993")
  )
})


test_that("tbl_uvregression does not throw error with odd variable names in `data=`", {
  expect_error(
    trial %>% dplyr::rename(`age person` = age) %>% tbl_uvregression(method = lm, y = `age person`),
    NA
  )
})

test_that("tbl_uvregression throw error with bad arguments in model function", {
  expect_error(
    trial %>%
      tbl_uvregression(method = glm, y = response, method.args = list(not_an_argument = letters))
  )
})

test_that("tbl_uvregression works with survey object", {
  skip_if_not(requireNamespace("survey"))
  svy <- survey::svydesign(ids = ~1, data = trial, weights = ~1)

  expect_error(
    tbl_uvreg <-
      svy %>%
      tbl_uvregression(
        y = response,
        method = survey::svyglm,
        method.args = list(family = binomial),
        hide_n = TRUE,
        include = c(response, age, marker, grade)
      ),
    NA
  )

  expect_equal(
    tbl_uvreg$tbls$age$model_obj %>% broom::tidy(),
    survey::svyglm(response ~ age, design = svy, family = binomial) %>% broom::tidy()
  )
})

