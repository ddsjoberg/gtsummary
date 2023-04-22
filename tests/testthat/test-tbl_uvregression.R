skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survival", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))

test_that("lm: no errors/warnings with standard use", {
  expect_snapshot(
    mtcars %>%
      tbl_uvregression(
        method = lm,
        y = mpg
      ) %>%
      as.data.frame()
  )
  expect_snapshot(
    mtcars %>%
      tbl_uvregression(
        method = lm,
        y = "mpg"
      ) %>%
      as.data.frame()
  )
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
  skip_if_not(broom.helpers::.assert_package("geepack", pkg_search = "gtsummary", boolean = TRUE))

  expect_snapshot(
    tbl_uvregression(
      na.omit(trial),
      y = age,
      method = geepack::geeglm,
      method.args = list(
        id = response,
        corstr = "exchangeable"
      ),
      include = -response
    ) %>%
      as.data.frame()
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
  expect_snapshot(
    mtcars %>%
      tbl_uvregression(
        method = lm,
        y = mpg,
        tidy_fun = broom::tidy
      ) %>%
      as.data.frame()
  )
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
      survival::lung %>%
      tbl_uvregression(
        method = survival::coxph,
        y = survival::Surv(time, status)
      ), NA
  )
  expect_snapshot(coxph_uv %>% as.data.frame())
  expect_warning(
    survival::lung %>%
      tbl_uvregression(
        method = survival::coxph,
        y = survival::Surv(time, status)
      ), NA
  )

  expect_identical(
    coxph_uv$meta_data$variable,
    setdiff(names(survival::lung), c("time", "status"))
  )
})


test_that("glmer: no errors/warnings with standard use", {
  expect_error(
    lme4_uv <-
      mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = lme4::glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial),
        label = "cyl" ~ "No. Cylinders",
        hide_n = TRUE
      ),
    NA
  )
  expect_snapshot(lme4_uv %>% as.data.frame())
  expect_warning(
    mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = lme4::glmer,
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
    tbl <-
      mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = lme4::glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial),
        label = "cyl" ~ "No. Cylinders",
        hide_n = TRUE,
        include = c("am", "gear", "hp", "cyl"),
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
  expect_warning(
    mtcars %>%
      select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = lme4::glmer,
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
  expect_snapshot(ux_x %>% as.data.frame())

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
      method = survival::coxph,
      y = survival::Surv(time, status),
      pvalue_fun = mtcars
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = survival::coxph,
      y = survival::Surv(time, status),
      estimate_fun = mtcars
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = mtcars,
      method = survival::coxph,
      y = survival::Surv(time, status),
      tidy_fun = mtcars
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = survival::coxph,
      y = survival::Surv(time, status),
      label = "Labels! YAY"
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = survival::coxph,
      y = survival::Surv(time, status),
      label = list("Age")
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = survival::coxph,
      y = survival::Surv(time, status),
      label = list("age" ~ c("Age", "Two"))
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = list(survival::lung),
      method = survival::coxph,
      y = survival::Surv(time, status)
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = survival::coxph,
      y = survival::Surv(time, status),
      formula = "y ~ x"
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = lm,
      y = age,
      x = marker
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = lm,
      y = c(age, sex)
    ),
    NULL
  )
  expect_error(
    tbl_uvregression(
      data = survival::lung,
      method = "lm",
      y = age,
      include = c("time", "sex")
    ),
    "*required and must be a function*"
  )
})


test_that("tbl_uvregression estimate_fun and pvalue_fun respected", {
  tbl_fmt <- tbl_uvregression(
    data = survival::lung %>% select(age, inst),
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
    tbl <- trial %>% dplyr::rename(`age person` = age) %>% tbl_uvregression(method = lm, y = `age person`),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
})

test_that("tbl_uvregression throw error with bad arguments in model function", {
  expect_error(
    trial %>%
      tbl_uvregression(method = glm, y = response, method.args = list(not_an_argument = letters))
  )
})

test_that("tbl_uvregression works with survey object", {
  skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))
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
  expect_snapshot(tbl_uvreg %>% as.data.frame())

  expect_equal(
    tbl_uvreg$tbls$age$model_obj %>% broom::tidy(),
    survey::svyglm(response ~ age, design = svy, family = binomial) %>% broom::tidy()
  )
})

test_that("tbl_uvregression() can pass the dots to tidy_plus_plus()", {
  # create a table with no reference row and no header rows (3 rows long)
  expect_equal(
    trial %>%
      tbl_uvregression(
        y = age,
        method = lm,
        include = c(trt, grade),
        no_reference_row = any_of(c("trt", "grade")),
        add_header_rows = FALSE
      ) %>%
      as_tibble() %>%
      nrow(),
    3L
  )
})
