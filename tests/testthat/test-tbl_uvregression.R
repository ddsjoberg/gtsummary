skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "broom", "survival", "survey")))

test_that("tbl_uvregression(x)", {
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        x = trt,
        include = c(marker, age),
        show_single_row = trt,
        method = lm
      )
  )
  expect_snapshot(as.data.frame(tbl1))
  expect_equal(
    tbl1$tbls$age$inputs$x |> broom::tidy(),
    lm(age ~ trt, trial) |> broom::tidy()
  )
})

test_that("tbl_uvregression(method)", {
  # first passing method three way, and checking equality of results
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = age,
        include = c(marker, grade),
        method = lm
      )
  )
  expect_silent(
    tbl2 <-
      tbl_uvregression(
        trial,
        y = age,
        include = c(marker, grade),
        method = "lm"
      )
  )
  expect_silent(
    tbl3 <-
      tbl_uvregression(
        trial,
        y = age,
        include = c(marker, grade),
        method = stats::lm
      )
  )
  expect_equal(as.data.frame(tbl1), as.data.frame(tbl2))
  expect_equal(as.data.frame(tbl1), as.data.frame(tbl3))
  expect_snapshot(as.data.frame(tbl1))
})

test_that("tbl_uvregression(method.args)", {
  # testing typical use of the argument
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = response,
        method = glm,
        method.args = list(family = binomial),
        include = c(age, trt)
      )
  )
  # check Ns are correct
  expect_equal(
    tbl1$table_body$stat_n |> na.omit(),
    c(trial[c("response", "age")] |> na.omit() |> nrow(),
      trial[c("response", "trt")] |> na.omit() |> nrow()),
    ignore_attr = TRUE
  )
  # check model coefs are correct
  expect_equal(
    tbl1$table_body$estimate |> na.omit(),
    c(glm(response ~ age, trial, family = binomial) |> coef() |> getElement("age"),
      glm(response ~ trt, trial, family = binomial) |> coef() |> getElement("trtDrug B")),
    ignore_attr = TRUE
  )

  # check with NSE argument
  expect_error(
    tbl2 <-
      tbl_uvregression(
        trial,
        y = survival::Surv(ttdeath, death),
        method = survival::coxph,
        method.args = list(id = response),
        include = c(age, trt)
      ),
    NA
  )
  # check models are the same
  expect_equal(
    tbl2$tbls$age$inputs$x |> broom::tidy(),
    survival::coxph(survival::Surv(ttdeath, death) ~ age, trial, id = response) |>
      broom::tidy()
  )
  expect_equal(
    tbl2$tbls$trt$inputs$x |> broom::tidy(),
    survival::coxph(survival::Surv(ttdeath, death) ~ trt, trial, id = response) |>
      broom::tidy()
  )
  expect_snapshot(as.data.frame(tbl2))
})

test_that("tbl_uvregression(exponentiate)", {
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = response,
        method = glm,
        method.args = list(family = binomial),
        include = c(age, trt),
        exponentiate = TRUE
      )
  )
  expect_equal(
    tbl1$table_styling$header |>
      dplyr::filter(column == "estimate") |>
      dplyr::pull("label"),
    "**OR**"
  )
  expect_equal(
    tbl1$table_body$estimate[1],
    glm(response ~ age, trial, family = binomial) |>
      broom::tidy(exponentiate = TRUE) |>
      dplyr::pull("estimate") |>
      dplyr::last()
  )
})

test_that("tbl_uvregression(label)", {
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = marker,
        method = lm,
        include = c(age, trt),
        label = list(age = "AGE", trt = "TRT")
      )
  )
  expect_equal(
    tbl1$table_body$label[1:2],
    c("AGE", "TRT"),
    ignore_attr = TRUE
  )

  expect_silent(
    tbl2 <-
      tbl_uvregression(
        trial,
        x = trt,
        method = lm,
        include = c(age, marker),
        label = list(age = "AGE", marker = "MARKER"),
        show_single_row = trt
      )
  )
  expect_equal(
    tbl2$table_body$label[1:2],
    c("AGE", "MARKER"),
    ignore_attr = TRUE
  )
})

test_that("tbl_uvregression(include)", {
  # check that trt is removed from include appropriately
  expect_silent(
    tbl1 <-
      trial |>
      dplyr::select(trt, age, marker) |>
      tbl_uvregression(
        x = trt,
        method = "lm",
        show_single_row = trt,
        include = everything()
      )
  )
  expect_equal(tbl1$inputs$include, c("age", "marker"))
  expect_snapshot(as.data.frame(tbl1))

  # check that ttdeath and death are removed from include
  expect_silent(
    tbl2 <-
      trial |>
      dplyr::select(ttdeath, death, age, marker) |>
      tbl_uvregression(
        y = survival::Surv(ttdeath, death),
        method = survival::coxph,
        exponentiate = TRUE,
        include = everything()
      )
  )
  expect_equal(tbl2$inputs$include, c("age", "marker"))
  expect_snapshot(as.data.frame(tbl2))

  # check that variables in the formula are removed from include
  expect_equal(
    trial |>
      select(age, grade, trt) |>
      tbl_uvregression(
        y = age,
        method = "lm",
        formula = "{y} ~ {x} + grade"
      ) |>
      getElement("table_body") |>
      getElement("variable") |>
      unique(),
    "trt"
  )

  # check we get an error if no variables are selected
  expect_error(
    tbl_uvregression(
      trial,
      y = age,
      method = "lm",
      include  = starts_with("xxxx")
    ),
    "The `include` argument cannot be empty"
  )

  # ensure non-syntactic names work in the `include` variables
  expect_snapshot(
    mtcars |>
      dplyr::select(`M P G` = mpg, hp) |>
      tbl_uvregression(y = hp, method = lm) |>
      as.data.frame()
  )
  expect_snapshot(
    mtcars |>
      dplyr::select(`M P G` = mpg, hp) |>
      tbl_uvregression(x = hp, method = lm) |>
      as.data.frame()
  )
})

test_that("tbl_uvregression(tidy_fun)", {
  # check the tidy_fun is respected
  expect_snapshot(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = age,
        method = lm,
        include = marker,
        tidy_fun = function(x, ...) {
          cat("THIS IS MY CUSTOM MESSAGE!")
          broom::tidy(x, ...)
        }
      )
  )
})

test_that("tbl_uvregression(hide_n)", {
  expect_equal(
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = marker,
      hide_n = TRUE
    ) |>
      as.data.frame(),
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = marker
    ) |>
      modify_column_hide(stat_n) |>
      as.data.frame()
  )
})

test_that("tbl_uvregression(conf.level)", {
  expect_equal(
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = marker,
      conf.level = 0.90
    ) |>
      getElement("table_body") |>
      dplyr::select(conf.low, conf.high) |>
      as.list(),
    lm(age ~ marker, trial) |>
      broom::tidy(conf.level = 0.9, conf.int = TRUE) |>
      dplyr::filter(term == "marker") |>
      dplyr::select(conf.low, conf.high) |>
      as.list(),
    ignore_attr = TRUE
  )
})

test_that("tbl_uvregression(estimate_fun)", {
  expect_snapshot(
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = c(marker, trt),
      estimate_fun = label_style_sigfig(digits = 4)
    ) |>
      as.data.frame(col_label = FALSE)
  )
})

test_that("tbl_uvregression(pvalue_fun)", {
  expect_snapshot(
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = c(marker, trt),
      pvalue_fun = label_style_pvalue(digits = 3)
    ) |>
      as.data.frame(col_label = FALSE)
  )
})

test_that("tbl_uvregression(formula)", {
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = age,
        method = lm,
        include = c(marker, trt),
        formula = "{y} ~ {x} + grade"
      )
  )
  expect_equal(
    tbl1$tbls$marker$inputs$x |> broom::tidy(),
    lm(age ~ marker + grade, trial) |> broom::tidy()
  )
})

test_that("tbl_uvregression(add_estimate_to_reference_rows)", {
  expect_equal(
    tbl_uvregression(
      trial,
      y = response,
      method = glm,
      method.args = list(family = binomial),
      include = trt,
      add_estimate_to_reference_rows = TRUE,
      exponentiate = TRUE
    ) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::pull(estimate),
    c(NA, "1.00", "1.21")
  )
})

test_that("tbl_uvregression(conf.int)", {
  expect_snapshot(
    tbl_uvregression(
      trial,
      y = response,
      method = glm,
      method.args = list(family = binomial),
      include = trt,
      conf.int = FALSE
    ) |>
      as.data.frame()
  )
})

# test the dots are passed on to broom.helpers, and the tidy function
test_that("tbl_uvregression(...)", {
  expect_snapshot(
    tbl_uvregression(
      trial,
      y = response,
      method = glm,
      method.args = list(family = binomial),
      include = trt,
      add_header_rows = FALSE
    ) |>
      as.data.frame()
  )
})

test_that("tbl_uvregression(x,y) messaging", {
  # expect error when both or neither x/y specified
  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      method = lm,
      include = trt
    )
  )
  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      x = age, y = marker,
      method = lm,
      include = trt
    )
  )
})

test_that("tbl_uvregression(formula) messaging", {
  # problems with formula argument
  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = trt,
      formula = "{y} ~ {y}"
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = trt,
      formula = "{y} ~ {x} + {x}"
    )
  )

  # no tilda
  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = trt,
      formula = "{y} {x}"
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial |> dplyr::rename(`Tx Effect` = trt),
      y = "Tx Effect",
      method = glm,
      method.args = list(family = binomial),
      include = age
    )
  )
})

test_that("tbl_uvregression(method.args) messaging", {
  # error with an incorrect argument passed
  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      y = response,
      method = glm,
      method.args = list(not_an_arg = FALSE),
      include = trt
    )
  )

  expect_snapshot(
    tbl <- tbl_uvregression(
      trial,
      y = age,
      method = lm,
      method.args = list(not_an_arg = FALSE),
      include = trt
    )
  )
})

test_that("tbl_uvregression() messaging", {
  # introduce an error in the `tbl_regression()` step
  expect_snapshot(
    error = TRUE,
    tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = trt,
      tidy_fun = \(x, ...) stop("Inducing an error")
    )
  )

  expect_snapshot(
    tbl <- tbl_uvregression(
      trial,
      y = age,
      method = lm,
      include = trt,
      tidy_fun = \(x, ...) {
        warning("Inducing an warning")
        broom::tidy(x, ...)
      }
    )
  )
})

# check function works with a survey.design object
test_that("tbl_uvregression() with survey.design", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~dnum + snum, weights = ~pw, data = apiclus2)
  survey::svyglm(api00 ~ ell + meals + mobility, design = dclus2)

  expect_silent(
    tbl1 <- tbl_uvregression(
      dclus2,
      y = api00,
      include = c(ell, meals),
      method = survey::svyglm
    )
  )
  # check models are the same
  expect_equal(
    tbl1$tbls$ell$inputs$x |> broom::tidy(),
    survey::svyglm(api00 ~ ell, design = dclus2) |> broom::tidy()
  )
})
