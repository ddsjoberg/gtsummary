skip_on_cran()
skip_if_not(is_pkg_installed(c("broom.helpers", "cardx"), reference_pkg = "gtsummary"))

# add_glance_source_note() -----------------------------------------------------
test_that("add_glance_source_note(x)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note() |>
      getElement("table_styling") |>
      getElement("source_note"),
    "R² = 0.000; Adjusted R² = -0.005; Sigma = 14.3; Statistic = 0.044; p-value = 0.8; df = 1; Log-likelihood = -771; AIC = 1,547; BIC = 1,557; Deviance = 38,499; Residual df = 187; No. Obs. = 189",
    ignore_attr = TRUE
  )
})

test_that("add_glance_source_note(include,label)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(include = r.squared, label = r.squared ~ "R * R") |>
      getElement("table_styling") |>
      getElement("source_note"),
    "R * R = 0.000",
    ignore_attr = TRUE
  )
})

test_that("add_glance_source_note(fmt_fn)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(fmt_fun = ~label_style_sigfig(digits = 5), include = 1:3) |>
      getElement("table_styling") |>
      getElement("source_note"),
    "R² = 0.00024; Adjusted R² = -0.00511; Sigma = 14.348",
    ignore_attr = TRUE
  )
})

test_that("add_glance_source_note(glance_fun)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(glance_fun = \(x, ...) broom::glance(x, ...) |> dplyr::select(1:3)) |>
      getElement("table_styling") |>
      getElement("source_note"),
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(include = 1:3) |>
      getElement("table_styling") |>
      getElement("source_note")
  )
})

test_that("add_glance_source_note(text_interpret)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("source_note") |>
      attr("text_interpret"),
    "html"
  )
})

test_that("add_glance_source_note(sep1,sep2)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_source_note(include = 1:3, sep1 = "==", sep2 = " | ") |>
      getElement("table_styling") |>
      getElement("source_note") ,
    "R²==0.000 | Adjusted R²==-0.005 | Sigma==14.3",
    ignore_attr = TRUE
  )
})


# add_glance_table() -----------------------------------------------------------
test_that("add_glance_table(x)", {
  expect_snapshot(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_table() |>
      as.data.frame()
  )
})

test_that("add_glance_table(include,label)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_table(include = r.squared, label = r.squared ~ "R * R") |>
      getElement("table_body") |>
      getElement("label") |>
      getElement(4L),
    "R * R"
  )
})

test_that("add_glance_table(fmt_fn)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_table(fmt_fun = ~label_style_sigfig(digits = 5), include = 1:3) |>
      as.data.frame(col_labels = FALSE) |>
      getElement("estimate") %>%
      `[`(4:6),
    c("0.00024", "-0.00511", "14.348")
  )
})

test_that("add_glance_table(glance_fun)", {
  expect_equal(
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_table(glance_fun = \(x, ...) broom::glance(x, ...) |> dplyr::select(1:3)) |>
      as.data.frame(),
    lm(age ~ trt, trial) |>
      tbl_regression() |>
      add_glance_table(include = 1:3) |>
      as.data.frame()
  )
})

test_that("add_glance_table(glance_fun) for mice models", {
  skip_if_not(is_pkg_installed("mice", reference_pkg = "gtsummary"))


  tbl <- mice::mice(mice::nhanes2, print = FALSE, maxit = 1) |>
    with(lm(bmi ~ age)) |>
    tbl_regression()
  glance <- tbl$inputs$x |>
    mice::pool() |>
    broom::glance() |>
    dplyr::mutate(
      across(c(nimp, nobs), label_style_number()),
      across(c(r.squared, adj.r.squared), label_style_number(digits = 3))
    ) |>
    as.list()

  expect_equal(
    tbl |>
      add_glance_table() |>
      as_tibble(col_labels = FALSE) |>
      getElement("estimate") %>%
      `[`(5:8),
    unlist(unname(glance))
  )

  expect_equal(
    tbl |>
      add_glance_source_note(
        label = names(glance) |> as.list() |> setNames(names(glance))
      ) |>
      getElement("table_styling") |>
      getElement("source_note"),
    imap(glance, ~paste0(.y, " = ", .x)) |> unlist() |> paste(collapse = "; "),
    ignore_attr = TRUE
  )
})

