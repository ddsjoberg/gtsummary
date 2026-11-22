skip_on_cran()
skip_if_pkg_not_installed("survey")

svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)

test_that("tbl_svycontinuous(data)", {
  # testing a typical case
  expect_silent(
    tbl <- tbl_svycontinuous(svy_trial, variable = marker, include = c(grade, response), by = trt)
  )
  expect_snapshot(tbl |> as.data.frame())
  expect_s3_class(tbl, c("tbl_svycontinuous", "gtsummary"))

  # pass something that is not a survey object
  expect_snapshot(error = TRUE, tbl_svycontinuous(trial, variable = age, include = grade))
})

test_that("tbl_svycontinuous() matches tbl_ard_continuous() with unit weights", {
  expect_equal(
    tbl_svycontinuous(svy_trial, variable = age, by = trt, include = grade, digits = ~1) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-label),
    svy_trial |>
      cardx::ard_summary(by = c(trt, grade), variables = age) |>
      tbl_ard_continuous(by = "trt", variable = "age", include = "grade") |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::select(-label)
  )
})

test_that("tbl_svycontinuous() mean/sd matches tbl_continuous() with unit weights", {
  expect_equal(
    tbl_svycontinuous(svy_trial, variable = age, by = trt, include = grade, statistic = ~"{mean} ({sd})") |>
      as.data.frame(col_labels = FALSE),
    tbl_continuous(trial, variable = age, by = trt, include = grade, statistic = ~"{mean} ({sd})") |>
      as.data.frame(col_labels = FALSE)
  )
})

test_that("tbl_svycontinuous(variable)", {
  # not passing a variable
  expect_error(tbl_svycontinuous(svy_trial, include = grade))

  # passing more than one variable
  expect_error(tbl_svycontinuous(svy_trial, variable = c(age, marker), include = grade))
})

test_that("tbl_svycontinuous(by)", {
  # passing more than one by variable
  expect_snapshot(
    error = TRUE,
    tbl_svycontinuous(svy_trial, variable = age, include = grade, by = c(trt, response))
  )

  # observations missing the by variable are removed with a message
  expect_snapshot(
    tbl_svycontinuous(svy_trial, variable = age, include = grade, by = response) |>
      as.data.frame()
  )
})

test_that("tbl_svycontinuous(statistic)", {
  # missingness statistics are available
  expect_silent(
    tbl <- tbl_svycontinuous(
      svy_trial,
      variable = age,
      include = grade,
      statistic = ~"{mean} ({mean.std.error}); {N_nonmiss}/{N_obs}"
    )
  )
  expect_snapshot(tbl |> as.data.frame())

  # requesting only missingness statistics
  expect_silent(
    tbl_svycontinuous(svy_trial, variable = age, include = grade, statistic = ~"{N_miss}")
  )

  # requesting a statistic that does not exist
  expect_snapshot(
    error = TRUE,
    tbl_svycontinuous(svy_trial, variable = age, include = grade, statistic = ~"{not_a_stat}")
  )

  # passing a non-glue string
  expect_error(
    tbl_svycontinuous(svy_trial, variable = age, include = grade, statistic = ~"mean")
  )
})

test_that("tbl_svycontinuous(value, label)", {
  expect_silent(
    tbl <-
      svy_trial |>
      tbl_svycontinuous(
        variable = age,
        include = c(grade, response),
        value = response ~ 1,
        label = list(response = "Tumor Response", age = "Patient Age")
      )
  )
  expect_snapshot(tbl |> as.data.frame())
  expect_equal(
    tbl$table_body |> dplyr::filter(variable == "response") |> nrow(),
    1L
  )
  # the continuous variable label is prepended to the header footnote
  expect_true(
    all(startsWith(tbl$table_styling$footnote_header$footnote, "Patient Age: "))
  )

  # value that is not a level of the variable
  expect_error(
    tbl_svycontinuous(svy_trial, variable = age, include = response, value = response ~ 5)
  )
})

test_that("tbl_svycontinuous() with a complex design", {
  data(api, package = "survey")
  svy_api <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_silent(
    tbl <- tbl_svycontinuous(
      svy_api,
      variable = api00,
      include = c(stype, awards),
      by = both,
      statistic = ~"{mean} ({sd})"
    )
  )
  expect_snapshot(tbl |> as.data.frame())
})

test_that("tbl_svycontinuous() works with add_overall(), add_stat(), and inline_text()", {
  tbl <- tbl_svycontinuous(svy_trial, variable = age, by = trt, include = grade)

  # add_overall() matches the unstratified table
  expect_silent(tbl_overall <- add_overall(tbl))
  expect_equal(
    tbl_overall$table_body$stat_0,
    tbl_svycontinuous(svy_trial, variable = age, include = grade)$table_body$stat_0
  )

  # inline_text()
  expect_equal(
    inline_text(tbl, variable = grade, level = "I", column = "Drug A"),
    tbl$table_body |>
      dplyr::filter(variable == "grade", label == "I") |>
      dplyr::pull(stat_1)
  )

  # add_stat()
  expect_silent(
    tbl_stat <-
      add_stat(
        tbl,
        fns = everything() ~ \(data, variable, by, ...) {
          tibble::tibble(n_rows = nrow(data$variables))
        }
      )
  )
  expect_true("n_rows" %in% names(tbl_stat$table_body))
})
