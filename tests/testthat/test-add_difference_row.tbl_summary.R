skip_on_cran()
skip_if_pkg_not_installed(
  c("broom", "broom.helpers", "lme4", "smd", "effectsize", "emmeans"),
  ref = "cardx"
)

test_that("add_difference_row() works", {
  expect_silent(
    tbl <- trial |>
      tbl_summary(by = grade, include = c(age, response), missing = "no") |>
      add_difference_row(
        reference = "I",
        statistic = everything() ~ c("{estimate}", "{conf.low}, {conf.high}", "{p.value}")
      )
  )
  expect_snapshot(as.data.frame(as.data.frame(tbl)))

  # check results in ARD
  # Age I vs III
  expect_equal(
    tbl$cards$add_difference_row$age$`'I' vs. 'III'` |>
      dplyr::select(-c("stat_fmt", "fmt_fun")),
    trial |>
      dplyr::filter(grade != "II") |>
      cardx::ard_stats_t_test(variables = "age", by = "grade") |>
      dplyr::select(-"fmt_fun"),
    ignore_attr = TRUE
  )
  # Response I vs III
  expect_equal(
    tbl$cards$add_difference_row$response$`'I' vs. 'III'` |>
      dplyr::select(-c("stat_fmt", "fmt_fun")),
    trial |>
      dplyr::filter(grade != "II") |>
      cardx::ard_stats_prop_test(variables = "response", by = "grade") |>
      cards::replace_null_statistic() |>
      dplyr::select(-"fmt_fun"),
    ignore_attr = TRUE
  )
})

test_that("add_difference_row(reference) messaging", {
  # bad input to `reference` arg
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = grade, include = c(age, response), missing = "no") |>
      add_difference_row(reference = "XXX")
  )

  # passing factor
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_summary(by = grade, include = c(age, response), missing = "no") |>
      add_difference_row(reference = factor("I"))
  )
})

test_that("add_difference_row() messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = age) |>
      add_difference_row(reference = "I")
  )

  expect_snapshot(
    tbl <- tbl_summary(trial, by = grade, include = response, percent = "row") |>
      add_difference_row(reference = "I")
  )
})

test_that("add_difference_row(test)", {
  # works with a custom test
  expect_equal(
    trial |>
      tbl_summary(by = grade, include = age, missing = "no") |>
      add_difference_row(
        reference = "I",
        statistic = everything() ~ c("{estimate}", "{conf.low}, {conf.high}", "{p.value}"),
        test = age ~ \(data, variable, by, ...) t.test(reformulate(by, variable), data = data) |> broom::tidy()
      ) |>
      getElement("cards") |>
      getElement("add_difference_row") |>
      getElement("age") |>
      getElement("'I' vs. 'III'") |>
      dplyr::select(-c("stat_fmt", "fmt_fun", "context", "stat_label")) |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")),
    trial |>
      dplyr::filter(grade != "II") |>
      cardx::ard_stats_t_test(variables = "age", by = "grade") |>
      dplyr::select(-c("fmt_fun", "context", "stat_label")) |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")),
    ignore_attr = TRUE
  )
})

test_that("add_difference_row(test) messaging", {
  # errors when the test return is in the wrong format
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, by = trt, include = age) |>
      add_difference_row(reference = "Drug A", test = age ~ \(...) letters)
  )

  # runs ok when test errors
  expect_snapshot(
    tbl <- tbl_summary(trial, by = trt, include = age) |>
      add_difference_row(reference = "Drug A", test = age ~ \(...) stop("oy!"))
  )
})

test_that("add_difference_row.tbl_summary(group)", {
  trial_group <- trial |>
    dplyr::mutate(.by = trt, id = dplyr::row_number())

  tbl_groups <-
    trial_group |>
    select(trt, id, stage, marker, age) %>%
    tbl_summary(
      by = trt,
      missing = "no",
      include = "age"
    ) |>
    add_difference_row(
      test = list(age = "ancova_lme4"),
      group = "id",
      reference = "Drug A"
    )
  expect_snapshot(as.data.frame(tbl_groups))

  expect_equal(
    tbl_groups$cards$add_difference$age$`'Drug A' vs. 'Drug B'` |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value")) |>
      tidyr::pivot_wider(
        id_cols = c(variable),
        names_from = stat_name,
        values_from = stat,
        values_fn = unlist
      ) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    withr::with_package(
      package = "lme4",
      lmer(age ~ fct_rev(factor(trt)) + (1 | id), trial_group)
    ) %>%
      {withr::with_package("broom.mixed", tidy(., conf.int = TRUE, effects = "fixed"))} |> # styler: off
      dplyr::slice(dplyr::n()) |>
      select(any_of(c("estimate", "conf.low", "conf.high", "p.value"))),
    ignore_attr = TRUE
  )
})
