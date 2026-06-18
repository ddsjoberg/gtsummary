skip_on_cran()
gtsummary:::skip_if_pkg_not_installed(c("broom", "smd", "withr"))

test_that("separate_p_footnotes()", {
  withr::local_options(list(width = 130))
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = c(age, grade)
    )

  # not a perfect snapshot, because it doesn't include row numbers, but it's close
  expect_snapshot(
    add_p(tbl, test = list(age = \(data, variable, by, ...) t.test(data[[variable]] ~ data[[by]]) |> broom::tidy())) |>
      separate_p_footnotes() |>
      getElement("table_styling") |>
      getElement("footnote_body") |>
      dplyr::filter(dplyr::row_number() %in% c(dplyr::n(), dplyr::n() - 1L)) |>
      dplyr::mutate(rows = purrr::map_chr(rows, ~quo_squash(.x) |> expr_deparse())) |>
      as.data.frame()
  )

  expect_snapshot(
    add_difference(tbl) |>
      separate_p_footnotes() |>
      getElement("table_styling") |>
      getElement("footnote_body") |>
      dplyr::filter(dplyr::row_number() %in% seq(dplyr::n(), dplyr::n() - 4L)) |>
      dplyr::mutate(rows = purrr::map_chr(rows, ~quo_squash(.x) |> expr_deparse())) |>
      as.data.frame()
  )
})

test_that("separate_p_footnotes() messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, by = trt, include = grade) |>
      add_difference() |>
      add_p() |>
      separate_p_footnotes()
  )
})


test_that("separate_p_footnotes() translates footnotes", {
  footnotes <-
    with_gtsummary_theme(
      theme_gtsummary_language("es"),
      expr = trial |>
        tbl_summary(by = trt, include = c(age, grade)) |>
        add_p() |>
        separate_p_footnotes() |>
        getElement("table_styling") |>
        getElement("footnote_body") |>
        dplyr::pull("footnote")
    )

  # footnotes must contain Spanish translations, not English originals
  expect_true(any(grepl("prueba chi cuadrado", footnotes, fixed = TRUE)))
  expect_false(any(grepl("Pearson's Chi-squared test", footnotes, fixed = TRUE)))
  expect_false(any(grepl("Wilcoxon rank sum test", footnotes, fixed = TRUE)))
})

# adding test against a `tbl_svysummary()` object
test_that("separate_p_footnotes() with tbl_svysummary()", {
  gtsummary:::skip_if_pkg_not_installed("survey")

  expect_error(
    survey::svydesign(~1, data = trial, weights = ~1) |>
      tbl_svysummary(by = trt, include = c(age, grade)) |>
      add_p() |>
      separate_p_footnotes(),
    NA
  )
})
