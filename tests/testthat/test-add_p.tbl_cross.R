skip_on_cran()
skip_if_not(is_pkg_installed("cardx"))

test_that("add_p.tbl_cross() works", {
  expect_silent(
    out <- trial |>
      tbl_cross(row = stage, col = trt) |>
      add_p()
  )
  expect_equal(round(out$table_body$p.value[1], 4), 0.8662)
})

test_that("add_p.tbl_cross(source_note) works", {
  expect_silent(
    out <- trial |>
      tbl_cross(row = stage, col = trt) |>
      add_p(source_note = TRUE)
  )
  expect_equal(round(out$table_body$p.value[1], 4), 0.8662)

  source_nt <- "Pearson's Chi-squared test, p=0.9"
  expect_equal(out$table_styling$source_note$source_note, source_nt)
})

test_that("add_p.tbl_cross(source_note) errors properly", {
  expect_snapshot(error = TRUE,
    tbl_cross(trial, row = stage, col = trt) |>
    add_p(source_note = NA)
  )
  expect_snapshot(error = TRUE,
    tbl_cross(trial, row = stage, col = trt) |>
    add_p(source_note = NULL)
  )
})
