skip_if_not(is_pkg_installed("cardx", reference_pkg = "gtsummary"))
skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("separate_p_footnotes() works", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = c(age, grade)
    )

  # not a perfect snapshot, because it doesn't include row numbers, but it's close
  expect_snapshot(
    add_p(tbl) |>
      separate_p_footnotes() |>
      getElement("table_styling") |>
      getElement("footnote")
  )
})
