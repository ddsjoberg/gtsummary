skip_on_cran()
skip_if_not(is_pkg_installed(c("cardx", "broom.helpers")))

test_that("tbl_butcher()", {
  tbl <- trial |>
    tbl_uvregression(y = age, method = lm)

  expect_true(
    object.size(tbl) > object.size(tbl_butcher(tbl))
  )

  expect_equal(
    tbl_butcher(tbl, "inputs") |> names(),
    c("table_body", "table_styling", "inputs")
  )
})
