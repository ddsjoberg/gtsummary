skip_on_cran()
skip_if_not(is_pkg_installed("withr"))

test_that("label_style_*()", {
  expect_equal(
    withr::with_options(
      list("OutDec" = ","),
      c(
        label_style_number(digits = 1, prefix = "$", suffix = "*")(1000),
        label_style_sigfig(digits = 1, prefix = "$", suffix = "*")(1000),
        label_style_ratio(digits = 1, prefix = "$", suffix = "*")(1000),
        label_style_pvalue(digits = 2)(0.236),
        label_style_percent(digits = 2, prefix = "$", suffix = "*")(10)
      )
    ),
    c("$1 000,0*", "$1 000*", "$1 000*", "0,24", "$1 000,00*")
  )
})
