skip_on_cran()
skip_if_pkg_not_installed("withr")

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

  expect_equal(
    c(
      label_style_number(na = "NE", prefix = "$", suffix = "*")(NA),
      label_style_sigfig(na = "NE", prefix = "$", suffix = "*")(NA),
      label_style_ratio(na = "NE", prefix = "$", suffix = "*")(NA),
      label_style_pvalue(na = "NE", prefix = "$", suffix = "*")(NA),
      label_style_percent(na = "NE", prefix = "$", suffix = "*")(NA)
    ),
    c("NE", "NE", "NE", "NE", "NE")
  )
})
