context("test-add_glance_source_note")
testthat::skip_on_cran()


test_that("add_glance_source_note: no errors/warnings with standard use", {

  expect_error(
    lm(age ~ marker + grade, trial) %>%
      tbl_regression() %>%
      add_glance_source_note(
        label = list(df  ~ "Degrees of Freedom", sigma ~ "\U03C3"),
        fmt_fun = df ~ style_number,
        include = c(r.squared, AIC, sigma, df)
      ),
    NA
  )
})
