skip_on_cran()
skip_if_not(requireNamespace("lme4"))

test_that("add_glance_source_note: no errors/warnings with standard use", {
  tbl1 <-
    lm(age ~ marker + grade, trial) %>%
    tbl_regression()
  tbl2 <-
    lme4::lmer(age ~ marker + (1 | grade), trial) %>%
    tbl_regression()

  expect_error(
    tbl1 %>%
      add_glance_source_note(
        label = list(df ~ "Degrees of Freedom", sigma ~ "\U03C3"),
        fmt_fun = df ~ style_number,
        include = c(r.squared, AIC, sigma, df)
      ),
    NA
  )

  expect_error(
    tbl2 %>%
      add_glance_source_note(
        glance_fun = broom.mixed::glance
      ),
    NA
  )

  expect_error(
    tbl1 %>%
      add_glance_table(
        label = list(df ~ "Degrees of Freedom", sigma ~ "\U03C3"),
        fmt_fun = df ~ style_number,
        include = c(r.squared, AIC, sigma, df)
      ),
    NA
  )

  expect_error(
    tbl2 %>%
      add_glance_table(
        glance_fun = broom.mixed::glance
      ),
    NA
  )
})
