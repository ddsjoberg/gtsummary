skip_on_cran()
skip_if_not(broom.helpers::.assert_package("lme4", pkg_search = "gtsummary", boolean = TRUE))

test_that("add_glance_source_note: no errors/warnings with standard use", {
  tbl1 <-
    lm(age ~ marker + grade, trial) %>%
    tbl_regression(pvalue_fun = purrr::partial(style_pvalue, digits = 3))
  tbl2 <-
    lme4::lmer(age ~ marker + (1 | grade), trial) %>%
    tbl_regression()

  expect_error(
    res <-
      tbl1 %>%
      add_glance_source_note(
        label = list(df ~ "Degrees of Freedom", sigma ~ "\U03C3"),
        fmt_fun = df ~ style_number,
        include = c(r.squared, AIC, sigma, df, nobs)
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    tbl_glance <-
      tbl1 %>%
      add_glance_table(
        label = list(df ~ "Degrees of Freedom", sigma ~ "\U03C3"),
        fmt_fun = df ~ style_number,
        include = c(r.squared, AIC, sigma, df, nobs, p.value)
      ),
    NA
  )
  expect_snapshot(tbl_glance %>% as.data.frame())

  expect_equal(
    tbl_glance %>%
      modify_column_unhide(everything()) %>%
      as_tibble(col_labels = FALSE) %>%
      filter(row_type == "glance_statistic") %>%
      pull(estimate),
    c("0.005", "1,473", "14.6", "3", "179", "0.832")
  )

  expect_error(
    res <-
      tbl2 %>%
      add_glance_source_note(
        glance_fun = broom.mixed::glance
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <-
      tbl1 %>%
      add_glance_table(
        label = list(df ~ "Degrees of Freedom", sigma ~ "\U03C3"),
        fmt_fun = df ~ style_number,
        include = c(r.squared, AIC, sigma, df)
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <-
      tbl2 %>%
      add_glance_table(
        glance_fun = broom.mixed::glance
      ),
    NA
  )
  expect_snapshot(res %>% as.data.frame())
})
