skip_on_cran()
skip_if_not(requireNamespace("Hmisc"))

test_that("add_prop_ci() works", {
  expect_error(
    tbl1 <-
      trial %>%
      select(response, trt) %>%
      tbl_summary(by= trt, missing = "no") %>%
      add_p() %>%
      add_prop_ci() %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    names(tbl1),
    c("label", "stat_1", "prop_ci_stat_1", "stat_2", "prop_ci_stat_2", "p.value")
  )
  expect_equal(
    tbl1 %>% select(starts_with("prop_ci_stat_")) %>% as.list(),
    list(prop_ci_stat_1 = "21%, 40%", prop_ci_stat_2 = "25%, 44%")
  )


  expect_error(
    tbl2 <-
      trial %>%
      select(response, trt) %>%
      tbl_summary(missing = "no") %>%
      add_prop_ci(
        method = "exact",
        pattern = "{conf.low} to {conf.high}"
      ) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    names(tbl2),
    c("label", "stat_0", "prop_ci_stat_0")
  )
  expect_equal(
    tbl2$prop_ci_stat_0,
    c("25 to 39", NA, "42 to 56", "44 to 58")
  )
})

test_that("add_prop_ci() throws errors with bad arguments", {
  tbl0 <-
    trial %>%
    select(response, trt) %>%
    tbl_summary(missing = "no")

  expect_error(
    tbl0 %>%
      add_prop_ci(
        ci_fun = letters
      )
  )
  expect_error(
    tbl0 %>%
      add_prop_ci(
        pattern = letters
      )
  )
  expect_error(
    tbl0 %>%
      add_prop_ci(
        method = "letters"
      )
  )
  expect_error(
    add_prop_ci(x = letters)
  )
})
