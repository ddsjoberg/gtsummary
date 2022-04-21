skip_on_cran()
skip_if_not(broom.helpers::.assert_package("Hmisc", pkg_search = "gtsummary", boolean = TRUE))

test_that("add_ci() works", {
  expect_error(
    tbl1 <-
      trial %>%
      select(response, age, trt) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_p() %>%
      add_ci(pattern = "{stat} ({ci})"),
    NA
  )
  expect_equal(
   as_tibble(tbl1) %>% names(),
   c("**Characteristic**", "**Drug A**, N = 98 (**95% CI**)",
     "**Drug B**, N = 102 (**95% CI**)", "**p-value**")
 )
  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% pull(stat_1),
    c("28 (29%) (21%, 40%)", "46 (37, 59) (44, 50)")
  )

  expect_error(
    tbl1 <-
      trial %>%
      select(response, age, trt) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_p() %>%
      add_ci() %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    names(tbl1),
    c("label", "stat_1", "ci_stat_1", "stat_2", "ci_stat_2", "p.value")
  )
  expect_equal(
    tbl1 %>% select(starts_with("ci_stat_")) %>% as.list(),
    list(ci_stat_1 = c("21%, 40%", "44, 50"),
         ci_stat_2 = c("25%, 44%", "45, 50"))
  )


  expect_error(
    tbl2 <-
      trial %>%
      select(response, trt) %>%
      tbl_summary(missing = "no") %>%
      add_ci(
        method = everything() ~ "exact",
        statistic = everything() ~  "{conf.low} to {conf.high}"
      ) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    names(tbl2),
    c("label", "stat_0", "ci_stat_0")
  )
  expect_equal(
    tbl2$ci_stat_0,
    c("25 to 39", NA, "42 to 56", "44 to 58")
  )
})

test_that("add_ci() throws errors with bad arguments", {
  tbl0 <-
    trial %>%
    select(response, trt) %>%
    tbl_summary(missing = "no")

  expect_error(
    tbl0 %>%
      add_ci(method = everything() ~ "t.test")
  )

  expect_error(
    tbl0 %>%
      add_ci(
        style_fun = letters
      )
  )
  expect_error(
    tbl0 %>%
      add_ci(
        statistic = letters
      )
  )
  expect_error(
    tbl0 %>%
      add_ci(
        method = "letters"
      )
  )
  expect_error(
    add_ci(x = letters)
  )
})
