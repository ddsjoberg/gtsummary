skip_on_cran()
skip_if_not(broom.helpers::.assert_package("flextable", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("huxtable", pkg_search = "gtsummary", boolean = TRUE))
skip_if_not(broom.helpers::.assert_package("kableExtra", pkg_search = "gtsummary", boolean = TRUE))


test_that("no errors/warnings with all output types", {
  tbl <- trial %>%
    select(age) %>%
    tbl_summary() %>%
    modify_caption("test caption")

  expect_snapshot(tbl %>% as.data.frame())
  expect_error(tbl %>% as_gt(), NA)
  expect_error(tbl %>% as_flex_table(), NA)
  expect_error(tbl %>% as_hux_table(), NA)
  expect_error(tbl %>% as_kable(), NA)
  expect_error(tbl %>% as_kable_extra(), NA)
  expect_error(tbl %>% as_tibble(), NA)

  tbl2 <- trial %>%
    select(age) %>%
    tbl_summary() %>%
    modify_caption("test caption", text_interpret = "html")
  expect_snapshot(tbl2 %>% as.data.frame())

  tbl_reg <- lm(mpg ~ hp, mtcars) %>% tbl_regression()
  expect_error(modify_caption(trial), "*")
  expect_error(tbl_reg %>% modify_caption(letters), "*")
  expect_equal(
    tbl_reg %>% modify_caption("{N}") %>% purrr::pluck("table_styling", "caption"),
    "32",
    ignore_attr = TRUE
  )

  expect_error(
    tbl_with_caption <-
      trial %>%
      select(age) %>%
      tbl_summary() %>%
      modify_table_body(~ rename(.x, label2 = label)) %>%
      modify_column_unhide(label2) %>%
      modify_caption("captions are great"),
    NA
  )
  expect_snapshot(tbl_with_caption %>% as.data.frame())
})
