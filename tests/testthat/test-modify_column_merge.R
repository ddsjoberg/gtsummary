test_that("modify_column_merge() works", {
  mod <- lm(marker ~ age + grade, trial) %>% tbl_regression()
  expect_error(
    tbl <-
      mod %>%
      modify_column_merge(
        pattern = "{estimate} ({ci})",
        rows = !is.na(estimate)
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())

  expect_equal(
    as_tibble(tbl, col_labels = FALSE) %>%
      dplyr::pull(estimate),
    c(
      "0.00 (-0.01, 0.01)", NA, NA,
      "-0.38 (-0.69, -0.07)", "-0.12 (-0.43, 0.19)"
    )
  )

  expect_error(
    mod %>%
      modify_column_merge(
        pattern = "{not_a_column} ({ci})",
        rows = !is.na(estimate)
      )
  )

  expect_error(
    mod %>%
      modify_column_merge(
        pattern = "no columns selected",
        rows = !is.na(estimate)
      )
  )
})
