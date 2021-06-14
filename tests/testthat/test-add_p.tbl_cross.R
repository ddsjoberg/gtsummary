# test-add_p.tbl_cross----------------------------------------------------------
skip_on_cran()

test_that("add_p.tbl_cross", {
  expect_error(
    trial %>% tbl_cross(response, death) %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(source_note = TRUE),
    NA
  )
  expect_error(
    tbl <-
      mtcars %>%
      tbl_cross(gear, carb) %>%
      add_p(test = "chisq.test",
            pvalue_fun = ~ifelse(is.na(.), NA, format(., digits = 2, scientific = TRUE))),
    NA
  )
  expect_equal(
    tbl %>%
      as_tibble(col_labels = FALSE) %>%
      slice(1) %>%
      pull(p.value),
    "8.6e-02"
  )
  expect_equal(
    tbl$meta_data$stat_test_lbl[1],
    "Pearson's Chi-squared test"
  )
})
