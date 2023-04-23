# test-add_p.tbl_cross----------------------------------------------------------
skip_on_cran()

test_that("add_p.tbl_cross", {
  expect_snapshot(
    trial %>% tbl_cross(response, death) %>% add_p() %>% as.data.frame()
  )
  expect_snapshot(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p() %>% as.data.frame()
  )
  expect_snapshot(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(source_note = TRUE) %>% as.data.frame()
  )
  expect_error(
    tbl <-
      mtcars %>%
      tbl_cross(gear, carb) %>%
      add_p(
        test = "chisq.test",
        pvalue_fun = ~ ifelse(is.na(.), NA, format(., digits = 2, scientific = TRUE))
      ),
    NA
  )
  expect_snapshot(tbl %>% as.data.frame())
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


  # oddly, the p-value is NA in this case, mcnemar.test() doesn't error
  df <-
    tibble::tibble(
      before = trial$response,
      after = rev(trial$response)
    )

  expect_error(
    {
      theme_gtsummary_language("pt")
      tbl_pt <-
        df %>%
        tbl_cross() %>%
        add_p(test = "mcnemar.test.wide", test.args = list(correct = FALSE))
      reset_gtsummary_theme()
    },
    NA
  )
  expect_snapshot(tbl_pt %>% as.data.frame())
  expect_equal(
    tbl_pt$table_styling$footnote %>%
      dplyr::filter(column == "p.value") %>%
      purrr::pluck("footnote", 1),
    "Teste de McNemar"
  )

  expect_error(
    {
      theme_gtsummary_language("pt")
      tbl_pt <-
        df %>%
        tbl_cross() %>%
        add_p(
          test = "mcnemar.test.wide", test.args = list(correct = FALSE),
          source_note = TRUE
        )
      reset_gtsummary_theme()
    },
    NA
  )
  expect_snapshot(tbl_pt %>% as.data.frame())
  expect_equal(
    tbl_pt$table_styling$source_note,
    "Teste de McNemar, ",
    ignore_attr = TRUE
  )
})
