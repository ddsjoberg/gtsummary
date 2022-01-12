skip_on_cran()

test_that("modify_spanning_header works", {
  tbl1 <-
    trial %>%
    dplyr::select(trt, age, grade) %>%
    tbl_summary(by = trt)

  expect_error(
    tbl1 %>%
      modify_spanning_header(starts_with("stat_") ~ "**Randomization Assignment**"),
    NA
  )

  expect_error(
    tbl1 %>%
      modify_spanning_header(label = "Variables", starts_with("stat_") ~ "**Randomization Assignment**"),
    NA
  )

  expect_error(
    trial %>%
      dplyr::select(trt, age, grade) %>%
      tbl_summary(by = trt) %>%
      modify_spanning_header(),
    NA
  )

  expect_true(
    tbl1 %>%
      modify_spanning_header(everything() ~ NA) %>%
      purrr::pluck("table_styling", "header", "spanning_header") %>%
      is.na() %>%
      all()
  )

  expect_error(
    tbl <-
      trial %>%
      tbl_summary(by = trt, missing = "no", include = c(age, grade)) %>%
      tbl_butcher() %>%
      modify_spanning_header(all_stat_cols() ~ "ooof") %>%
      modify_header(all_stat_cols() ~ "ooof"),
    NA
  )

  expect_equal(
    tbl$table_styling$header %>%
      dplyr::filter(column == "stat_1") %>%
      select(label, spanning_header) %>%
      unlist(),
    c(label = "ooof", spanning_header = "ooof")
  )

})
