skip_on_cran()

test_that("modify_spanning_header works", {
  tbl1 <-
    trial %>%
    dplyr::select(trt, age, grade) %>%
    tbl_summary(by = trt)

  expect_snapshot(
    tbl1 %>%
      modify_spanning_header(starts_with("stat_") ~ "**Randomization Assignment**") %>%
      as.data.frame()
  )

  expect_snapshot(
    tbl1 %>%
      modify_spanning_header(
        label = "Variables", starts_with("stat_") ~ "**Randomization Assignment**"
      ) %>%
      as.data.frame()
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
})
