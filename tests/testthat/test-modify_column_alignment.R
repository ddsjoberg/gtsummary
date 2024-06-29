test_that("modify_column_alignment() works", {
  tbl <-
    lm(age ~ marker + grade, trial) |>
    tbl_regression()

  expect_equal(
    map_chr(
      c("left", "right", "center"),
      ~ tbl |>
        modify_column_alignment(columns = everything(), align = .x) |>
        getElement("table_styling") |>
        getElement("header") |>
        dplyr::pull("align") |>
        unique()
    ),
    c("left", "right", "center")
  )
})

