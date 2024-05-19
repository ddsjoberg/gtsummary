test_that("modify_table_body() works", {
  expect_snapshot(
    glm(response ~ trt + marker, trial, family = binomial) |>
      tbl_regression() |>
      modify_column_hide(c("conf.low", "conf.high")) |>
      # adding number of non-events to table
      modify_table_body(~.x |> dplyr::mutate(n_nonevent = N - nevent)) |>
      modify_table_body(dplyr::relocate, nevent, n_nonevent, .after = label) |>
      modify_header(n_nonevent = "**Control N**", nevent = "**Case N**") |>
      as.data.frame()
  )
})

test_that("modify_table_body() messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_table_body(~stop("I made an error."))
  )
})
