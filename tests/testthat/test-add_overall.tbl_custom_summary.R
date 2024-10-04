test_that("add_overall.tbl_custom_summary() works", {
  my_stats <- function(data, ...) {
    dplyr::tibble(
      marker_sum = sum(data$marker, na.rm = TRUE)
    )
  }
  expect_snapshot(
    trial |>
      tbl_custom_summary(
        include = c("stage", "grade"),
        by = "trt",
        stat_fns = everything() ~ my_stats,
        type = everything() ~ "continuous2", #new
        statistic = everything() ~ "S: {marker_sum}"
      ) |>
      add_overall() |>
      as.data.frame()
  )
})
