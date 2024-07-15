test_that("scope_header()", {
  tbl <- trial |>
    tbl_summary(by = trt, include = age, missing = "no")

  # add modify_selector information to header
  tbl$table_styling$header <-
    tbl$table_styling$header |>
    dplyr::mutate(
      modify_selector_by =
        dplyr::case_when(
          column == "stat_1" ~ "Drug A",
          column == "stat_2" ~ "Drug B"
        )
    )

  # create a fake selector to see if it works
  all_by_levels <- function(level) {
    where(\(x) isTRUE(attr(x, "gtsummary.by") %in% level))
  }

  expect_equal(
    scope_header(tbl$table_body, tbl$table_styling$header) |> dplyr::select(all_by_levels("Drug A")) |> unlist(),
    "46 (37, 60)",
    ignore_attr = TRUE
  )
})
