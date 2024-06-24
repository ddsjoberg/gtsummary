test_that("add_stat for tbl_continuous() works", {
  tt <-
    trial %>%
    tbl_continuous(
      age,
      include = grade,
      by = trt
    )

  add_stat_test1 <- function(data, variable, by, ...) {
    tibble::tibble(test_col = "Ugh")
  }

  add_stat_test2 <- function(data, variable, by, ...) {
    tibble::tibble(test_col = rep_len("Ugh", 3))
  }

  expect_error(
    tt %>%
      add_stat(fns = everything() ~ add_stat_test1),
    NA
  )
  expect_error(
    tt %>%
      add_stat(fns = everything() ~ add_stat_test2, location = everything() ~ "level"),
    NA
  )
})
