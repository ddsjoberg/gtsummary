test_that("add_stat() for 'tbl_continuous'", {
  tt <-
    trial |>
    tbl_continuous(
      age,
      include = grade,
      by = trt
    )

  add_stat_test1 <- function(data, variable, by, ...) {
    tibble::tibble(addtl = "Data from elsewhere")
  }

  expect_equal(
    tt |>
      add_stat(fns = everything() ~ add_stat_test1) %>%
      as_tibble() %>%
      dplyr::pull(addtl),
    c("Data from elsewhere", NA, NA, NA)
  )
})


test_that("add_stat(location) for 'tbl_continuous'", {
  tt <-
    trial %>%
    tbl_continuous(
      age,
      include = grade,
      by = trt
    )

  p_vals <- lapply(
    dplyr::group_split(trial, grade),
    function(x) t.test(x$age ~ x$trt)$p.value
  ) %>%
    unlist %>%
    round(3) %>%
    as.character

  add_stat_test2 <- function(data, variable, by, tbl, ...) {
    col_name <- tbl$inputs$include
    col_sym <- sym(col_name)

    strat <- dplyr::group_split(data, !!col_sym)

    lapply(strat, function(x) t.test(x$age ~ x$trt)$p.value) %>% unlist
  }

  expect_equal(
    tt %>%
      add_stat(fns = everything() ~ add_stat_test2, location = everything() ~ "level") %>%
      as_tibble() %>%
      dplyr::pull(add_stat_1),
    c(NA, p_vals)
  )
})
