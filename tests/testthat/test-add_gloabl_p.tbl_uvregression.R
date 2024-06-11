test_that("add_n.tbl_regression() works", {
  tbl <- tbl_uvregression(
        trial,
        method = glm,
        include = c(trt, grade, age),
        y = response,
        method.args = list(family = binomial),
        exponentiate = TRUE
    )

  # total N added to table is accurate
  expect_error(
    res <- tbl |> add_global_p(),
    NA
  )

  expect_snapshot(
    res |> as.data.frame()
  )
})


test_that("no errors/warnings with standard use after tbl_uvregression", {
  tbl2 <- trial %>% tbl_uvregression(method = lm, y = age)
  expect_error(
    res <- tbl2 %>% add_global_p(), NA
  )
  expect_warning(
    tbl2 %>% add_global_p(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl2 %>% add_global_p(type = 2, keep = TRUE), NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_warning(
    tbl2 %>% add_global_p(type = "II"), NA
  )
  expect_equal(
    lm(age ~ trt, trial) %>% car::Anova(type = "II") %>% select(last_col()) %>%
      dplyr::pull() %>% purrr::discard(is.na),
    tbl2 %>% add_global_p(type = "II", include = trt) %>% purrr::pluck("table_body", "p.value", 1)
  )

  expect_message(tbl2 %>% add_global_p(), NA)
})

skip_on_cran()
skip_if_not(broom.helpers::.assert_package("car", pkg_search = "gtsummary", boolean = TRUE))

test_that("no errors/warnings with standard use after tbl_uvregression", {
  tbl2 <- trial %>% tbl_uvregression(method = lm, y = age)
  expect_error(
    res <- tbl2 %>% add_global_p(), NA
  )
  expect_warning(
    tbl2 %>% add_global_p(), NA
  )
  expect_snapshot(res %>% as.data.frame())

  expect_error(
    res <- tbl2 %>% add_global_p(type = 2, keep = TRUE), NA
  )
  expect_snapshot(res %>% as.data.frame())
  expect_warning(
    tbl2 %>% add_global_p(type = "II"), NA
  )
  expect_equal(
    lm(age ~ trt, trial) %>% car::Anova(type = "II") %>% dplyr::select(last_col()) %>%
      dplyr::pull() %>% purrr::discard(is.na),
    tbl2 %>% add_global_p(type = "II", include = trt) %>% purrr::pluck("table_body", "p.value", 1)
  )

  expect_message(tbl2 %>% add_global_p(), NA)
})
