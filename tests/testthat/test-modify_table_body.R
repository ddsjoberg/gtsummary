context("test-modify_table_body")
testthat::skip_on_cran()

test_that("modify_table_body with standard use", {
  expect_error(
    t1 <- trial %>%
      select(response, age, marker) %>%
      tbl_uvregression(
        y = response,
        method = glm,
        method.args = list(family = binomial),
        exponentiate = TRUE,
        hide_n = TRUE
      ) %>%
      add_nevent() %>%
      # adding number of non-events to table
      modify_table_body(dplyr::mutate, n_nonevent = N - nevent) %>%
      modify_table_body(dplyr::relocate, n_nonevent, .before = nevent) %>%
      modify_header(n_nonevent = "**Control N**", nevent = "**Case N**"),
    NA
  )
})
