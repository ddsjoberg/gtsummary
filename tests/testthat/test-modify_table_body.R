skip_on_cran()

test_that("modify_table_body with standard use", {
  tbl0 <-
    trial %>%
    select(response, age, marker) %>%
    tbl_uvregression(
      y = response,
      method = glm,
      method.args = list(family = binomial),
      exponentiate = TRUE,
      hide_n = TRUE
    )

  expect_error(
    t1 <-
      tbl0 %>%
      # adding number of non-events to table
      modify_table_body(dplyr::mutate, n_nonevent = N - nevent) %>%
      modify_table_body(dplyr::relocate, n_nonevent, .before = nevent) %>%
      modify_header(n_nonevent = "**Control N**", nevent = "**Case N**"),
    NA
  )
  expect_snapshot(t1 %>% as.data.frame())

  expect_equal(
    t1[["table_body"]],
    tbl0 %>%
      modify_table_body(
        ~ mutate(.x, n_nonevent = N - nevent) %>% dplyr::relocate(n_nonevent, .before = nevent)
      ) %>%
      modify_header(n_nonevent = "**Control N**", nevent = "**Case N**") %>%
      purrr::pluck("table_body")
  )
})
