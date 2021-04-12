# test-add_p.tbl_cross----------------------------------------------------------
skip_on_cran()

test_that("add_p.tbl_cross", {
  expect_error(
    trial %>% tbl_cross(response, death) %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(),
    NA
  )
  expect_error(
    trial[c("trt", "grade")] %>% tbl_cross() %>% add_p(source_note = TRUE),
    NA
  )
  expect_error(
    mtcars %>%
      tbl_cross(gear, carb) %>%
      add_p(test = "fisher.test"),
    NA
  )
})
