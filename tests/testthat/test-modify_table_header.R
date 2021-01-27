context("test-modify_table_header")
testthat::skip_on_cran()

test_that("modify_table_header works properly", {
  tbl_r <- lm(mpg ~ factor(cyl), mtcars) %>% tbl_regression()

  expect_error(
    updated <-
      modify_table_header(tbl_r,
        column = estimate,
        label = "estimate test",
        fmt_fun = function(x) style_sigfig(x, digits = 5),
        footnote_abbrev = "test = unit test",
        footnote = "this is a footnote"
      ) %>%
      modify_table_header(
        column = "p.value",
        hide = TRUE
      ),
    NA
  )

  expect_equivalent(
    dplyr::filter(updated$table_header, column == "estimate") %>% pull(label),
    "estimate test"
  )

  expect_equivalent(
    dplyr::filter(updated$table_header, column == "estimate") %>% pull(fmt_fun),
    list(function(x) style_sigfig(x, digits = 5))
  )

  expect_equivalent(
    dplyr::filter(updated$table_header, column == "estimate") %>% pull(footnote_abbrev),
    "test = unit test"
  )

  expect_equivalent(
    dplyr::filter(updated$table_header, column == "estimate") %>% pull(footnote),
    "this is a footnote"
  )

  expect_equivalent(
    dplyr::filter(updated$table_header, column == "p.value") %>% pull(hide),
    TRUE
  )
})
