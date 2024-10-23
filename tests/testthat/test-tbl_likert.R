skip_on_cran()
skip_if_not(is_pkg_installed("withr"))

levels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
df_likert <-
  withr::with_seed(
    seed = 11235,
    data.frame(
      recommend_friend = sample(levels, size = 20, replace = TRUE) |> factor(levels = levels),
      regret_purchase = sample(levels, size = 20, replace = TRUE) |> factor(levels = levels)
    )
  )

test_that("tbl_likert(data)", {
  # standard use works well
  expect_snapshot(
    df_likert |>
      tbl_likert() |>
      add_n() |>
      as.data.frame()
  )

  # errors with bad input
  expect_error(
    tbl_likert(letters),
    "The `data` argument must be class"
  )
})

test_that("tbl_likert(statistic)", {
  # standard use works well
  expect_snapshot(
    df_likert |>
      tbl_likert(statistic = ~"{n} / {N} ({p}%)") |>
      as.data.frame()
  )

  # errors with bad inputs
  expect_error(
    df_likert |>
      tbl_likert(statistic = ~letters),
    "Values pass in `statistic` argument must be strings"
  )

  # statistic doesn't have any glue syntax
  expect_error(
    df_likert |>
      tbl_likert(statistic = ~"n / N"),
    "The `statistic` argument string does not contain any glue element"
  )

  # statistic has stats that are not available
  expect_error(
    df_likert |>
      tbl_likert(statistic = ~"{n} ({sd})"),
    "are not valid"
  )
})

test_that("tbl_likert(label)", {
  expect_equal(
    df_likert |>
      tbl_likert(label = list(recommend_friend = "I Would Recommend to a Friend")) |>
      getElement("table_body") |>
      getElement("label") |>
      head(1L),
    "I Would Recommend to a Friend"
  )

  expect_error(
    df_likert |>
      tbl_likert(label = list(recommend_friend = letters)),
    "Values pass in `label` argument must be strings."
  )
})


test_that("tbl_likert(digits)", {
  # standard use works well
  expect_snapshot(
    df_likert |>
      tbl_likert(digits = ~list(p = label_style_sigfig(digits = 3, scale = 100))) |>
      as.data.frame()
  )

  # errors with bad inputs
  expect_error(
    df_likert |>
      tbl_likert(digits = ~letters),
    "Error in `digits` argument for variable"
  )
})

test_that("tbl_likert(include)", {
  expect_error(
    mtcars |>
      tbl_likert(include = mpg),
    "All variables in the `include` argument must be"
  )

  expect_error(
    df_likert |>
      dplyr::mutate(
        bad_fct = recommend_friend |> fct_expand("anoter_level")
      ) |>
      tbl_likert(include = everything()),
    "All variables in the `include` argument must have the same factor levels"
  )
})

test_that("tbl_likert(sort)", {
  expect_snapshot(
    df_likert |>
      tbl_likert(sort = "descending") |>
      as.data.frame()
  )
})
