skip_on_cran()

test_that("tbl_cross- throws error if both `col` and `row`` are not specified", {
  expect_error(
    tbl_cross(trial, col = trt),
    NULL
  )
  expect_error(
    tbl_cross(trial, row = trt),
    NULL
  )
})

test_that("tbl_cross- works if no `col` or `row` specified", {
  expect_error(
    tbl_cross(trial, col = trt, row = response),
    NA
  )
})


test_that("tbl_cross- works in character inputs for `col` and `row", {
  col_variable <- "trt"
  row_variable <- "response"
  expect_error(
    tbl_cross(trial,
      col = col_variable,
      row = row_variable
    ),
    NA
  )
})

test_that("tbl_cross- creates output without error with continuous args", {
  expect_error(
    tbl_cross(mtcars, row = gear, col = am),
    NA
  )
})


test_that("tbl_cross- returns errors with bad inputs", {
  expect_error(
    tbl_cross(tibble::tibble()),
    NULL
  )
  expect_error(
    tbl_cross(tibble::tibble(t = integer())),
    NULL
  )
  expect_error(
    tbl_cross(trial, col = THIS_IS_NOT_A_VARIABLE),
    NULL
  )
})

# Labels Argument ------------------------------------------------------------
test_that("tbl_cross- labels work", {
  expect_error(
    tbl_cross(mtcars, row = am, col = cyl, label = list(
      am = "AM LABEL",
      cyl = "New cyl"
    )),
    NA
  )
  expect_error(
    tbl_cross(mtcars,
      row = am, col = cyl,
      label = vars(am) ~ "AM LABEL"
    ),
    NA
  )
})

# Stats and Percent Argument ---------------------------------------------------
test_that("tbl_cross- statistics argument works", {
  expect_error(
    tbl_cross(trial, statistic = "{p}"),
    NA
  )
  expect_error(
    tbl_cross(trial, percent = "cell"),
    NA
  )
})

test_that("tbl_cross- passing percent without stat works and produces %", {
  expect_error(
    tbl_cross(trial, percent = "cell"),
    NA
  )
  x <- tbl_cross(trial, percent = "cell")
  expect_equal(
    sum(str_detect(x$table_body$stat_1, "%"), na.rm = TRUE) > 1,
    TRUE
  )
})

# Missing Argument -------------------------------------------------------------
test_that("tbl_cross- test 'no' missing throws message", {
  expect_message(
    x <- tbl_cross(trial,
      row = trt,
      col = response,
      missing = "no"
    ),
    NULL
  )
})

test_that("tbl_cross- test no missing omits all NAs", {
  x <- tbl_cross(trial,
    row = trt,
    col = response,
    missing = "no"
  )
  expect_equal(
    "Unknown" %in% x$table_body$label,
    FALSE
  )
})

test_that("tbl_cross- test ifany missing returns Unknown when missing", {
  x <- tbl_cross(trial,
    row = response,
    col = trt,
    missing = "ifany"
  )
  expect_equal(
    "Unknown" %in% x$table_body$label,
    TRUE
  )
})


test_that("tbl_cross- test 'always' missing returns Unknown even when none", {
  x <- tbl_cross(trial,
    row = trt,
    col = grade,
    missing = "always"
  )
  expect_equal(
    "Unknown" %in% x$table_body$label,
    TRUE
  )
})


test_that("tbl_cross- works with grouped data (it ungroups it first)", {
  expect_error(
    trial %>% dplyr::group_by(response) %>% tbl_cross(death, trt),
    NA
  )
})

# Test Dichotomous -> Categorical  -------------------------------------------
test_that("tbl_cross- test 'no' missing throws message", {
  data <- data.frame(
    X = rep(c("Yes", "No"), 3),
    Y = rep(c("Yes", "No"), each = 3)
  )

  table <- data %>% tbl_cross(row = X, col = Y)

  type <- table$meta_data %>%
    filter(variable == "X") %>%
    pull(summary_type)

  expect_equal(type, "categorical")
})

# Margin Argument  -------------------------------------------
test_that("tbl_cross- test NULL margin argument", {
  margins <- tbl_cross(trial,
    row = trt,
    col = response
  )

  no_margins <- tbl_cross(trial,
    row = trt,
    col = response,
    margin = NULL
  )

  # test row margins ------
  expect_equal(
    "..total.." %in% margins$table_body$variable,
    TRUE
  )
  expect_equal(
    "..total.." %in% no_margins$table_body$variable,
    FALSE
  )

  # test col margins ------
  expect_equal(
    "stat_0" %in% names(margins$table_body),
    TRUE
  )
  expect_equal(
    "stat_0" %in% names(no_margins$table_body),
    FALSE
  )
})

# digits Argument  -------------------------------------------
test_that("tbl_cross-digits argument", {
  expect_equal(
    tbl_cross(
      trial,
      response,
      stage,
      statistic = "{n} ({p}%)",
      digits = c(0, 2)
    ) %>%
      as_tibble(col_labels = FALSE) %>%
      dplyr::pull(stat_1),
    c(NA, "34 (17.00%)", "18 (9.00%)", "1 (0.50%)", "53 (26.50%)")
  )
})
