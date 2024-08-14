skip_on_cran()

# tbl_summary(data) ------------------------------------------------------------
test_that("tbl_summary(data)", {
  # creates table when data frame is passed
  expect_snapshot(tbl_summary(data = trial) |> as.data.frame())
  expect_snapshot(tbl_summary(data = mtcars) |> as.data.frame())
  expect_snapshot(tbl_summary(data = iris) |> as.data.frame())
})

test_that("tbl_summary(data) errors properly", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_summary())
  expect_snapshot(error = TRUE, tbl_summary(data = letters))
  expect_snapshot(error = TRUE, tbl_summary(data = dplyr::tibble()))
})

# tbl_summary(by) --------------------------------------------------------------
test_that("tbl_summary(by)", {
  expect_snapshot(tbl_summary(data = trial, by = trt) |> as.data.frame())
  expect_snapshot(tbl_summary(data = mtcars, by = am) |> as.data.frame())
  expect_snapshot(tbl_summary(data = iris, by = Species) |> as.data.frame())

  # ensure the columns appear in the correct order with 10+ by levels
  expect_equal(
    tbl_summary(data.frame(x = 1, y = LETTERS[1:10]), by = y, type = x ~ "continuous") |>
      getElement("table_body") |>
      select(all_stat_cols()) |>
      names(),
    paste0("stat_", 1:10)
  )
  expect_equal(
    tbl_summary(data.frame(x = 1, y = LETTERS[1:10]), by = y, type = x ~ "continuous2") |>
      getElement("table_body") |>
      select(all_stat_cols()) |>
      names(),
    paste0("stat_", 1:10)
  )
  expect_equal(
    tbl_summary(data.frame(x = 1, y = LETTERS[1:10]), by = y, type = x ~ "categorical") |>
      getElement("table_body") |>
      select(all_stat_cols()) |>
      names(),
    paste0("stat_", 1:10)
  )
  expect_equal(
    tbl_summary(data.frame(x = 1, y = LETTERS[1:10]), by = y, type = x ~ "dichotomous", value = x ~ 1) |>
      getElement("table_body") |>
      select(all_stat_cols()) |>
      names(),
    paste0("stat_", 1:10)
  )
})

test_that("tbl_summary(by) errors properly", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_summary(mtcars, by = c("mpg", "am")))
})


# tbl_summary(label) -----------------------------------------------------------
test_that("tbl_summary(label)", {
  expect_error(
    tbl <- tbl_summary(
      mtcars,
      by = am,
      label = list(mpg = "New mpg", cyl = "New cyl"),
      include = c(mpg, cyl)
    ),
    NA
  )
  expect_snapshot(as.data.frame(tbl))

  expect_equal(
    tbl$table_body |>
      dplyr::filter(row_type %in% "label") |>
      dplyr::pull(label),
    c("New mpg", "New cyl")
  )
})

test_that("tbl_summary(label) errors properly", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial["age"], label = list(age = letters))
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial["age"], label = letters)
  )
})

# tbl_summary(statistic) -------------------------------------------------------
test_that("tbl_summary(statistic)", {
  # categorical summary
  expect_equal(
    trial |>
      tbl_summary(
        include = response,
        statistic =
          response ~ "n={n} | N={N} | p={p} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}",
        missing = "no"
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0),
    "n=61 | N=193 | p=32 | N_obs=200 | N_miss=7 | N_nonmiss=193 | p_miss=3.5 | p_nonmiss=97"
  )

  # continuous summary, testing cv function and passed in formula
  expect_equal(
    {
      cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
      trial |>
        tbl_summary(
          include = age,
          statistic =
            age ~ "cv={cv} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}",
          missing = "no"
        ) |>
        as.data.frame(col_labels = FALSE) |>
        dplyr::pull(stat_0)
    },
    "cv=30 | N_obs=200 | N_miss=11 | N_nonmiss=189 | p_miss=5.5 | p_nonmiss=95"
  )

  # continuous summary, testing cv function and passed in named list
  expect_equal(
    {
      cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
      trial |>
        tbl_summary(
          include = age,
          statistic =
            list(age = "cv={cv} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}"),
          missing = "no"
        ) |>
        as.data.frame(col_labels = FALSE) |>
        dplyr::pull(stat_0)
    },
    "cv=30 | N_obs=200 | N_miss=11 | N_nonmiss=189 | p_miss=5.5 | p_nonmiss=95"
  )
})

test_that("tbl_summary(statistic) errors properly", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = response,
      statistic = ~"{n} ({not_a_statistic})"
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = age,
      statistic = ~"({not_a_summary_statistic})"
    )
  )
})

test_that("tbl_summary(statistic,type) errors", {
  # we get a nice message for a continuous variable with stat as a character vector
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = age,
      statistic = ~c("{mean}", "{sd}")
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = grade,
      statistic = ~c("{mean}", "{sd}")
    )
  )
})

# tbl_summary(digit) -----------------------------------------------------------
test_that("tbl_summary(digit)", {
  expect_error(
    tbl <- tbl_summary(
      trial,
      include = c(age, response, marker, ttdeath),
      digits = list(
        # using named list to change 2 of the 3 statistics
        age = list(median = 4, p25 = \(x) style_number(x, digits = 2)),
        # using a vector of integers
        response = c(0, 3),
        # using a single integer that will apply to all stats
        marker = 0,
        # passing a single function that will apply to all stats
        ttdeath = list(\(x) style_number(x, digits = 2))
      ),
      missing = "no"
    ) |>
      modify_column_unhide(variable) |>
      as.data.frame(col_labels = FALSE),
    NA
  )

  # check the correct stats
  expect_equal(
    tbl |>
      dplyr::filter(variable == "age") |>
      dplyr::pull(stat_0),
    "47.0000 (38.00, 57)"
  )

  expect_equal(
    tbl |>
      dplyr::filter(variable == "response") |>
      dplyr::pull(stat_0),
    "61 (31.606%)"
  )

  expect_equal(
    tbl |>
      dplyr::filter(variable == "marker") |>
      dplyr::pull(stat_0),
    "1 (0, 1)"
  )

  expect_equal(
    tbl |>
      dplyr::filter(variable == "ttdeath") |>
      dplyr::pull(stat_0),
    "22.41 (15.92, 24.00)"
  )

  expect_silent(
    tbl <-
      tbl_summary(
        mtcars,
        include = carb,
        type = carb ~ "categorical",
        digits = carb ~ c(0, 1)
      )
  )
  expect_equal(
    tbl$table_body$stat_0 |>
      dplyr::last(),
    "1 (3.1%)"
  )
})

test_that("tbl_summary(digit) errors properly", {
  expect_error(
    tbl_summary(
      trial,
      include = age,
      digits = list(
        age = list(
          median = letters, # this is not a function!
          p25 = \(x) style_number(x, digits = 2)
        )
      ),
      missing = "no"
    ),
    "*"
  )
})

# tbl_summary(type) ------------------------------------------------------------
test_that("tbl_summary(type)", {
  expect_snapshot(
    tbl_summary(
      trial,
      include = c(age, marker, response, stage),
      type = list(age = "continuous", marker = "continuous2", response = "dichotomous", state = "categorical"),
      missing = "no"
    ) |>
      getElement("table_body") |>
      dplyr::select(variable, var_type, row_type, label)
  )

  # can use the default type to select variables to change the summary type
  expect_equal(
    tbl_summary(
      trial,
      type = list(all_continuous() ~ "continuous2", all_dichotomous() ~ "continuous"),
      include = c(age, marker, response),
      missing = "no"
    ) |>
      getElement("inputs") |>
      getElement("type"),
    list(age = "continuous2", marker = "continuous2", response = "continuous")
  )

  # yes/no variables default to dichotomous
  expect_equal(
    data.frame(yn = c("no", "yes", "yes")) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )
  expect_equal(
    data.frame(
      yn = c("no", "yes", "yes") |> factor()
    ) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )
  expect_equal(
    data.frame(
      yn = c("no", "yes", "yes") |> factor(levels = c("yes", "no"))
    ) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )
  expect_equal(
    data.frame(
      yn = c("no", "no", "no") |> factor(levels = c("no", "yes"))
    ) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )

  # a yes or no only character defaults to categorical
  expect_equal(
    data.frame(yn = c("yes", "yes")) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
  expect_equal(
    data.frame(yn = c("no", "no")) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
  expect_equal(
    data.frame(yn = c("nO", "yEs", "yEs")) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yEs"
  )

  # a zero or one only numeric defaults to categorical
  expect_equal(
    data.frame(yn = c(0, 0)) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
  expect_equal(
    data.frame(yn = c(1, 1)) |>
      tbl_summary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
})

test_that("tbl_summary(type) proper errors/messages", {
  # grade cannot be summarized continuously, and we'll see reports in the console
  expect_snapshot(
    tbl <- tbl_summary(
      trial,
      include = grade,
      type = grade ~ "continuous"
    )
  )
  expect_equal(tbl$table_body$stat_0, "NA (NA, NA)")

  # unobserved levels cannot be summarized for a dichotomous summary
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = grade,
      type = grade ~ "dichotomous",
      value = grade ~ "IV"
    )
  )

  # error when no clear dichotomous value present
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = grade,
      type = grade ~ "dichotomous"
    )
  )
})

# tbl_summary(value) -----------------------------------------------------------
test_that("tbl_summary(value)", {
  # ensure grade is coerced to dichotomous and response defaults to dichotomous
  expect_error(
    tbl <- tbl_summary(trial, value = "grade" ~ "III", include = c(grade, response)),
    NA
  )
  expect_snapshot(as.data.frame(tbl))

  # check all summary types are assigned to dichotomous
  expect_equal(
    tbl$table_body$var_type |> unique(),
    "dichotomous"
  )

  # check we can pass unobserved levels to values
  expect_equal(
    trial |>
      dplyr::mutate(
        grade = factor(grade, levels = c("I", "II", "III", "IV")),
        response = TRUE
      ) |>
      tbl_summary(
        include = c(grade, response),
        value = list(grade = "IV", response = FALSE)
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      unique(),
    "0 (0%)"
  )
})

test_that("tbl_summary(value) errors properly", {
  # passing a value that does not exist
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, value = "grade" ~ "IV", include = c(grade, response))
  )
})

# tbl_summary(missing) ---------------------------------------------------------
test_that("tbl_summary(missing)", {
  # default is correctly "ifany"
  expect_equal(
    tbl_summary(
      trial,
      include = c(trt, age)
    ) |>
      as.data.frame(),
    tbl <- tbl_summary(
      trial,
      include = c(trt, age),
      missing = "ifany"
    ) |>
      as.data.frame()
  )
  # age includes an Unknown row, and trt does not
  expect_equal(tbl[, 1], c("Chemotherapy Treatment", "Drug A", "Drug B", "Age", "Unknown"))

  # all vars have a missing row when requested
  expect_equal(
    tbl_summary(
      trial,
      include = c(trt, age),
      missing = "always"
    ) |>
      getElement("table_body") |>
      dplyr::filter(row_type %in% "missing") |>
      nrow(),
    2L
  )

  # None of the vars have a missing row when requested
  expect_equal(
    tbl_summary(
      trial,
      include = c(trt, age),
      missing = "no"
    ) |>
      getElement("table_body") |>
      dplyr::filter(row_type %in% "missing") |>
      nrow(),
    0L
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      missing = "NOT AN OPTION"
    )
  )
})

# tbl_summary(missing_text) ----------------------------------------------------
test_that("tbl_summary(missing_text)", {
  expect_snapshot(
    tbl_summary(
      trial,
      include = response,
      missing_text = "(MISSING)"
    ) |>
      as.data.frame(col_label = FALSE)
  )

  # errors with invalid inputs
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = response,
      missing_text = letters
    )
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(
      trial,
      include = response,
      missing_text = 10L
    )
  )
})

# tbl_summary(missing_stat) ----------------------------------------------------
test_that("tbl_summary(missing_stat)", {
  # basic reporting works
  expect_equal(
    tbl_summary(
      trial,
      include = response,
      missing_stat = "N = {N_miss}"
    ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      dplyr::last(),
    "N = 7"
  )

  # reporting of non-standard stats works as well
  expect_equal(
    tbl_summary(
      trial,
      include = response,
      missing_stat = "{N_miss}, {N_obs}, {N_nonmiss}, {p_miss}, {p_nonmiss}"
    ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      dplyr::last(),
    "7, 200, 193, 3.5, 97"
  )

  # errors with bad inputs
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = response, missing_stat = letters)
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = response, missing_stat = 10L)
  )
})

# tbl_summary(sort) ------------------------------------------------------------
test_that("tbl_summary(sort)", {
  expect_equal(
    tbl_summary(mtcars, sort = all_categorical() ~ "frequency", include = cyl) |>
      getElement("table_body") |>
      dplyr::filter(row_type %in% "level") |>
      dplyr::pull(label),
    c("8", "4", "6")
  )
})

test_that("tbl_summary(sort) errors properly", {
  # proper errors are returned
  expect_snapshot(
    error = TRUE,
    tbl_summary(mtcars, sort = list(all_categorical() ~ c("frequency", "two")))
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(mtcars, sort = list(all_categorical() ~ "freq5555uency"))
  )
})

# tbl_summary(percent) ---------------------------------------------------------
test_that("tbl_summary(percent)", {
  expect_snapshot(
    tbl_summary(trial, by = trt, include = grade, percent = "column", statistic = ~"{p}%") |>
      as.data.frame(col_labels = FALSE)
  )

  expect_snapshot(
    tbl_summary(trial, by = trt, include = grade, percent = "row", statistic = ~"{p}%") |>
      as.data.frame(col_labels = FALSE)
  )

  expect_snapshot(
    tbl_summary(trial, by = trt, include = grade, percent = "cell", statistic = ~"{p}%") |>
      as.data.frame(col_labels = FALSE)
  )

  # errors with bad input
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, by = trt, include = grade, percent = letters, statistic = ~"{p}%")
  )
})

test_that("tbl_summary() with hms times", {
  # originally reported in https://github.com/ddsjoberg/gtsummary/issues/1893
  skip_if_not_installed("hms")
  withr::local_package("hms")

  trial2 <- trial |> dplyr::mutate(time_hms = hms(seconds = 15))
  expect_silent(
    tbl <- tbl_summary(trial2, by = trt, include = time_hms)
  )
  expect_equal(
    tbl$table_body$label,
    c("time_hms", "00:00:15")
  )
})

