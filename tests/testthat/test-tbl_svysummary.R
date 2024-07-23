skip_on_cran()
skip_if_not(is_pkg_installed("survey", reference_pkg = "gtsummary"))

svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
df_titanic <- as.data.frame(Titanic) |> tidyr::uncount(weights = Freq)
svy_mtcars <- survey::svydesign(~1, data = mtcars, weights = ~1)
svy_trial <- survey::svydesign(~1, data = trial, weights = ~1)


# tbl_svysummary(data) ---------------------------------------------------------
test_that("tbl_svysummary(data)", {
  # creates table when data frame is passed
  expect_snapshot(tbl_svysummary(data = svy_titanic) |> as.data.frame())
  expect_snapshot(tbl_svysummary(data = svy_mtcars) |> as.data.frame())
})


test_that("tbl_svysummary(data) errors properly", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_svysummary())
  expect_snapshot(error = TRUE, tbl_svysummary(data = letters))
  expect_snapshot(error = TRUE, tbl_svysummary(data = dplyr::tibble()))
})

# tbl_svysummary(by) -----------------------------------------------------------
test_that("tbl_svysummary(by)", {
  expect_snapshot(tbl_svysummary(data = svy_mtcars, by = am) |> as.data.frame())
  expect_snapshot(tbl_svysummary(data = svy_titanic, by = Survived) |> as.data.frame())
})

test_that("tbl_svysummary(by) errors properly", {
  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_svysummary(svy_mtcars, by = c("mpg", "am")))
})

# tbl_svysummary(label) --------------------------------------------------------
test_that("tbl_svysummary(label)", {
  expect_silent(
    tbl <- tbl_svysummary(
      svy_mtcars,
      by = am,
      label = list(mpg = "New mpg", cyl = "New cyl"),
      include = c(mpg, cyl)
    )
  )
  expect_snapshot(as.data.frame(tbl))

  expect_equal(
    tbl$table_body |>
      dplyr::filter(row_type %in% "label") |>
      dplyr::pull(label),
    c("New mpg", "New cyl")
  )
})

test_that("tbl_svysummary(label) errors properly", {
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, include = age, label = list(age = letters))
  )

  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, include = age, label = letters)
  )
})

# tbl_svysummary(statistic) ----------------------------------------------------
test_that("tbl_svysummary(statistic)", {
  # categorical summary
  expect_equal(
    svy_trial |>
      tbl_svysummary(
        include = response,
        statistic =
          response ~ "n={n} | N={N} | p={p} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss} | p.std.error={p.std.error} | deff={deff} | n_unweighted={n_unweighted} | N_unweighted={N_unweighted} | p_unweighted={p_unweighted}",
        missing = "no"
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0),
    "n=61 | N=193 | p=32 | N_obs=200 | N_miss=7 | N_nonmiss=193 | p_miss=3.5 | p_nonmiss=97 | p.std.error=0.0 | deff=Inf | n_unweighted=61 | N_unweighted=193 | p_unweighted=31.6"
  )

  # continuous summary when there is no "continuous" stats (just missingness stats)
  expect_equal(
    {
      svy_trial |>
        tbl_svysummary(
          include = age,
          statistic =
            age ~ "N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}",
          missing = "no"
        ) |>
        as.data.frame(col_labels = FALSE) |>
        dplyr::pull(stat_0)
    },
    "N_obs=200 | N_miss=11 | N_nonmiss=189 | p_miss=5.5 | p_nonmiss=95"
  )

  # continuous summary
  expect_equal(
      svy_trial |>
        tbl_svysummary(
          include = age,
          statistic =
            list(age = "mean={mean} | N_obs={N_obs} | N_miss={N_miss} | N_nonmiss={N_nonmiss} | p_miss={p_miss} | p_nonmiss={p_nonmiss}"),
          missing = "no"
        ) |>
        as.data.frame(col_labels = FALSE) |>
        dplyr::pull(stat_0),
    "mean=47 | N_obs=200 | N_miss=11 | N_nonmiss=189 | p_miss=5.5 | p_nonmiss=95"
  )
})

test_that("tbl_svysummary(statistic) errors properly", {
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = response,
      statistic = ~"{n} ({not_a_statistic})"
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = age,
      statistic = ~"({not_a_summary_statistic})"
    )
  )
})

test_that("tbl_svysummary(statistic,type) errors", {
  # we get a nice message for a continuous variable with stat as a character vector
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = age,
      statistic = ~c("{mean}", "{sd}")
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = grade,
      statistic = ~c("{mean}", "{sd}")
    )
  )
})

# tbl_svysummary(digit) --------------------------------------------------------
test_that("tbl_svysummary(digit)", {
  expect_error(
    tbl <- tbl_svysummary(
      svy_trial,
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
    "22.40 (15.77, 24.00)"
  )
})

test_that("tbl_svysummary(digit) errors properly", {
  expect_error(
    tbl_svysummary(
      svy_trial,
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

# tbl_svysummary(type) ---------------------------------------------------------
test_that("tbl_svysummary(type)", {
  expect_snapshot(
    tbl_svysummary(
      svy_trial,
      include = c(age, marker, response, stage),
      type = list(age = "continuous", marker = "continuous2", response = "dichotomous", state = "categorical"),
      missing = "no"
    ) |>
      getElement("table_body") |>
      dplyr::select(variable, var_type, row_type, label)
  )

  # can use the default type to select variables to change the summary type
  expect_equal(
    tbl_svysummary(
      svy_trial,
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
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )
  expect_equal(
    data.frame(
      yn = c("no", "yes", "yes") |> factor()
    ) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )
  expect_equal(
    data.frame(
      yn = c("no", "yes", "yes") |> factor(levels = c("yes", "no"))
    ) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )
  expect_equal(
    data.frame(
      yn = c("no", "no", "no") |> factor(levels = c("no", "yes"))
    ) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yes"
  )

  # a yes or no only character defaults to categorical
  expect_equal(
    data.frame(yn = c("yes", "yes")) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
  expect_equal(
    data.frame(yn = c("no", "no")) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
  expect_equal(
    data.frame(yn = c("nO", "yEs", "yEs")) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("value") |>
      getElement("yn"),
    "yEs"
  )

  # a zero or one only numeric defaults to categorical
  expect_equal(
    data.frame(yn = c(0, 0)) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
  expect_equal(
    data.frame(yn = c(1, 1)) |>
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary() |>
      getElement("inputs") |>
      getElement("type") |>
      getElement("yn"),
    "categorical"
  )
})

test_that("tbl_svysummary(type) proper errors/messages", {
  # grade cannot be summarized continuously, and we'll see reports in the console
  expect_snapshot(
    tbl <- tbl_svysummary(
      svy_trial,
      include = grade,
      type = grade ~ "continuous",
      statistic = ~ "{min}"
    )
  )
  expect_equal(tbl$table_body$stat_0, "NA")

  # unobserved levels cannot be summarized for a dichotomous summary
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = grade,
      type = grade ~ "dichotomous",
      value = grade ~ "IV"
    )
  )

  # error when no clear dichotomous value present
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = grade,
      type = grade ~ "dichotomous"
    )
  )
})

# tbl_svysummary(value) --------------------------------------------------------
test_that("tbl_svysummary(value)", {
  # ensure grade is coerced to dichotomous and response defaults to dichotomous
  expect_error(
    tbl <- tbl_svysummary(svy_trial, value = "grade" ~ "III", include = c(grade, response)),
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
      survey::svydesign(~1, data = _, weights = ~1) |>
      tbl_svysummary(
        include = c(grade, response),
        value = list(grade = "IV", response = FALSE)
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      unique(),
    "0 (0%)"
  )
})

test_that("tbl_svysummary(value) errors properly", {
  # passing a value that does not exist
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, value = "grade" ~ "IV", include = c(grade, response))
  )
})

# tbl_svysummary(missing) ------------------------------------------------------
test_that("tbl_svysummary(missing)", {
  # default is correctly "ifany"
  expect_equal(
    tbl_svysummary(
      svy_trial,
      include = c(trt, age)
    ) |>
      as.data.frame(),
    tbl <- tbl_svysummary(
      svy_trial,
      include = c(trt, age),
      missing = "ifany"
    ) |>
      as.data.frame()
  )
  # age includes an Unknown row, and trt does not
  expect_equal(tbl[, 1], c("Chemotherapy Treatment", "Drug A", "Drug B", "Age", "Unknown"))

  # all vars have a missing row when requested
  expect_equal(
    tbl_svysummary(
      svy_trial,
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
    tbl_svysummary(
      svy_trial,
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
    tbl_svysummary(
      svy_trial,
      missing = "NOT AN OPTION"
    )
  )
})

# tbl_svysummary(missing_text) -------------------------------------------------
test_that("tbl_svysummary(missing_text)", {
  expect_snapshot(
    tbl_svysummary(
      svy_trial,
      include = response,
      missing_text = "(MISSING)"
    ) |>
      as.data.frame(col_label = FALSE)
  )

  # errors with invalid inputs
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = response,
      missing_text = letters
    )
  )
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(
      svy_trial,
      include = response,
      missing_text = 10L
    )
  )
})

# tbl_svysummary(missing_stat) -----------------------------------------------
test_that("tbl_svysummary(missing_stat)", {
  # basic reporting works
  expect_equal(
    tbl_svysummary(
      svy_trial,
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
    tbl_svysummary(
      svy_trial,
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
    tbl_svysummary(svy_trial, include = response, missing_stat = letters)
  )
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, include = response, missing_stat = 10L)
  )
})

# tbl_svysummary(sort) -------------------------------------------------------
test_that("tbl_svysummary(sort)", {
  expect_equal(
    tbl_svysummary(svy_mtcars, sort = all_categorical() ~ "frequency", include = cyl) |>
      getElement("table_body") |>
      dplyr::filter(row_type %in% "level") |>
      dplyr::pull(label),
    c("8", "4", "6")
  )
})

test_that("tbl_svysummary(sort) errors properly", {
  # proper errors are returned
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_mtcars, sort = list(all_categorical() ~ c("frequency", "two")))
  )
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_mtcars, sort = list(all_categorical() ~ "freq5555uency"))
  )
})

# tbl_svysummary(percent) ----------------------------------------------------
test_that("tbl_svysummary(percent)", {
  expect_snapshot(
    tbl_svysummary(svy_trial, by = trt, include = grade, percent = "column", statistic = ~"{p}%") |>
      as.data.frame(col_labels = FALSE)
  )

  expect_snapshot(
    tbl_svysummary(svy_trial, by = trt, include = grade, percent = "row", statistic = ~"{p}%") |>
      as.data.frame(col_labels = FALSE)
  )

  expect_snapshot(
    tbl_svysummary(svy_trial, by = trt, include = grade, percent = "cell", statistic = ~"{p}%") |>
      as.data.frame(col_labels = FALSE)
  )

  # errors with bad input
  expect_snapshot(
    error = TRUE,
    tbl_svysummary(svy_trial, by = trt, include = grade, percent = letters, statistic = ~"{p}%")
  )
})
