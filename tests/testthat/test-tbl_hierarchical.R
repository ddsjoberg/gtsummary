skip_on_cran()
skip_if_not(is_pkg_installed("withr"))

trial2 <- trial |>
  mutate(id = rep(1:50, length.out = nrow(trial)))

# tbl_hierarchical(data) ------------------------------------------------------------
test_that("tbl_hierarchical(data) works properly", {
  # creates table when data frame is passed
  expect_snapshot(tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = id) |> as.data.frame())

  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_hierarchical())
  expect_snapshot(error = TRUE, tbl_hierarchical(data = letters))
})

# tbl_hierarchical(by) ------------------------------------------------------------
test_that("tbl_hierarchical(by) works properly", {
  # creates table when by is passed
  expect_snapshot(tbl_hierarchical(data = trial2, variables = stage, by = trt, denominator = trial2, id = id) |> as.data.frame())

  # errors thrown when bad by argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, variables = stage, by = name, denominator = trial2, id = id)
  )
})

# tbl_hierarchical(id) ------------------------------------------------------------
test_that("tbl_hierarchical(id) works properly", {
  # errors thrown when bad id argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = 10)
  )
})

# tbl_hierarchical(denominator) ------------------------------------------------------------
test_that("tbl_hierarchical(denominator) works properly", {
  # errors thrown when bad denominator argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, variables = trt, denominator = "test", id = id)
  )
})

# tbl_hierarchical(include) ------------------------------------------------------------
test_that("tbl_hierarchical(include) works properly", {
  # creates table when include is passed
  expect_snapshot(
    tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2, id = id, include = grade) |> as.data.frame()
  )
  expect_snapshot(
    tbl_hierarchical(data = trial2, variables = c(stage, grade), by = trt, denominator = trial2, id = id, include = NULL) |> as.data.frame()
  )

  # errors thrown when bad include argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(
      data = trial2, variables = c(stage, grade), denominator = trial2, id = id, include = name
    )
  )
})

# tbl_hierarchical(statistic) ------------------------------------------------------------
test_that("tbl_hierarchical(statistic) works properly", {
  # creates table when statistic is passed
  expect_snapshot(tbl_hierarchical(
    data = trial2, variables = c(stage, grade), denominator = trial2, id = id,
    statistic = ~"{n}, {N}, {p}"
  ) |> as.data.frame())

  # errors thrown when bad statistic argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(
      data = trial2, variables = c(stage, grade), denominator = trial2, id = id,
      statistic = ~list(stage = "{n}")
    )
  )
})

# tbl_hierarchical(overall_row) ------------------------------------------------------------
test_that("tbl_hierarchical(overall_row) works properly", {
  withr::local_options(list(width = 120))

  # creates table when overall_row is passed
  expect_snapshot(tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = id, overall_row = TRUE) |> as.data.frame())

  # value are correct when by is passed
  expect_warning(expect_warning(
    res <-
      tbl_hierarchical(data = trial2, variables = trt, by = grade, denominator = trial2, id = id, overall_row = TRUE)
  ))
  expect_snapshot(res |> as.data.frame())
  expect_equal((res |> as.data.frame())[1, 1], "Number of patients with event")
  expect_equal(
    (res |> as.data.frame())[1, -1] |> as.character(),
    c("40 (59%)", "38 (56%)", "39 (61%)")
  )

  # overall row labeling works
  expect_warning(expect_warning(
    res <-
      tbl_hierarchical(
        data = trial2, variables = trt, by = grade, denominator = trial2, id = id, overall_row = TRUE,
        label = list(..ard_hierarchical_overall.. = "Total patients")
      )
  ))
  expect_equal((res |> as.data.frame())[1, 1], "Total patients")

  # errors thrown when bad overall_row argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, variables = trt, denominator = trial2, id = id, overall_row = "test")
  )
})

# tbl_hierarchical(label) ------------------------------------------------------------
test_that("tbl_hierarchical(label) works properly", {
  # creates table when label is passed
  res <- tbl_hierarchical(
    data = trial2, variables = c(stage, grade), denominator = trial2, id = id,
    label = list(stage = "My Stage", grade = "My Grade")
  )
  expect_snapshot(res |> as.data.frame())
  expect_snapshot_value(res$table_styling$header$label[6])

  # errors thrown when bad label argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2, id = id, label = "Stages")
  )
})

# tbl_hierarchical(digits) ------------------------------------------------------------
test_that("tbl_hierarchical(digits) works properly", {
  # creates table when digits is passed
  res <- tbl_hierarchical(
    data = trial2, variables = c(stage, grade), denominator = trial2, id = id,
    digits = grade ~ list(n = label_style_number(digits = 1, decimal.mark = ","), p = 3)
  )
  expect_snapshot(res |> as.data.frame())
  expect_equal(res$table_body$stat_0[1], "36 (68%)")

  # testing passing vector
  expect_equal(
    tbl_hierarchical(
      data = trial2, variables = c(stage, grade), denominator = trial2, id = id,
      digits = grade ~ 2
    ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      dplyr::last(),
    "18.00 (100.00%)"
  )
  expect_equal(
    tbl_hierarchical(
      data = trial2, variables = c(stage, grade), denominator = trial2, id = id,
      digits = grade ~ c(0, 2)
    ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_0) |>
      dplyr::last(),
    "18 (100.00%)"
  )


  # errors thrown when bad digits argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, variables = c(stage, grade), denominator = trial2, id = id, digits = "0")
  )
})

# tbl_hierarchical with ordered variables ------------------------------------------------------------
test_that("tbl_hierarchical works properly when last variable of hierarchy is ordered", {
  data <- cards::ADAE |>
    dplyr::filter(
      AESOC %in% unique(cards::ADAE$AESOC)[1:10],
      AETERM %in% unique(cards::ADAE$AETERM)[1:10]
    )

  # unordered variable
  res_uo <- tbl_hierarchical(
    data = data, variables = c(AESOC, AESEV), by = TRTA, id = USUBJID,
    denominator = cards::ADSL |> mutate(TRTA = ARM), include = AESEV
  )

  # ordered variable
  data$AESEV <- factor(data$AESEV, ordered = TRUE)

  res_o <- tbl_hierarchical(
    data = data, variables = c(AESOC, AESEV), by = TRTA, id = USUBJID,
    denominator = cards::ADSL |> mutate(TRTA = ARM), include = AESEV, label = list(AESEV = "Highest Severity")
  ) |> suppressMessages()
  expect_snapshot(res_o |> as.data.frame())

  # compare ordered and unordered results
  expect_true(res_uo$table_body$stat_1[9] > res_o$table_body$stat_1[10])

  # ordered variable, no by
  res <- tbl_hierarchical(
    data = data, variables = c(AESOC, AESEV),
    denominator = cards::ADSL |> mutate(TRTA = ARM), id = USUBJID
  ) |> suppressMessages()
  expect_snapshot(res |> as.data.frame())
})

# tbl_hierarchical_count(data) ------------------------------------------------------------
test_that("tbl_hierarchical_count(data) works properly", {
  # creates table when data frame is passed
  expect_snapshot(tbl_hierarchical_count(data = trial, variables = trt) |> as.data.frame())
  expect_snapshot(tbl_hierarchical_count(data = iris, variables = Species) |> as.data.frame())

  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_hierarchical_count())
  expect_snapshot(error = TRUE, tbl_hierarchical_count(data = letters))
})

# tbl_hierarchical_count(by) ------------------------------------------------------------
test_that("tbl_hierarchical_count(by) works properly", {
  # creates table when by is passed
  expect_snapshot(tbl_hierarchical_count(data = trial, variables = stage, by = trt) |> as.data.frame())

  # errors thrown when bad by argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, variables = stage, by = name)
  )
})

# tbl_hierarchical_count(denominator) ------------------------------------------------------------
test_that("tbl_hierarchical_count(denominator) works properly", {
  # creates table when denominator is passed
  res <- tbl_hierarchical_count(data = trial, variables = trt, denominator = rbind(trial, trial))
  expect_snapshot(res |> as.data.frame())
  expect_equal(res$table_styling$header$modify_stat_N[1], nrow(trial) * 2)

  # errors thrown when bad denominator argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, variables = trt, denominator = "test")
  )
})

# tbl_hierarchical_count(include) ------------------------------------------------------------
test_that("tbl_hierarchical_count(include) works properly", {
  # creates table when include is passed
  expect_snapshot(
    tbl_hierarchical_count(data = trial, variables = c(stage, grade), include = grade) |> as.data.frame()
  )
  expect_snapshot(
    tbl_hierarchical_count(data = trial, variables = c(stage, grade), by = trt, include = NULL) |> as.data.frame()
  )

  # errors thrown when bad include argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, variables = c(stage, grade), include = name)
  )
})

# tbl_hierarchical_count(overall_row) ------------------------------------------------------------
test_that("tbl_hierarchical_count(overall_row) works properly", {
  # creates table when overall_row is passed
  expect_snapshot(tbl_hierarchical_count(data = trial, variables = trt, overall_row = TRUE) |> as.data.frame())

  # value are correct when by is passed
  res <- tbl_hierarchical_count(data = trial, variables = trt, by = grade, overall_row = TRUE)
  expect_snapshot(res |> as.data.frame())
  expect_equal((res |> as.data.frame())[1, 1], "Total number of events")
  expect_equal(
    (res |> as.data.frame())[1, -1] |> as.character(),
    (trial |> dplyr::group_by(grade) |> dplyr::summarise(n = dplyr::n()))$n |> as.character()
  )

  # overall row labeling works
  res <- tbl_hierarchical_count(
    data = trial, variables = trt, by = grade, overall_row = TRUE, label = list(..ard_hierarchical_overall.. = "Total rows")
  )
  expect_equal((res |> as.data.frame())[1, 1], "Total rows")

  # errors thrown when bad overall_row argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, variables = trt, overall_row = "test")
  )
})

# tbl_hierarchical_count(label) ------------------------------------------------------------
test_that("tbl_hierarchical_count(label) works properly", {
  # creates table when label is passed
  res <- tbl_hierarchical_count(
    data = trial, variables = c(stage, grade), label = list(stage = "My Stage", grade = "My Grade")
  )
  expect_snapshot(res |> as.data.frame())
  expect_snapshot_value(res$table_styling$header$label[6])

  # errors thrown when bad label argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, variables = c(stage, grade), label = "Stages")
  )
})

# tbl_hierarchical_count(digits) ------------------------------------------------------------
test_that("tbl_hierarchical_count(digits) works properly", {
  # creates table when digits is passed
  res <- tbl_hierarchical_count(
    data = trial, variables = c(stage, grade),
    digits = everything() ~ list(n = label_style_number(digits = 1, decimal.mark = ","))
  )
  expect_snapshot(res |> as.data.frame())
  expect_equal(res$table_body$stat_0[1], "53,0")

  # errors thrown when bad digits argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, variables = c(stage, grade), digits = n ~ 2)
  )
})

# tbl_hierarchical_count with 10+ hierarchy variables --------------------------------------
test_that("tbl_hierarchical_count with 10+ hierarchy variables", {
  withr::local_options(list(width = 250))
  set.seed(1)
  data <- data.frame(
    x1 = sample(LETTERS[1:2], 30, replace = TRUE),
    x2 = sample(LETTERS[3:4], 30, replace = TRUE),
    x3 = sample(LETTERS[5:6], 30, replace = TRUE),
    x4 = sample(LETTERS[7:8], 30, replace = TRUE),
    x5 = sample(LETTERS[9:10], 30, replace = TRUE),
    x6 = sample(LETTERS[11:12], 30, replace = TRUE),
    x7 = sample(LETTERS[13:14], 30, replace = TRUE),
    x8 = sample(LETTERS[15:16], 30, replace = TRUE),
    x9 = sample(LETTERS[17:18], 30, replace = TRUE),
    x10 = sample(LETTERS[19:20], 30, replace = TRUE)
  )

  res <- expect_silent(
    tbl_hierarchical_count(data = data, variables = names(data), include = "x10")
  )
  expect_snapshot(res |> as.data.frame())
})

# tbl_hierarchical_count table_body enables sorting ----------------------------------------
test_that("tbl_hierarchical_count table_body enables sorting", {
  withr::local_options(list(width = 250))

  ADAE_subset <- cards::ADAE |>
    dplyr::filter(
      AESOC %in% unique(cards::ADAE$AESOC)[1:5],
      AETERM %in% unique(cards::ADAE$AETERM)[1:5]
    )

  res <- expect_silent(
    tbl_hierarchical(
      data = ADAE_subset,
      variables = c(SEX, AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL |> mutate(TRTA = ARM),
      id = USUBJID,
      overall_row = TRUE
    )
  )

  expect_snapshot(res$table_body)
})
