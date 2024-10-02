skip_on_cran()

trial2 <- trial |>
  mutate(id = rep(1:50, length.out = nrow(trial)))

# tbl_hierarchical(data) ------------------------------------------------------------
test_that("tbl_hierarchical(data) works properly", {
  # creates table when data frame is passed
  expect_snapshot(tbl_hierarchical(data = trial2, hierarchies = trt, denominator = trial2, id = id) |> as.data.frame())

  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_hierarchical())
  expect_snapshot(error = TRUE, tbl_hierarchical(data = letters))
})

# tbl_hierarchical(by) ------------------------------------------------------------
test_that("tbl_hierarchical(by) works properly", {
  # creates table when by is passed
  expect_snapshot(tbl_hierarchical(data = trial2, hierarchies = stage, by = trt, denominator = trial2, id = id) |> as.data.frame())

  # errors thrown when bad by argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = stage, by = name, denominator = trial2, id = id)
  )
})

# tbl_hierarchical(id) ------------------------------------------------------------
test_that("tbl_hierarchical(id) works properly", {
  # errors thrown when bad id argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = trt, denominator = trial2, id = 10)
  )
})

# tbl_hierarchical(denominator) ------------------------------------------------------------
test_that("tbl_hierarchical(denominator) works properly", {
  # errors thrown when bad denominator argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = trt, denominator = 10)
  )
})

# tbl_hierarchical(include) ------------------------------------------------------------
test_that("tbl_hierarchical(include) works properly", {
  # creates table when include is passed
  expect_snapshot(
    tbl_hierarchical(data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id, include = grade) |> as.data.frame()
  )
  expect_snapshot(
    tbl_hierarchical(data = trial2, hierarchies = c(stage, grade), by = trt, denominator = trial2, id = id, include = NULL) |> as.data.frame()
  )

  # errors thrown when bad include argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id, include = name)
  )
})

# tbl_hierarchical(statistic) ------------------------------------------------------------
test_that("tbl_hierarchical(statistic) works properly", {
  # creates table when statistic is passed
  expect_snapshot(tbl_hierarchical(
    data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id,
    statistic = "{n}, {N}, {p}"
  ) |> as.data.frame())

  # errors thrown when bad statistic argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(
      data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id,
      statistic = list(stage = "{n}")
    )
  )
})

# tbl_hierarchical(overall_row) ------------------------------------------------------------
test_that("tbl_hierarchical(overall_row) works properly", {
  # creates table when overall_row is passed
  expect_snapshot(tbl_hierarchical(data = trial2, hierarchies = trt, denominator = trial2, id = id, overall_row = TRUE) |> as.data.frame())

  # value are correct when by is passed
  res <- tbl_hierarchical(data = trial2, hierarchies = trt, by = grade, denominator = trial2, id = id, overall_row = TRUE)
  expect_snapshot(res |> as.data.frame())
  expect_equal((res |> as.data.frame())[1, 1], "Total number of patients with any event")
  expect_equal(
    (res |> as.data.frame())[1, -1] |> as.character(),
    c("40 (58.8)", "38 (55.9)", "39 (60.9)")
  )

  # overall row labeling works
  res <- tbl_hierarchical(
    data = trial2, hierarchies = trt, by = grade, denominator = trial2, id = id, overall_row = TRUE,
    label = list(overall = "Total patients")
  )
  expect_equal((res |> as.data.frame())[1, 1], "Total rows")

  # errors thrown when bad overall_row argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = trt, denominator = trial2, id = id, overall_row = "test")
  )
})

# tbl_hierarchical(label) ------------------------------------------------------------
test_that("tbl_hierarchical(label) works properly", {
  # creates table when label is passed
  res <- tbl_hierarchical(
    data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id,
    label = list(stage = "My Stage", grade = "My Grade")
  )
  expect_snapshot(res |> as.data.frame())
  expect_snapshot_value(res$table_styling$header$label[4])

  # errors thrown when bad label argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id, label = "Stages")
  )
})

# tbl_hierarchical with ordered variables ------------------------------------------------------------
test_that("tbl_hierarchical works properly when last variable of hierarchy is ordered", {
  trial_o <- trial2
  trial_o$grade <- factor(trial_o$grade, ordered = TRUE)
  # creates table when label is passed
  res <- tbl_hierarchical(
    data = trial_o, hierarchies = c(stage, grade), by = trt, denominator = trial_o, id = id
  )
  expect_snapshot(res |> as.data.frame())
  expect_snapshot_value(res$table_styling$header$label[4])

  # errors thrown when bad label argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical(data = trial2, hierarchies = c(stage, grade), denominator = trial2, id = id, label = "Stages")
  )
})

# tbl_hierarchical(data) ------------------------------------------------------------
test_that("tbl_hierarchical(data) works properly", {
  # creates table when data frame is passed
  expect_snapshot(tbl_hierarchical(data = trial2, hierarchies = trt, denominator = trial2, id = id) |> as.data.frame())

  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_hierarchical())
  expect_snapshot(error = TRUE, tbl_hierarchical(data = letters))
})

# tbl_hierarchical_count(data) ------------------------------------------------------------
test_that("tbl_hierarchical_count(data) works properly", {
  # creates table when data frame is passed
  expect_snapshot(tbl_hierarchical_count(data = trial, hierarchies = trt) |> as.data.frame())
  expect_snapshot(tbl_hierarchical_count(data = iris, hierarchies = Species) |> as.data.frame())

  # errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_hierarchical_count())
  expect_snapshot(error = TRUE, tbl_hierarchical_count(data = letters))
})

# tbl_hierarchical_count(by) ------------------------------------------------------------
test_that("tbl_hierarchical_count(by) works properly", {
  # creates table when by is passed
  expect_snapshot(tbl_hierarchical_count(data = trial, hierarchies = stage, by = trt) |> as.data.frame())

  # errors thrown when bad by argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, hierarchies = stage, by = name)
  )
})

# tbl_hierarchical_count(denominator) ------------------------------------------------------------
test_that("tbl_hierarchical_count(denominator) works properly", {
  # creates table when denominator is passed
  res <- tbl_hierarchical_count(data = trial, hierarchies = trt, denominator = rbind(trial, trial))
  expect_snapshot(res |> as.data.frame())
  expect_equal(res$table_styling$header$modify_stat_N[1], nrow(trial) * 2)

  # errors thrown when bad denominator argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, hierarchies = trt, denominator = 10)
  )
})

# tbl_hierarchical_count(include) ------------------------------------------------------------
test_that("tbl_hierarchical_count(include) works properly", {
  # creates table when include is passed
  expect_snapshot(
    tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade), include = grade) |> as.data.frame()
  )
  expect_snapshot(
    tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade), by = trt, include = NULL) |> as.data.frame()
  )

  # errors thrown when bad include argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade), include = name)
  )
})

# tbl_hierarchical_count(overall_row) ------------------------------------------------------------
test_that("tbl_hierarchical_count(overall_row) works properly", {
  # creates table when overall_row is passed
  expect_snapshot(tbl_hierarchical_count(data = trial, hierarchies = trt, overall_row = TRUE) |> as.data.frame())

  # value are correct when by is passed
  res <- tbl_hierarchical_count(data = trial, hierarchies = trt, by = grade, overall_row = TRUE)
  expect_snapshot(res |> as.data.frame())
  expect_equal((res |> as.data.frame())[1, 1], "Total number of records")
  expect_equal(
    (res |> as.data.frame())[1, -1] |> as.character(),
    (trial |> dplyr::group_by(grade) |> dplyr::summarise(n = dplyr::n()))$n |> as.character()
  )

  # overall row labeling works
  res <- tbl_hierarchical_count(
    data = trial, hierarchies = trt, by = grade, overall_row = TRUE, label = list(overall = "Total rows")
  )
  expect_equal((res |> as.data.frame())[1, 1], "Total rows")

  # errors thrown when bad overall_row argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, hierarchies = trt, overall_row = "test")
  )
})

# tbl_hierarchical_count(label) ------------------------------------------------------------
test_that("tbl_hierarchical_count(label) works properly", {
  # creates table when label is passed
  res <- tbl_hierarchical_count(
    data = trial, hierarchies = c(stage, grade), label = list(stage = "My Stage", grade = "My Grade")
  )
  expect_snapshot(res |> as.data.frame())
  expect_snapshot_value(res$table_styling$header$label[4])

  # errors thrown when bad label argument passed
  expect_snapshot(
    error = TRUE,
    tbl_hierarchical_count(data = trial, hierarchies = c(stage, grade), label = "Stages")
  )
})
