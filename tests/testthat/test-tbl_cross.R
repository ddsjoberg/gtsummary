skip_on_cran()

trial2 <- rbind(
  NA, # For missing stats
  trial[, c(1, 4)] # Useful for not specifying row and col
)

# tbl_cross(data) --------------------------------------------------------------
test_that("tbl_cross(data) works", {
  expect_snapshot(tbl_cross(trial2) |> as.data.frame()) # First and second cols
})

test_that("tbl_cross(data) errors properly", {
  # Errors thrown when bad data argument passed
  expect_snapshot(error = TRUE, tbl_cross())
  expect_snapshot(error = TRUE, tbl_cross(data = letters))
  expect_snapshot(error = TRUE, tbl_cross(data = dplyr::tibble()))
  expect_snapshot(error = TRUE, tbl_cross(data = data.frame()))
})

# tbl_cross(row, col) ----------------------------------------------------------
test_that("tbl_cross(row, col) works", {
  expect_snapshot(tbl_cross(trial, row = trt, col = grade) |> as.data.frame())
})

test_that("tbl_cross(row, col) errors properly", {
  # Errors thrown when bad row or col argument passed
  expect_snapshot(error = TRUE, tbl_cross(trial, row = trt))
  expect_snapshot(error = TRUE, tbl_cross(trial |> mutate("..total.." = 1), row = "..total..", col = trt))
  expect_snapshot(error = TRUE, tbl_cross(trial, col = trt))
  expect_snapshot(error = TRUE, tbl_cross(trial, row = trt, col = 1))
  expect_snapshot(error = TRUE, tbl_cross(trial, row = NULL, col = grade))
})

# tbl_cross(label) ----------------------------------------------------------
test_that("tbl_cross(label) works", {
  expect_silent(
    out <- tbl_cross(
      trial2,
      label = list(trt = "TRT", stage = "STAGE")
    )
  )
  expect_identical(out$table_body$var_label[1], "TRT")
  expect_identical(unique(out$table_styling$header$spanning_header)[2], "STAGE")
})

test_that("tbl_cross(label) errors properly", {
  # Errors thrown when bad label argument passed
  expect_snapshot(
    error = TRUE,
    tbl_cross(trial2, label = list(trt = letters))
  )

  expect_snapshot(
    error = TRUE,
    tbl_cross(trial2, label = letters)
  )

  expect_snapshot(
    error = TRUE,
    tbl_cross(trial2, label = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    tbl_cross(trial2, label = list(trt = NA))
  )
})

# tbl_cross(statistic) ---------------------------------------------------------
all_stats <- c("p", "n", "N", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss") |>
  sapply(function(var) sprintf("%s={%s}", var, var)) |>
  paste(collapse = " | ")

test_that("tbl_cross(statistic) works", {
  expect_silent(
    out <- tbl_cross(trial2,
              statistic = all_stats)
  )
  expect_equal(
    out$table_body$stat_0[4],
    "p=0.5 | n=1 | N=201 | N_miss=0 | N_nonmiss=201 | p_miss=0 | p_nonmiss=100" # n = 1 is unknown (missing)
  )
})

# tbl_cross(margin) ------------------------------------------------------------
test_that("tbl_cross(margin) works", {
  expect_snapshot(tbl_cross(trial2, margin = "column") |> as.data.frame())
})

test_that("tbl_cross(margin) errors properly", {
  expect_snapshot(error = TRUE, tbl_cross(trial2, margin = "columsadasn"))
  expect_snapshot(error = TRUE, tbl_cross(trial2, margin = 1))
})

# tbl_cross(percent) -----------------------------------------------------------
test_that("tbl_cross(percent) works", {
  expect_silent(out <- tbl_cross(trial2, percent = "row"))
  expect_true(all(grepl(out$table_body$stat_0[-1], pattern = "\\(100\\%\\)")))

  expect_silent(out <- tbl_cross(trial2, percent = "cell"))
  expect_true(grepl(out$table_body$stat_0[5], pattern = "\\(100\\%\\)"))

  expect_silent(out <- tbl_cross(trial2, percent = "column"))
  expect_true(all(grepl(out$table_body[5, -seq(5)] |> unlist(use.names = FALSE), pattern = "\\(100\\%\\)")))
})

test_that("tbl_cross(percent) errors properly", {
  expect_snapshot(error = TRUE, tbl_cross(trial2, percent = "columsadasn"))
  expect_snapshot(error = TRUE, tbl_cross(trial2, percent = 1))
})
