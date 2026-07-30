# `{p_col}`/`{p_row}`/`{p_cell}` (and `{N_col}`/`{N_row}`/`{N_cell}`) in
# tbl_summary(statistic): percentages under all standard denominators are
# available in a single statistic string, independently of `percent=`.

# reference table_body built by the primary machinery, one denominator at a time
.ref_body <- function(include, statistic, percent = "column", type = NULL) {
  trial |>
    tbl_summary(
      by = trt,
      include = all_of(include),
      statistic = everything() ~ statistic,
      percent = percent,
      type = type,
      missing = "no"
    ) |>
    getElement("table_body")
}

# composite of reference stat columns, NA where the reference is NA (label rows)
.composite <- function(n, left, right) {
  ifelse(is.na(n), NA_character_, paste0(n, " (", left, "% / ", right, "%)"))
}

test_that("tbl_summary() combines multiple percent denominators in one statistic string", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = grade,
      statistic = all_categorical() ~ "{n} ({p_col}% / {p_row}%)",
      missing = "no"
    )

  body_n <- .ref_body("grade", "{n}")
  body_col <- .ref_body("grade", "{p}", percent = "column")
  body_row <- .ref_body("grade", "{p}", percent = "row")

  for (col in c("stat_1", "stat_2")) {
    expect_equal(
      tbl$table_body[[col]],
      .composite(body_n[[col]], body_col[[col]], body_row[[col]])
    )
  }
})

test_that("extra percent denominators work for dichotomous variables", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = response,
      statistic = all_dichotomous() ~ "{n} ({p}% / {p_row}%)",
      missing = "no"
    )

  body_n <- .ref_body("response", "{n}")
  body_col <- .ref_body("response", "{p}", percent = "column")
  body_row <- .ref_body("response", "{p}", percent = "row")

  for (col in c("stat_1", "stat_2")) {
    expect_equal(
      tbl$table_body[[col]],
      .composite(body_n[[col]], body_col[[col]], body_row[[col]])
    )
  }
})

test_that("{p_cell} and {N_row} statistics match their single-denominator references", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = grade,
      statistic = all_categorical() ~ "{n}/{N_row} ({p_cell}%)",
      missing = "no"
    )

  body_n <- .ref_body("grade", "{n}")
  body_N_row <- .ref_body("grade", "{N}", percent = "row")
  body_cell <- .ref_body("grade", "{p}", percent = "cell")

  for (col in c("stat_1", "stat_2")) {
    expect_equal(
      tbl$table_body[[col]],
      ifelse(
        is.na(body_n[[col]]),
        NA_character_,
        paste0(body_n[[col]], "/", body_N_row[[col]], " (", body_cell[[col]], "%)")
      )
    )
  }
})

test_that("{p_col} under percent='row' reproduces the column percentage", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = grade,
      statistic = all_categorical() ~ "{p_col}",
      percent = "row",
      missing = "no"
    )
  body_col <- .ref_body("grade", "{p}", percent = "column")

  for (col in c("stat_1", "stat_2")) {
    expect_equal(tbl$table_body[[col]], body_col[[col]])
  }
})

test_that("digits are honored for extra percent statistics", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = grade,
      statistic = all_categorical() ~ "{p_row}%",
      digits = grade ~ list(p_row = 1),
      missing = "no"
    )

  n_IA <- sum(trial$grade == "I" & trial$trt == "Drug A")
  n_I <- sum(trial$grade == "I")
  expect_equal(
    tbl$table_body$stat_1[tbl$table_body$label %in% "I"],
    paste0(style_number(100 * n_IA / n_I, digits = 1), "%")
  )
})

test_that("{p_row} without `by` is 100%", {
  tbl <- trial |>
    tbl_summary(
      include = grade,
      statistic = all_categorical() ~ "{p_row}",
      missing = "no"
    )
  expect_true(
    all(tbl$table_body$stat_0[tbl$table_body$row_type %in% "level"] %in% "100")
  )
})

test_that("extra percent statistics work with add_overall() and add_p()", {
  expect_error(
    tbl <- trial |>
      tbl_summary(
        by = trt,
        include = grade,
        statistic = all_categorical() ~ "{n} ({p}% / {p_row}%)",
        missing = "no"
      ) |>
      add_overall() |>
      add_p(),
    NA
  )
  # in the overall column the row percentage is trivially 100%
  expect_true(
    all(grepl(
      "/ 100%\\)$",
      tbl$table_body$stat_0[tbl$table_body$row_type %in% "level"]
    ))
  )
})

test_that("default footnote names the extra percent statistics", {
  tbl <- trial |>
    tbl_summary(
      by = trt,
      include = grade,
      statistic = all_categorical() ~ "{n} ({p_col}% / {p_row}%)",
      missing = "no"
    )
  expect_true(
    any(grepl(
      "n (column % / row %)",
      tbl$table_styling$footnote_header$footnote,
      fixed = TRUE
    ))
  )
})
