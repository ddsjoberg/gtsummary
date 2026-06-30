skip_on_cran()
skip_if_pkg_not_installed(c("broom", "broom.helpers"))

test_that("modify_abbreviation()", {
  expect_silent(
    tbl <-
      tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      modify_abbreviation("Q3 = Third Quartile")
  )
  expect_equal(
    tbl$table_styling$abbreviation,
    dplyr::tribble(
      ~column,               ~abbreviation, ~text_interpret,
      NA_character_, "Q1 = First Quartile",        "gt::md",
      NA_character_, "Q3 = Third Quartile",        "gt::md"
    )
  )
})


test_that("remove_abbreviation()", {
  expect_silent(
    tbl <-
      tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      modify_abbreviation("Q3 = Third Quartile") |>
      remove_abbreviation("Q3 = Third Quartile")
  )
  expect_equal(
    tbl$table_styling$abbreviation,
    dplyr::tribble(
      ~column,               ~abbreviation, ~text_interpret,
      NA_character_, "Q1 = First Quartile",        "gt::md",
    )
  )

  # test all abbreviations removed by default
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      modify_abbreviation("Q3 = Third Quartile") |>
      remove_abbreviation() |>
      getElement("table_styling") |>
      getElement("abbreviation") |>
      nrow(),
    0L
  )
})

test_that("remove_abbreviation() messaging", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      remove_abbreviation("Q3 = Third Quartile")
  )

  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile") |>
      remove_abbreviation("Q3 = Third Quartile")
  )
})

test_that("modify_abbreviation(prefix, sep1, sep2) customizes the source note", {
  # plural prefix + custom separators
  tbl <- tbl_summary(trial, include = marker) |>
    modify_abbreviation("Q1 = First Quartile", prefix = c("Abbr", "Abbrs"), sep1 = " - ", sep2 = "; ") |>
    modify_abbreviation("Q3 = Third Quartile")
  expect_equal(
    as.character(as_gt(tbl)$`_source_notes`[[1]]),
    "Abbrs - Q1 = First Quartile; Q3 = Third Quartile"
  )

  # singular prefix when a single abbreviation is present
  tbl_one <- tbl_summary(trial, include = marker) |>
    modify_abbreviation("Q1 = First Quartile", prefix = c("Abbr", "Abbrs"))
  expect_equal(
    as.character(as_gt(tbl_one)$`_source_notes`[[1]]),
    "Abbr: Q1 = First Quartile"
  )

  # empty prefix omits the leading text and sep1
  tbl_empty <- tbl_summary(trial, include = marker) |>
    modify_abbreviation("Q1 = First Quartile", prefix = c("", ""))
  expect_equal(
    as.character(as_gt(tbl_empty)$`_source_notes`[[1]]),
    "Q1 = First Quartile"
  )
})

test_that("modify_abbreviation(prefix, sep1, sep2) persisted as attributes", {
  tbl <- tbl_summary(trial, include = marker) |>
    modify_abbreviation("Q1 = First Quartile", prefix = c("Abbr", "Abbrs"), sep1 = " - ", sep2 = "; ")
  expect_equal(attr(tbl$table_styling$abbreviation, "prefix"), c("Abbr", "Abbrs"))
  expect_equal(attr(tbl$table_styling$abbreviation, "sep1"), " - ")
  expect_equal(attr(tbl$table_styling$abbreviation, "sep2"), "; ")

  # subsequent calls without prefix/sep do not reset previously set values
  tbl2 <- tbl |> modify_abbreviation("Q3 = Third Quartile")
  expect_equal(attr(tbl2$table_styling$abbreviation, "prefix"), c("Abbr", "Abbrs"))
  expect_equal(attr(tbl2$table_styling$abbreviation, "sep1"), " - ")
  expect_equal(attr(tbl2$table_styling$abbreviation, "sep2"), "; ")

  # remove_abbreviation() preserves the attributes
  tbl3 <- tbl2 |> remove_abbreviation("Q3 = Third Quartile")
  expect_equal(attr(tbl3$table_styling$abbreviation, "prefix"), c("Abbr", "Abbrs"))
  expect_equal(attr(tbl3$table_styling$abbreviation, "sep1"), " - ")
  expect_equal(attr(tbl3$table_styling$abbreviation, "sep2"), "; ")

  # removing all abbreviations preserves the attributes
  tbl4 <- tbl2 |> remove_abbreviation()
  expect_equal(attr(tbl4$table_styling$abbreviation, "prefix"), c("Abbr", "Abbrs"))
  expect_equal(attr(tbl4$table_styling$abbreviation, "sep1"), " - ")
  expect_equal(attr(tbl4$table_styling$abbreviation, "sep2"), "; ")
})

test_that("modify_abbreviation() defaults controlled by theme elements", {
  with_gtsummary_theme(
    list(
      "modify_abbreviation-arg:prefix" = c("Key", "Keys"),
      "modify_abbreviation-arg:sep1" = " = ",
      "modify_abbreviation-arg:sep2" = " | "
    ),
    {
      tbl <- tbl_summary(trial, include = marker) |>
        modify_abbreviation("Q1 = First Quartile") |>
        modify_abbreviation("Q3 = Third Quartile")
      expect_equal(
        as.character(as_gt(tbl)$`_source_notes`[[1]]),
        "Keys = Q1 = First Quartile | Q3 = Third Quartile"
      )
    }
  )

  # an explicit argument overrides the theme element
  with_gtsummary_theme(
    list("modify_abbreviation-arg:sep2" = " | "),
    {
      tbl <- tbl_summary(trial, include = marker) |>
        modify_abbreviation("Q1 = First Quartile", sep2 = " / ") |>
        modify_abbreviation("Q3 = Third Quartile")
      expect_equal(
        as.character(as_gt(tbl)$`_source_notes`[[1]]),
        "Abbreviations: Q1 = First Quartile / Q3 = Third Quartile"
      )
    }
  )
})

test_that("modify_abbreviation() translates only the default prefix", {
  # the built-in default prefix is translated
  with_gtsummary_theme(theme_gtsummary_language("fr"), {
    tbl <- tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile")
    expect_equal(
      as.character(as_gt(tbl)$`_source_notes`[[1]]),
      "Abr\u00e9viation: Q1 = First Quartile"
    )
  })

  # a user-supplied prefix is used literally (not translated)
  with_gtsummary_theme(theme_gtsummary_language("fr"), {
    tbl <- tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile", prefix = c("Abbreviation", "Abbreviations"))
    expect_equal(
      as.character(as_gt(tbl)$`_source_notes`[[1]]),
      "Abbreviation: Q1 = First Quartile"
    )
  })
})

test_that("modify_abbreviation(prefix, sep1, sep2) input checks", {
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile", prefix = "one")
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile", sep1 = 1)
  )
  expect_snapshot(
    error = TRUE,
    tbl_summary(trial, include = marker) |>
      modify_abbreviation("Q1 = First Quartile", sep2 = 1)
  )
})

test_that("modify_abbreviation(text_interpret = 'none') stores identity", {
  # accepts "none" and stores the `identity` interpret function (#1987)
  tbl <- tbl_summary(trial, include = marker) |>
    modify_abbreviation("Q1 = First Quartile", text_interpret = "none")
  expect_equal(
    tbl$table_styling$abbreviation$text_interpret,
    "identity"
  )

  # invalid values are rejected
  expect_error(
    modify_abbreviation(tbl_summary(trial, include = marker), "x = y", text_interpret = "latex")
  )
})
