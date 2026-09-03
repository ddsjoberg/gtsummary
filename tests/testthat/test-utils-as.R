# `.table_styling_expr_to_row_number()` resolves the `rows` predicates stored in
# `x$table_styling` to row numbers with hand-vectorized code rather than
# dplyr/tidyr verbs. These tests pin the properties that are easy to lose when
# that code is optimized further:
#
#   1. the schema (names, types, class) of every styling table it rewrites,
#      including when the input is empty,
#   2. last-write-wins within a styling table,
#   3. `NA` predicates being dropped rather than propagated into `NA` rows,
#   4. locale-independent ordering of the abbreviation footnote.

skip_on_cran()

test_that(".table_styling_expr_to_row_number() returns a stable schema", {
  # both a styled table and one whose styling tables are empty must come back
  # with the same names/types, since the downstream `as_*()` code indexes them
  # by name and binds them into typed data frames
  styled <-
    trial |>
    tbl_summary(include = c(age, grade), by = trt) |>
    bold_labels() |>
    modify_indent(columns = label, rows = row_type == "level", indent = 4L) |>
    modify_missing_symbol(symbol = "-", columns = all_stat_cols(), rows = TRUE) |>
    modify_footnote_body(footnote = "note", columns = label, rows = variable == "age") |>
    modify_abbreviation("Q1 = First Quartile") |>
    modify_column_merge(pattern = "{stat_1} / {stat_2}")

  emptied <- styled
  for (element in c(
    "text_format", "indent", "fmt_missing", "fmt_fun", "post_fmt_fun",
    "cols_merge", "footnote_body", "footnote_header", "footnote_spanning_header",
    "spanning_header", "abbreviation", "source_note"
  )) {
    emptied$table_styling[[element]] <- emptied$table_styling[[element]][0L, ]
  }

  expected <- list(
    text_format = c(column = "character", row_numbers = "list", format_type = "character", undo_text_format = "logical"),
    indent = c(column = "character", n_spaces = "integer", row_numbers = "list"),
    fmt_missing = c(column = "character", symbol = "character", row_numbers = "list"),
    fmt_fun = c(column = "character", fmt_fun = "list", row_numbers = "list"),
    post_fmt_fun = c(column = "character", fmt_fun = "list", row_numbers = "list"),
    cols_merge = c(column = "character", pattern = "character", rows = "list"),
    footnote_body = c(column = "character", row_numbers = "integer", text_interpret = "character", footnote = "character")
  )

  for (tbl in list(styled, emptied)) {
    styling <- .table_styling_expr_to_row_number(tbl)$table_styling
    for (element in names(expected)) {
      expect_equal(
        vapply(styling[[element]], \(x) class(x)[1], character(1L)),
        expected[[element]],
        info = element
      )
    }
    # `as_gt()` applies its own `rowwise()` and no other consumer groups this
    # frame, so it stays a plain tibble. `expect_equal()` on `class()` rather
    # than `expect_s3_class()`, which a `rowwise_df` would also satisfy.
    expect_equal(class(styling$cols_merge), c("tbl_df", "tbl", "data.frame"))
  }
})

test_that(".table_styling_expr_to_row_number() keeps the most recent instruction", {
  # each styling table stacks instructions and the last one wins. The
  # de-duplication happens after the `rows` predicates are expanded to row
  # numbers, so overlapping predicates must resolve per row, not per instruction.
  tbl <-
    trial |>
    tbl_summary(include = c(age, grade)) |>
    modify_indent(columns = label, rows = row_type == "level", indent = 2L) |>
    modify_indent(columns = label, rows = variable == "grade", indent = 6L) |>
    modify_missing_symbol(symbol = "first", columns = stat_0, rows = TRUE) |>
    modify_missing_symbol(symbol = "second", columns = stat_0, rows = variable == "age")

  styling <- .table_styling_expr_to_row_number(tbl)$table_styling
  body <- tbl$table_body

  # `grade` levels were indented twice; only the later 6-space instruction applies
  grade_levels <- which(body$variable == "grade" & body$row_type == "level")
  indent_6 <- styling$indent$row_numbers[[which(styling$indent$n_spaces == 6L)]]
  expect_setequal(intersect(grade_levels, indent_6), grade_levels)
  indent_2 <- unlist(styling$indent$row_numbers[which(styling$indent$n_spaces == 2L)])
  expect_length(intersect(grade_levels, indent_2), 0L)

  # zero-width indents are dropped entirely
  expect_false(0L %in% styling$indent$n_spaces)

  # the `age` rows take the later missing symbol, everything else the earlier one
  age_rows <- which(body$variable == "age")
  second <- styling$fmt_missing$row_numbers[[which(styling$fmt_missing$symbol == "second")]]
  first <- styling$fmt_missing$row_numbers[[which(styling$fmt_missing$symbol == "first")]]
  expect_setequal(second, age_rows)
  expect_length(intersect(first, age_rows), 0L)
})

test_that(".table_styling_expr_to_row_number() drops undone text formatting", {
  tbl <-
    trial |>
    tbl_summary(include = c(age, grade)) |>
    bold_labels() |>
    remove_bold(columns = label, rows = variable == "age")

  styling <- .table_styling_expr_to_row_number(tbl)$table_styling
  bold_rows <- unlist(styling$text_format$row_numbers[styling$text_format$format_type == "bold"])

  expect_false(any(styling$text_format$undo_text_format))
  expect_length(intersect(bold_rows, which(tbl$table_body$variable == "age")), 0L)
})

test_that(".table_styling_expr_to_row_number() drops NA predicates instead of propagating them", {
  # `dplyr::filter()` drops rows whose predicate is `NA`; base `[` keeps them as
  # all-`NA` rows. The styling tables are subset with `[` for speed, so an `NA`
  # slipping into one of these columns (e.g. via a `bind_rows()` in
  # `tbl_merge()`/`tbl_stack()` over an object missing the column) must not
  # produce a phantom row -- or, for `spanning_header`, an error out of
  # `seq_len(max(level))`.
  base_tbl <- trial |> tbl_summary(include = c(age, grade), by = trt)

  tbl <- base_tbl |> modify_indent(columns = label, rows = row_type == "level", indent = 4L)
  tbl$table_styling$indent$n_spaces[1] <- NA_integer_
  indent <- .table_styling_expr_to_row_number(tbl)$table_styling$indent
  expect_false(anyNA(indent$column))
  expect_false(anyNA(indent$n_spaces))

  tbl <- base_tbl |> bold_labels()
  tbl$table_styling$text_format$undo_text_format[1] <- NA
  text_format <- .table_styling_expr_to_row_number(tbl)$table_styling$text_format
  expect_false(anyNA(text_format$column))
  expect_false(anyNA(text_format$format_type))

  tbl <- base_tbl |>
    modify_footnote_body(footnote = "note", columns = label, rows = variable == "age")
  tbl$table_styling$footnote_body$remove[1] <- NA
  footnote_body <- .table_styling_expr_to_row_number(tbl)$table_styling$footnote_body
  expect_false(anyNA(footnote_body$column))
  expect_false(anyNA(footnote_body$row_numbers))

  tbl <- base_tbl |> modify_spanning_header(all_stat_cols() ~ "**Treatment**")
  tbl$table_styling$spanning_header$remove[1] <- NA
  expect_silent(
    spanning_header <- .table_styling_expr_to_row_number(tbl)$table_styling$spanning_header
  )
  expect_false(anyNA(spanning_header$column))

  tbl <- base_tbl |> modify_source_note("a source note")
  tbl$table_styling$source_note$remove[1] <- NA
  source_note <- .table_styling_expr_to_row_number(tbl)$table_styling$source_note
  expect_false(anyNA(source_note$source_note))
})

test_that(".table_styling_expr_to_row_number() orders abbreviations in the C locale", {
  tbl <-
    trial |>
    tbl_summary(include = age) |>
    modify_abbreviation("zeta = z") |>
    modify_abbreviation("Alpha = a") |>
    modify_abbreviation("beta = b") |>
    modify_abbreviation("Gamma = g")

  # duplicated abbreviations collapse to the most recently added entry
  tbl2 <- tbl |> modify_abbreviation("Alpha = a second time")
  abbreviation2 <- .table_styling_expr_to_row_number(tbl2)$table_styling$abbreviation
  expect_equal(anyDuplicated(abbreviation2$abbreviation), 0L)

  # The abbreviation footnote is sorted, and the sort must not depend on the
  # user's locale. `dplyr::arrange()` always collates in the C locale, base
  # `order()` collates in the user's -- so `Gamma` sorts before `beta` in the
  # former and after it in the latter. testthat pins `LC_COLLATE=C`, which is
  # precisely the setting where a locale-sensitive sort still looks correct, so
  # a non-C collation has to be requested explicitly for this to be a real test.
  mixed <- c("zeta = z", "Alpha = a", "beta = b", "Gamma = g")
  original <- Sys.getlocale("LC_COLLATE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_COLLATE", original)))
  for (locale in c("en_US.UTF-8", "English_United States.1252", "en_US")) {
    suppressWarnings(try(Sys.setlocale("LC_COLLATE", locale), silent = TRUE))
    if (!identical(mixed[order(mixed)], mixed[order(mixed, method = "radix")])) break
  }
  skip_if(
    identical(mixed[order(mixed)], mixed[order(mixed, method = "radix")]),
    "no collation locale available that differs from the C locale"
  )

  abbreviation <- .table_styling_expr_to_row_number(tbl)$table_styling$abbreviation
  expect_equal(
    as.character(abbreviation$abbreviation),
    c("Alpha = a", "Gamma = g", "beta = b", "zeta = z")
  )
})

test_that(".table_styling_expr_to_row_number() honors an NA cols_merge pattern", {
  # an `NA` pattern is the hold-over syntax for undoing a merge. Because the
  # most recent instruction per column wins, an `NA` in that position must drop
  # the column rather than let an older pattern resurface.
  tbl <-
    lm(marker ~ age, trial) |>
    tbl_regression() |>
    modify_column_merge(pattern = "{estimate} ({conf.low}, {conf.high})")
  merge_instr <- tbl$table_styling$cols_merge

  # a later NA pattern undoes the merge entirely
  tbl_undone <- tbl
  tbl_undone$table_styling$cols_merge <- dplyr::bind_rows(
    merge_instr,
    dplyr::tibble(column = merge_instr$column[1], rows = merge_instr$rows[1], pattern = NA_character_)
  )
  expect_equal(nrow(.table_styling_expr_to_row_number(tbl_undone)$table_styling$cols_merge), 0L)

  # but an NA that is *superseded* by a later pattern does not remove it
  tbl_redone <- tbl
  tbl_redone$table_styling$cols_merge <- dplyr::bind_rows(
    dplyr::tibble(column = merge_instr$column[1], rows = merge_instr$rows[1], pattern = NA_character_),
    merge_instr
  )
  redone <- .table_styling_expr_to_row_number(tbl_redone)$table_styling$cols_merge
  expect_equal(redone$pattern, merge_instr$pattern)
})

test_that(".construct_summary_footnote() memoization does not collapse distinct labels", {
  # the glue evaluation is memoized on the (statistic template, stat label
  # mapping) pair so that homogeneous tables evaluate it once. Two variables
  # sharing a template but carrying different `stat_label`s must not share an
  # entry, and a `NULL`/longer label must not misalign the cache key.
  tbl <- trial |> tbl_summary(include = c(age, marker), statistic = ~"{mean} ({sd})")
  card <- tbl$cards$tbl_summary

  statistic <- list(age = "{mean} ({sd})", marker = "{mean} ({sd})")
  type <- list(age = "continuous", marker = "continuous")

  expect_equal(
    .construct_summary_footnote(card, c("age", "marker"), statistic, type),
    "Mean (SD)"
  )

  relabeled <- card
  relabeled$stat_label[relabeled$variable == "marker" & relabeled$stat_name == "mean"] <- "Average"
  expect_equal(
    .construct_summary_footnote(relabeled, c("age", "marker"), statistic, type),
    "Mean (SD); Average (SD)"
  )

  # an NA label still resolves, and does not shift the key's name/value pairing
  na_label <- card
  na_label$stat_label[na_label$variable == "age" & na_label$stat_name == "sd"] <- NA_character_
  expect_equal(
    .construct_summary_footnote(na_label, c("age", "marker"), statistic, type),
    c("Mean (NA); Mean (SD)")
  )

  # `continuous2` variables carry their labels in the table body, not a footnote
  expect_null(
    .construct_summary_footnote(
      card, c("age", "marker"), statistic,
      list(age = "continuous2", marker = "continuous2")
    )
  )
})
