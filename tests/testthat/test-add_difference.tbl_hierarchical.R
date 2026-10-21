skip_on_cran()
skip_if_pkg_not_installed("withr")
skip_if_not(
  "diff_ard_hierarchical" %in% getNamespaceExports("cards"),
  message = "cards::diff_ard_hierarchical() is not available"
)

ADAE_subset <- cards::ADAE |>
  dplyr::filter(AESOC %in% unique(cards::ADAE$AESOC)[1:3])

# a two-arm subset for the `levels = NULL` default path
ADAE_2arm <- ADAE_subset |> dplyr::filter(TRTA %in% c("Placebo", "Xanomeline High Dose"))
ADSL_2arm <- cards::ADSL |> dplyr::filter(TRTA %in% c("Placebo", "Xanomeline High Dose"))

test_that("add_difference.tbl_hierarchical() works", {
  withr::local_options(list(width = 220))

  tbl <-
    tbl_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AEDECOD),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID
    )

  expect_silent(res <- add_difference(tbl, levels = c("Xanomeline High Dose", "Placebo")))

  # a new `estimate` column is added, placed last
  expect_equal(
    names(as.data.frame(res, col_labels = FALSE)) |> dplyr::last(),
    "estimate"
  )
  # the header is "Rate Difference"
  expect_equal(
    names(as.data.frame(res, col_labels = TRUE)) |> dplyr::last(),
    "**Rate Difference**"
  )
  # the stored ARD holds only the renamed `estimate` statistic
  expect_setequal(res$cards$add_difference$stat_name, "estimate")
  expect_true("add_difference" %in% names(res$call_list))

  # values equal the 'estimate' (rate differences) computed directly by cards, formatted with the default
  d <- cards::diff_ard_hierarchical(
    tbl$cards$tbl_hierarchical,
    levels = list(TRTA = "Xanomeline High Dose", TRTA = "Placebo")
  )
  soc <- unique(cards::ADAE$AESOC)[1]
  p_soc <- d$stat[[which(d$variable == "AESOC" &
    vapply(d$variable_level, \(z) as.character(z[[1]]), "") == soc)]] |> as.numeric()
  est_soc <- as.data.frame(res, col_labels = FALSE) |>
    dplyr::filter(label == soc) |>
    dplyr::pull("estimate")
  expect_equal(est_soc, paste0(style_number(p_soc, digits = 1, scale = 100), "%"))

  expect_snapshot(as.data.frame(res))
})

test_that("add_difference.tbl_hierarchical() default `levels = NULL` works with a two-level `by`", {
  tbl <-
    tbl_hierarchical(
      data = ADAE_2arm,
      variables = c(AESOC, AEDECOD),
      by = TRTA,
      denominator = ADSL_2arm,
      id = USUBJID
    )

  expect_silent(res <- add_difference(tbl))
  expect_true("estimate" %in% names(as.data.frame(res, col_labels = FALSE)))
  expect_setequal(res$cards$add_difference$stat_name, "estimate")
})

test_that("add_difference.tbl_hierarchical() `levels` order flips the sign", {
  tbl <-
    tbl_hierarchical(
      data = ADAE_subset, variables = c(AESOC, AEDECOD), by = TRTA,
      denominator = cards::ADSL, id = USUBJID
    )

  fwd <- add_difference(tbl, levels = c("Xanomeline High Dose", "Placebo"))
  rev <- add_difference(tbl, levels = c("Placebo", "Xanomeline High Dose"))

  num <- function(x) {
    as.data.frame(x, col_labels = FALSE)$estimate |>
      sub("%$", "", x = _) |>
      as.numeric()
  }
  expect_equal(num(fwd), -num(rev))
})

test_that("add_difference.tbl_hierarchical() respects custom `statistic` and `estimate_fun`", {
  tbl <-
    tbl_hierarchical(
      data = ADAE_subset, variables = c(AESOC, AEDECOD), by = TRTA,
      denominator = cards::ADSL, id = USUBJID
    )

  res <- add_difference(
    tbl,
    levels = c("Xanomeline High Dose", "Placebo"),
    statistic = "{estimate}",
    estimate_fun = label_style_number(digits = 3, scale = 100)
  )
  # no "%" suffix, 3 decimals
  est <- as.data.frame(res, col_labels = FALSE)$estimate
  expect_false(any(grepl("%", est)))
  expect_true(all(grepl("\\.\\d{3}$", est[!is.na(est)])))
})

test_that("add_difference.tbl_ard_hierarchical() works and uses the source ARD fmt_fun by default", {
  ard <-
    cards::ard_stack_hierarchical(
      data = ADAE_subset,
      variables = c(AESOC, AEDECOD),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID
    )

  tbl <- tbl_ard_hierarchical(cards = ard, variables = c(AESOC, AEDECOD), by = TRTA)

  # default: estimate_fun = NULL -> uses the diff ARD's fmt_fun (digits = 1, scale = 100)
  expect_silent(res <- add_difference(tbl, levels = c("Xanomeline High Dose", "Placebo")))
  est <- as.data.frame(res, col_labels = FALSE)$estimate
  est <- est[!is.na(est)]
  expect_true(all(grepl("\\.\\d{1}%$", est)))

  # an explicit estimate_fun overrides the source fmt_fun (here, 3 decimals)
  res1 <- add_difference(
    tbl,
    levels = c("Xanomeline High Dose", "Placebo"),
    estimate_fun = label_style_number(digits = 3, scale = 100)
  )
  est1 <- as.data.frame(res1, col_labels = FALSE)$estimate
  est1 <- est1[!is.na(est1)]
  expect_true(all(grepl("\\.\\d{3}%$", est1)))
})

test_that("add_difference.tbl_hierarchical() aligns correctly after a prior sort_hierarchical()", {
  tbl <-
    tbl_hierarchical(
      data = ADAE_subset, variables = c(AESOC, AEDECOD), by = TRTA,
      denominator = cards::ADSL, id = USUBJID
    )

  sorted <- sort_hierarchical(tbl)
  res <- add_difference(sorted, levels = c("Xanomeline High Dose", "Placebo"))

  # every level row has a difference (join is by hierarchy identity, not position)
  na_levels <- sum(is.na(res$table_body$estimate) & res$table_body$row_type == "level")
  expect_equal(na_levels, 0L)
})

test_that("sort_hierarchical() works after add_difference()", {
  tbl <-
    tbl_hierarchical(
      data = ADAE_subset, variables = c(AESOC, AEDECOD), by = TRTA,
      denominator = cards::ADSL, id = USUBJID
    )

  res <- add_difference(tbl, levels = c("Xanomeline High Dose", "Placebo"))
  expect_silent(sorted <- sort_hierarchical(res))
  expect_true("estimate" %in% names(sorted$table_body))
  expect_equal(
    sum(is.na(sorted$table_body$estimate) & sorted$table_body$row_type == "level"),
    0L
  )
})

test_that("add_difference.tbl_hierarchical() error messaging", {
  tbl <-
    tbl_hierarchical(
      data = ADAE_subset, variables = c(AESOC, AEDECOD), by = TRTA,
      denominator = cards::ADSL, id = USUBJID
    )

  # called twice
  res <- add_difference(tbl, levels = c("Xanomeline High Dose", "Placebo"))
  expect_error(add_difference(res), "already been called")

  # no `by` variable
  tbl_noby <-
    tbl_hierarchical(
      data = ADAE_subset, variables = c(AESOC, AEDECOD),
      denominator = cards::ADSL, id = USUBJID
    )
  expect_error(add_difference(tbl_noby), "not stratified")

  # invalid `levels`
  expect_error(add_difference(tbl, levels = "Placebo"), "length-two")
  expect_error(add_difference(tbl, levels = c("Placebo", "Placebo")), "distinct")
})
