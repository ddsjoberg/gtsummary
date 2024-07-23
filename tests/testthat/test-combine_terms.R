skip_on_cran()

lmod <- lm(
  age ~ marker + I(marker^2) + stage,
  trial[c("age", "marker", "stage")] |> na.omit()
)

mod_simple <- lm(
  age ~ stage,
  trial[c("age", "marker", "stage")] |> na.omit()
)

test_that("combine_terms works with standard use", {
  expect_silent(
    tbl <- tbl_regression(lmod, label = stage ~ "Stage") |>
      combine_terms(
        formula_update = . ~ . - marker - I(marker^2)
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 6)
  expect_equal(
    as_tibble(tbl)[1, ] |> unlist(use.names = FALSE),
    c("Marker Level (ng/mL)", NA, NA, ">0.9")
  )

  # testing anova p-value is correct
  expect_equal(
    tbl$table_body |>
      dplyr::slice(1) |>
      dplyr::pull(p.value),
    anova(lmod, mod_simple) |>
      as_tibble() |>
      dplyr::slice(dplyr::n()) |>
      dplyr::pull(`Pr(>F)`)
  )
})

test_that("combine_terms(label) works as expected", {
  tbl <- tbl_regression(lmod, label = stage ~ "Stage") |>
    combine_terms(
      formula_update = . ~ . - marker - I(marker^2),
      label = "Marker (non-linear terms)"
    )

  expect_equal(
    tbl$table_body$label[[1]],
    "Marker (non-linear terms)"
  )
})

test_that("combine_terms works with add_global_p", {
  expect_silent(
    tbl <- lmod |>
      tbl_regression() |>
      add_global_p() |>
      combine_terms(formula = . ~ . - marker - I(marker^2))
  )

  # correct label when unspecified
  expect_equal(
    tbl$table_body$label[[1]],
    "Marker Level (ng/mL)"
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 6)
  expect_equal(
    as_tibble(tbl)[1, -1] |> unlist(use.names = FALSE),
    c(NA, NA, ">0.9")
  )
})

test_that("combine_terms works with logistic regression models", {
  mod <- glm(
    response ~ age + marker + grade,
    data = trial |> na.omit(),
    family = "binomial"
  )

  expect_no_error(
    tbl <- mod |>
      tbl_regression(exponentiate = TRUE) |>
      combine_terms(
        formula_update = . ~ . - grade,
        test = "LRT"
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 3)
  expect_equal(
    as_tibble(tbl)[3, ] |> unlist(use.names = FALSE),
    c("Grade", NA, NA, ">0.9")
  )
})

test_that("combine_terms works with Cox models", {
  skip_if_not(is_pkg_installed("survival", reference_pkg = "gtsummary"))

  mod <- survival::coxph(
    survival::Surv(ttdeath, death) ~ age + stage,
    data = trial |> na.omit()
  )

  expect_silent(
    tbl <- mod |>
      tbl_regression() |>
      combine_terms(
        formula_update = . ~ . - stage
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 2)
  expect_equal(
    as_tibble(tbl)[2, ] |> unlist(use.names = FALSE),
    c("T Stage", NA, NA, "<0.001")
  )
})

test_that("combine_terms works with GEE models", {
  skip_if_not(is_pkg_installed("geepack", reference_pkg = "gtsummary"))

  mod <- geepack::geeglm(
    as.formula("weight ~ Diet + Time"),
    data = ChickWeight |> na.omit(),
    family = gaussian,
    id = Chick,
    corstr = "exchangeable"
  )

  # selected terms cannot be only terms in model - GEE does not work for comparison with null model
  expect_silent(
    tbl <- mod |>
      tbl_regression() |>
      combine_terms(formula_update = . ~ . - Diet)
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 2)
  expect_equal(
    as_tibble(tbl)[1, ] |> unlist(use.names = FALSE),
    c("Diet", NA, NA, "<0.001")
  )
})

test_that("combine_terms works when used in map/apply", {
  data <- data.frame(outcome = "marker", exp = FALSE, test = "F")

  expect_no_error(
    res <- data |>
      mutate(
        mod = map(outcome, ~ glm(as.formula(paste0(.x, " ~ age + stage")), data = trial, family = gaussian)),
        tbl = map2(mod, exp, ~ tbl_regression(.x, exponentiate = .y)),
        tbl2 = map2(tbl, test, ~ combine_terms(..1, formula_update = . ~ . - stage, test = ..2))
      ) |>
      invisible()
  )
})

test_that("combine_terms(quiet) causes deprecation warning", {
  lifecycle::expect_deprecated(
    tbl_regression(lmod, label = stage ~ "Stage") |>
      combine_terms(formula_update = . ~ . - marker - I(marker^2), quiet = TRUE)
  )
})

test_that("combine_terms catches expected errors", {
  # p-value calculation error
  expect_snapshot(
    lm(age ~ marker + stage, trial) |>
      tbl_regression() |>
      combine_terms(formula = . ~ . - marker),
    error = TRUE
  )

  # incorrect label format
  expect_snapshot(
    lm(age ~ marker + stage, trial) |>
      tbl_regression() |>
      combine_terms(formula = . ~ . - marker, label = c("marker", "marker2")),
    error = TRUE
  )

  # no p-value returned by anova
  expect_snapshot(
    lm(mpg ~ disp + am * factor(cyl), data = mtcars) |>
      tbl_regression() |>
      combine_terms(. ~ . - am),
    error = TRUE
  )
})
