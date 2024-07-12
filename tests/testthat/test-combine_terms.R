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
    tbl$table_body$label[1],
    "Marker (non-linear terms)"
  )
})

test_that("combine_terms works with models with splines", {
  skip_if_not(is_pkg_installed("Hmisc", reference_pkg = "gtsummary"))

  mod_splines <- lm(
    age ~ Hmisc::rcspline.eval(marker, inclx = TRUE) + stage,
    trial[c("age", "marker", "stage")] |> na.omit()
  )

  expect_silent(
    tbl <- tbl_regression(mod_splines, label = stage ~ "Stage") |>
      combine_terms(
        formula_update = . ~ . - Hmisc::rcspline.eval(marker, inclx = TRUE),
        label = "Marker (non-linear terms)"
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 6)
  expect_equal(
    as_tibble(tbl)[1, -1] |> unlist(use.names = FALSE),
    c(NA, NA, "0.8")
  )

  # p-value calculated is correct
  expect_equal(
    tbl$table_body |>
      dplyr::slice(1) |>
      dplyr::pull(p.value),
    anova(mod_splines, mod_simple) |>
      as_tibble() |>
      dplyr::slice(dplyr::n()) |>
      dplyr::pull(`Pr(>F)`)
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
    tbl$table_body$label[1],
    "Marker Level (ng/mL)"
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 6)
  expect_equal(
    as_tibble(tbl)[1, -1] |> unlist(use.names = FALSE),
    c(NA, NA, "0.8")
  )
})

test_that("combine_terms works with logistic regression models", {
  skip_if_not(is_pkg_installed("Hmisc", reference_pkg = "gtsummary"))

  mod <- glm(
    response ~ age + marker + sp2marker + sp3marker,
    data = trial |>
      bind_cols(
        Hmisc::rcspline.eval(trial$marker, nk = 4, inclx = FALSE, norm = 0) |>
          as.data.frame() |>
          set_names("sp2marker", "sp3marker")
      ) |>
      na.omit(),
    family = "binomial"
  )

  expect_no_error(
    tbl <- mod |>
      tbl_regression(exponentiate = TRUE) |>
      combine_terms(
        formula_update = . ~ . - marker - sp2marker - sp3marker,
        test = "LRT"
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 2)
  expect_equal(
    as_tibble(tbl)[2, ] |> unlist(use.names = FALSE),
    c("Marker Level (ng/mL)", NA, NA, "0.5")
  )
})

test_that("combine_terms works with Cox models", {
  skip_if_not(is_pkg_installed(c("Hmisc", "survival"), reference_pkg = "gtsummary"))

  mod <- survival::coxph(
    survival::Surv(ttdeath, death) ~ grade + Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0),
    data = trial |> na.omit()
  )

  expect_silent(
    tbl <- mod |>
      tbl_regression() |>
      combine_terms(
        formula_update = . ~ . - Hmisc::rcspline.eval(marker, nk = 4, inclx = TRUE, norm = 0),
        label = "Marker"
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 5)
  expect_equal(
    as_tibble(tbl)[5, ] |> unlist(use.names = FALSE),
    c("Marker", NA, NA, "0.7")
  )
})

test_that("combine_terms works with GEE models", {
  skip_if_not(is_pkg_installed(c("Hmisc", "geepack"), reference_pkg = "gtsummary"))

  mod <- geepack::geeglm(
    as.formula("weight ~ Diet + Time + sp2Time + sp3Time"),
    data = ChickWeight |>
      bind_cols(
        Hmisc::rcspline.eval(ChickWeight$Time, nk = 4, inclx = FALSE, norm = 0) |>
          as.data.frame() |>
          set_names("sp2Time", "sp3Time")
      ),
    family = gaussian,
    id = Chick,
    corstr = "exchangeable"
  )

  # selected terms cannot be only terms in model - GEE does not work for comparison with null model
  expect_silent(
    tbl <- mod |>
      tbl_regression() |>
      combine_terms(
        formula_update = . ~ . - Time - sp2Time - sp3Time
      )
  )

  # correct row contents
  expect_equal(nrow(tbl$table_body), 6)
  expect_equal(
    as_tibble(tbl)[5, ] |> unlist(use.names = FALSE),
    c("Time", NA, NA, "<0.001")
  )
})

test_that("combine_terms works when used in map/apply", {
  data <- tibble(outcome = "marker", exp = FALSE, test = "F")

  expect_no_error(
    res <- data |>
      mutate(
        mod = map(outcome, ~ glm(as.formula(paste0(.x, " ~ age + stage")), data = trial, family = gaussian)),
        tbl = map2(mod, exp, ~ tbl_regression(.x, exponentiate = .y)),
        tbl2 = map2(tbl, test, ~ combine_terms(..1, formula_update = . ~ . - stage, test = ..2))
      )
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
