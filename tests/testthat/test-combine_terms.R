test_that("combine_terms() works", {
  expect_silent(
    # Logistic Regression Example, LRT p-value
    glm(response ~ marker + I(marker^2) + grade,
        trial[c("response", "marker", "grade")] |> na.omit(), # keep complete cases only!
        family = binomial) |>
      tbl_regression(label = grade ~ "Grade", exponentiate = TRUE) |>
      # collapse non-linear terms to a single row in output using anova
      combine_terms(
        formula_update = . ~ . - marker - I(marker^2),
        label = "Marker (non-linear terms)",
        test = "LRT"
      )
  )
})
