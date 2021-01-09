test_that("tidy_add_header_rows() works as expected", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.sum)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$label,
    c("(Intercept)", "T Stage", "T2", "T3", "T4", "Grade", "I", "II",
      "Chemotherapy Treatment", "Drug A", "Grade * Chemotherapy Treatment",
      "I * Drug A", "II * Drug A")
  )
  expect_equivalent(
    res$term,
    c("(Intercept)", NA, "stage2", "stage3", "stage4", NA, "grade1",
      "grade2", NA, "trt1", NA, "grade1:trt1", "grade2:trt1")
  )
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
      TRUE, FALSE, FALSE)
  )
  expect_equivalent(
    res$var_nlevels,
    c(NA, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 2L, 2L, NA, NA, NA)
  )

  # show_single_row has an effect only on variables with one term (2 if a ref term)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables() %>%
    tidy_add_header_rows(show_single_row = everything(), quiet = TRUE)
  expect_equivalent(
    res$label,
    c("(Intercept)", "T Stage", "T2", "T3", "T4", "Grade", "I", "II",
      "Chemotherapy Treatment", "Grade * Chemotherapy Treatment", "I * Drug A",
      "II * Drug A")
  )
  expect_equivalent(
    res$term,
    c(
      "(Intercept)", NA, "stage2", "stage3", "stage4", NA, "grade1",
      "grade2", "trt1", NA, "grade1:trt1", "grade2:trt1"
    )
  )
  expect_equivalent(
    res$header_row,
    c(
      NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA, TRUE,
      FALSE, FALSE
    )
  )

  # with reference rows
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$label,
    c(
      "(Intercept)", "T Stage", "T1", "T2", "T3", "T4", "Grade",
      "I", "II", "III", "Chemotherapy Treatment", "Drug A", "Drug B",
      "Grade * Chemotherapy Treatment", "I * Drug A", "II * Drug A"
    )
  )
  expect_equivalent(
    res$term,
    c(
      "(Intercept)", NA, "stage1", "stage2", "stage3", "stage4",
      NA, "grade1", "grade2", "grade3", NA, "trt1", "trt2", NA, "grade1:trt1",
      "grade2:trt1"
    )
  )
  expect_equivalent(
    res$header_row,
    c(
      NA, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE
    )
  )

  # no warning with an intercept only model
  mod <- lm(mpg ~ 1, mtcars)
  expect_warning(
    mod %>% tidy_and_attach() %>% tidy_add_header_rows(),
    NA
  )

  # header row for all categorical variable (even if no reference row)
  # and if interaction with a categorical variable
  # (except if )
  mod <- lm(age ~ factor(response) * marker + trt, gtsummary::trial)
  res <- mod %>% tidy_and_attach() %>% tidy_add_header_rows(show_single_row = "trt")
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, NA, NA, TRUE, FALSE)
  )

  # show_single_row could be apply to an interaction variable
  mod <- lm(age ~ factor(response) * marker, gtsummary::trial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_header_rows(show_single_row = "factor(response):marker")
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, NA, NA)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows(show_single_row = "factor(response):marker")
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, NA, NA)
  )
  expect_equivalent(
    res$var_label,
    c("(Intercept)", "factor(response)", "factor(response)", "factor(response)",
      "Marker Level (ng/mL)", "factor(response) * Marker Level (ng/mL)")
  )

  # no standard name
  mod <- lm(hp ~ `miles per gallon`,
     mtcars %>% dplyr::rename(`miles per gallon` = mpg))
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$header_row,
    c(NA, NA)
  )
  mod <- lm(hp ~ `cyl as factor`,
            mtcars %>% dplyr::mutate(`cyl as factor` = factor(cyl)))
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE)
  )
})

test_that("test tidy_add_header_rows() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_add_header_rows())

  # warning if applied twice
  expect_message(
    mod %>% tidy_and_attach() %>%
      tidy_add_header_rows() %>%
      tidy_add_header_rows()
  )
})

test_that("tidy_add_header_rows() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age + trt, data = gtsummary::trial, trace = FALSE)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$header_row,
    c(
      NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, TRUE, FALSE,
      FALSE, NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, TRUE, FALSE,
      FALSE
    )
  )
  expect_equivalent(
    res$label,
    c(
      "(Intercept)", "T Stage", "T1", "T2", "T3", "T4",
      "Marker Level (ng/mL)", "Age", "Chemotherapy Treatment",
      "Drug A", "Drug B", "(Intercept)", "T Stage", "T1", "T2",
      "T3", "T4", "Marker Level (ng/mL)", "Age",
      "Chemotherapy Treatment", "Drug A", "Drug B"
    )
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows(show_single_row = everything(), quiet = TRUE)
  expect_equivalent(
    res$header_row,
    c(
      NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, TRUE,
      FALSE, FALSE, FALSE, FALSE, NA, NA, NA
    )
  )
  expect_equivalent(
    res$label,
    c(
      "(Intercept)", "T Stage", "T1", "T2", "T3", "T4",
      "Marker Level (ng/mL)", "Age", "Chemotherapy Treatment",
      "(Intercept)", "T Stage", "T1", "T2", "T3", "T4",
      "Marker Level (ng/mL)", "Age", "Chemotherapy Treatment"
    )
  )
})


test_that("test tidy_add_header_rows() bad single row request", {
  mod <- lm(mpg ~ hp + factor(cyl) + factor(am), mtcars) %>%
    tidy_and_attach() %>%
    tidy_identify_variables()

  expect_message(
    tidy_add_header_rows(mod, show_single_row = "factor(cyl)")
  )
  expect_error(
    tidy_add_header_rows(mod, show_single_row = "factor(cyl)", strict = TRUE)
  )
})
