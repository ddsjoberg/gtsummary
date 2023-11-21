skip_on_cran()

# univariate regression models
t0 <-
  trial %>%
  dplyr::select(response, trt, grade, age) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
  )
# MVA logistic regression
t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

# MVA cox regression
t2 <-
  survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(
    exponentiate = TRUE
  )


# tbl_stack adjusted model
covars <- c("trt", "age")

# get model covariates adjusted by stage and grade
adj_mods <-
  purrr::map(
    covars, ~
      survival::coxph(
        as.formula(
          paste("survival::Surv(ttdeath, death) ~ grade + ", .x)
        ),
        trial
      ) %>%
        tbl_regression(
          include = all_of(.x),
          exponentiate = TRUE
        )
  )

# now get stage and grade models adjusted for each other
adj_mods[["grade_mod"]] <-
  survival::coxph(
    as.formula(paste("survival::Surv(ttdeath, death) ~ grade")),
    trial
  ) %>%
  tbl_regression(exponentiate = TRUE)

# stack all your adjusted models
t3 <- tbl_stack(adj_mods)


# putting all tables together
t4 <-
  tbl_merge(
    tbls = list(t0, t1, t2, t3),
    tab_spanner = c("UVA Tumor Response", "MVA Tumor Response", "MVA Time to Death", "TTD Adjusted for grade")
  )

t5 <-
  trial %>%
  dplyr::select(age, grade, response) %>%
  tbl_summary(missing = "no") %>%
  add_n()
t6 <-
  tbl_uvregression(
    trial %>% dplyr::select(ttdeath, death, age, grade, response),
    method = survival::coxph,
    y = survival::Surv(ttdeath, death),
    exponentiate = TRUE,
    hide_n = TRUE
  )

test_that("no errors/warnings with standard use", {
  expect_snapshot(t4 %>% as.data.frame())
  expect_error(t4, NA)
  expect_warning(t4, NA)
  expect_error(tbl_merge(tbls = list(t5, t6)), NA)
  expect_warning(tbl_merge(tbls = list(t5, t6)), NA)
})

test_that("number of rows the same after joining", {
  expect_true(nrow(t0$table_body) == nrow(t4$table_body))
  expect_true(nrow(t1$table_body) == nrow(t4$table_body))
  expect_true(nrow(t2$table_body) == nrow(t4$table_body))
  expect_true(nrow(t3$table_body) == nrow(t4$table_body))
})

test_that("tbl_merge throws errors", {
  expect_error(tbl_merge(t1), NULL)
  expect_error(tbl_merge(list(mtcars)), NULL)
  expect_error(tbl_merge(tbls = list(t5, t6), tab_spanner = c("Table")), NULL)
})

test_that("tbl_merge throws errors", {
  expect_equal(
    trial %>%
      split(.$trt) %>%
      purrr::map(tbl_summary, by = stage) %>%
      tbl_merge(tab_spanner = c("Drug A", "Drug B")) %>%
      purrr::pluck("table_styling", "header", "spanning_header") %>%
      unique(),
    c(NA, "Drug A", "Drug B")
  )
})

test_that("tbl_merge() column ordering", {
  t1 <- lm(marker ~ age, trial) %>% tbl_regression()
  t2 <- lm(marker ~ response, trial) %>% tbl_regression()
  t3 <- lm(marker ~ age, trial) %>% tbl_regression()

  expect_equal(
    tbl_merge(list(t1, t2, t3)) %>% as_tibble(col_labels = FALSE),
    tbl_merge(list(t1, t2, t3)) %>%
      as_tibble(col_labels = FALSE) %>%
      select(label, ends_with("_1"), ends_with("_2"), ends_with("_3"))
  )
  expect_snapshot(tbl_merge(list(t1, t2, t3)) %>% as.data.frame())
})

test_that("tbl_merge() no spanning header", {
  tbl <- lm(mpg ~ factor(cyl) + am, mtcars) %>% tbl_regression()

  expect_error(
    tbl_no_spanning <- tbl_merge(list(tbl, tbl), tab_spanner = FALSE),
    NA
  )
  expect_snapshot(tbl_no_spanning %>% as.data.frame())
  expect_true(
    is.na(tbl_no_spanning$table_styling$header$spanning_header) %>% all()
  )
})

test_that("tbl_merge() one table", {
  tbl <- lm(mpg ~ factor(cyl) + am, mtcars) %>% tbl_regression()

  expect_error(
    tbl_only_one <- tbl_merge(list(tbl)),
    NA
  )
  expect_snapshot(tbl_only_one %>% as.data.frame())
})

test_that("tbl_merge() errors are triggered", {
  tbl <-
    head(mtcars) %>%
    .create_gtsummary_object() %>%
    modify_column_unhide(everything())

  expect_error(
    tbl_merge(list(tbl, tbl)),
    "objects must have columns"
  )
})

test_that("tbl_merge with complicated tbl_stack + cols_merge", {
  theme_gtsummary_journal("jama")

  t3 <-
    trial[c("age", "grade", "response")] %>%
    tbl_summary(
      missing = "no",
      type = list(where(is.numeric) ~ "continuous2"),
      include = c(age, response)
    ) %>%
    add_n() %>%
    modify_header(stat_0 ~ "**Summary Statistics**")


  t4 <-
    tbl_uvregression(
      trial[c("ttdeath", "death", "age", "grade", "response")],
      method = survival::coxph,
      y = survival::Surv(ttdeath, death),
      exponentiate = TRUE,
      hide_n = TRUE,
      include = c(age, response)
    )

  expect_snapshot(
    tbl_merge(tbls = list(t3, t4)) %>%
      modify_spanning_header(everything() ~ NA) %>%
      as.data.frame()
  )
  reset_gtsummary_theme()
})
