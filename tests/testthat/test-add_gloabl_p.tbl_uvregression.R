test_that("add_global_p.tbl_uvregression(x)", {
  tbl <- trial |> tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  expect_silent(
    res <- tbl |> add_global_p()
  )

  expect_snapshot(res |> as.data.frame())

  # two model terms, two p values
  expect_equal(
    sum(!is.na(res$table_body$p.value)),
    2
  )

  # strangely not equal
  # expect_equal(
  #   res$table_body$p.value[1:2],
  #   (car::Anova(lm(marker ~ age + grade, trial)))$`Pr(>F)`
  # )

})

test_that("add_global_p.tbl_uvregression(include)", {
  tbl <- trial |> tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res1 <- tbl |> add_global_p(include = age)

  # one expected p-value for age
  expect_equal(
    sum(!is.na(res1$table_body$p.value)),
    1
  )
})

test_that("add_global_p.tbl_uvregression(type)", {
  tbl <- trial |> tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res2 <- tbl |> add_global_p(type = "II")

  # 4 expected p-values, 2 for each variable, 2 for each level of grade
  expect_equal(
    sum(!is.na(res2$table_body$p.value)),
    2
  )
})

test_that("add_global_p.tbl_uvregression(keep)", {
  tbl <- trial |> tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res3 <- tbl |> add_global_p(keep = TRUE)

  # 4 expected p-values, 2 for each variable, 2 for each level of grade
  expect_equal(
    sum(!is.na(res3$table_body$p.value)),
    4
  )
})

test_that("add_global_p.tbl_uvregression(anova_fun)", {
  tbl <- trial |> tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  res4 <- tbl |> add_global_p(anova_fun = cardx::ard_aod_wald_test)

  # two model terms, two p values
  expect_equal(
    sum(!is.na(res4$table_body$p.value)),
    2
  )

  # strangely not equal
  # expect_equal(
  #   res4$table_body$p.value[1:2],
  #   lm(marker ~ age + grade, trial) |>
  #     cardx::ard_aod_wald_test() |>
  #     dplyr::filter(variable %in% c("age", "grade") & stat_name == "p.value") |>
  #     dplyr::pull(stat) %>%
  #     unlist
  # )
})

test_that("add_global_p.tbl_uvregression(anova_fun) inappropriate anova function", {
  tbl <- trial |> tbl_uvregression(method = lm, y = marker, include = c("age", "grade"))

  broken_anova <- function(x) {
    x + 1
  }

  expect_error(
    res5 <- tbl |> add_global_p(anova_fun = broken_anova),
    regexp = "There was an error running `anova_fun`"
  )
})

test_that("geeglm model for add_global_p.tbl_uvregression()", {
  suppressMessages(
    res5 <- geepack::dietox |>
      dplyr::mutate(Cu = as.factor(Cu)) |>
      tbl_uvregression(
        method = geepack::geeglm,
        y = Weight,
        include = c("Cu", "Time"),
        method.args = list(family = poisson("identity"), id = Pig, corstr = "ar1"),
      ) |>
      add_global_p()
  )
  expect_snapshot(res5 %>% as.data.frame)
})

test_that("modify tidy_fun to not show p-values", {
  expect_silent(
    res6 <- trial |>
      tbl_uvregression(
        method = glm,
        y = response,
        include = c("age", "grade"),
        tidy_fun = \(x, ...) broom::tidy(x, ...) |> dplyr::select(-p.value)
      ) |>
      add_global_p()
  )
  expect_snapshot(res6 %>% as.data.frame)
})
