skip_if_not(is_pkg_installed(c("broom.helpers", "broom", "survival"), reference_pkg = "gtsummary"))

test_that("tbl_uvregression(method)", {
  # first passing method three way, and checking equality of results
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = age,
        include = c(marker, grade),
        method = lm
      )
  )
  expect_silent(
    tbl2 <-
      tbl_uvregression(
        trial,
        y = age,
        include = c(marker, grade),
        method = "lm"
      )
  )
  expect_silent(
    tbl3 <-
      tbl_uvregression(
        trial,
        y = age,
        include = c(marker, grade),
        method = stats::lm
      )
  )
  expect_equal(as.data.frame(tbl1), as.data.frame(tbl2))
  expect_equal(as.data.frame(tbl1), as.data.frame(tbl3))
  expect_snapshot(as.data.frame(tbl1))
})

test_that("tbl_uvregression(method.args)", {
  # testing typical use of the argument
  expect_silent(
    tbl1 <-
      tbl_uvregression(
        trial,
        y = response,
        method = glm,
        method.args = list(family = binomial),
        include = c(age, trt)
      )
  )
  # check Ns are correct
  expect_equal(
    tbl1$table_body$stat_n |> na.omit(),
    c(trial[c("response", "age")] |> na.omit() |> nrow(),
      trial[c("response", "trt")] |> na.omit() |> nrow()),
    ignore_attr = TRUE
  )
  # check model coefs are correct
  expect_equal(
    tbl1$table_body$estimate |> na.omit(),
    c(glm(response ~ age, trial, family = binomial) |> coef() |> getElement("age"),
      glm(response ~ trt, trial, family = binomial) |> coef() |> getElement("trtDrug B")),
    ignore_attr = TRUE
  )

  # # check with NSE argument
  # expect_silent(
  #   tbl2 <-
  #     tbl_uvregression(
  #       trial,
  #       y = survival::Surv(ttdeath, death),
  #       method = survival::coxph,
  #       method.args = list(id = response)
  #     )
  # )
})
