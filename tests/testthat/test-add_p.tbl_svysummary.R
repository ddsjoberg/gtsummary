context("test-add_p.tbl_ssvyummary")
testthat::skip_on_cran()

if (require(survey)) {

  strial <- trial %>%
    survey::svydesign(data = ., ids = ~ 1, weights = ~1)
  d <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  test_that("add_p creates output without error/warning", {
    expect_error(
      tbl_svysummary(strial, by = grade) %>% add_p(),
      NA
    )

    expect_error(
      strial %>%
        tbl_svysummary(by = trt) %>%
        add_p(),
      NA
    )

    expect_warning(
      strial %>%
        tbl_svysummary(by = trt) %>%
        add_p(),
      NA
    )

    expect_message(
      strial %>%
        tbl_svysummary(by = trt) %>%
        add_p(),
      NA
    )

    expect_warning(
      tbl_svysummary(d, by = Survived) %>% add_p(),
      NA
    )

    expect_message(
      tbl_svysummary(d, by = Survived) %>% add_p(),
      NA
    )

  })

  test_that("add_p creates errors with bad args", {
    expect_error(
      tbl_svysummary(strial, by = trt) %>%
        add_p(pvalue_fun = mtcars),
      NULL
    )

    expect_error(
      tbl_svysummary(strial, by = grade, include = -response) %>%
        add_p(test = "chisq"),
      NULL
    )

    expect_error(
      tbl_svysummary(strial, by = grade, include = -response) %>%
        add_p(test = list(all_continuous() ~ "t.test")), # not adapted
      NULL
    )

    expect_error(
      tbl_svysummary(strial, by = grade, include = -response) %>%
        add_p(test = list(all_continuous() ~ "ttttessstttt")),
      NULL
    )
  })


  test_that("add_p works well", {
    expect_error(
      tbl_svysummary(strial, by = response) %>%
        add_p(test = list(
          trt ~ "svy.wald.test",
          age ~ "svy.t.test",
          marker ~ "svy.median.test",
          stage ~"svy.adj.wald.test",
          death ~ "svy.saddlepoint.test"
        )),
      NA
    )
  })

}
