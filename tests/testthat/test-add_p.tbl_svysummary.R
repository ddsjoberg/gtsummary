context("test-add_p.tbl_svyummary")
testthat::skip_on_cran()

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


test_that("add_p creates output without error/warning with continuous2", {
  expect_error(
    tbl_svysummary(strial, by = grade, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_error(
    strial %>%
      tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_warning(
    strial %>%
      tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_message(
    strial %>%
      tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p(),
    NA
  )

  expect_warning(
    tbl_svysummary(d, by = Survived, type = all_continuous() ~ "continuous2") %>% add_p(),
    NA
  )

  expect_message(
    tbl_svysummary(d, by = Survived, type = all_continuous() ~ "continuous2") %>% add_p(),
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
      add_p(test = list(all_continuous() ~ "ttttessstttt")),
    NULL
  )
})


test_that("add_p works well", {
  # creating version with explicit NA factor levels for response variable
  strial2 <- strial
  strial2$variables$response <- factor(strial2$variables$response) %>% forcats::fct_explicit_na()

  expect_error(
    tbl1 <-
      tbl_svysummary(strial, by = response) %>%
      add_p(test = list(
        age ~ "svy.t.test",
        marker ~ "svy.wilcox.test",
        trt ~ "svy.chisq.test",
        stage ~"svy.adj.chisq.test",
        death ~ "svy.wald.test"
      )),
    NA
  )

  expect_equivalent(
    dplyr::filter(tbl1$meta_data, variable == "age")$p.value,
    survey::svyttest(age ~ forcats::fct_explicit_na(factor(response)), strial)$p.value %>% rlang::set_names(NULL)
  )

  expect_equivalent(
    dplyr::filter(tbl1$meta_data, variable == "marker")$p.value,
    survey::svyranktest(marker ~ response, strial2, test = "wilcoxon")$p.value %>% rlang::set_names(NULL)
  )

  expect_equivalent(
    dplyr::filter(tbl1$meta_data, variable == "trt")$p.value,
    survey::svychisq(~trt + response, strial2, statistic = "F")$p.value %>% rlang::set_names(NULL)
  )

  expect_equivalent(
    dplyr::filter(tbl1$meta_data, variable == "stage")$p.value,
    survey::svychisq(~stage + response, strial2, statistic = "Chisq")$p.value %>% rlang::set_names(NULL)
  )


  expect_equivalent(
    dplyr::filter(tbl1$meta_data, variable == "death")$p.value,
    survey::svychisq(~death + response, strial2, statistic = "Wald")$p.value %>% rlang::set_names(NULL)
  )



  expect_error(
    tbl2 <-
      tbl_svysummary(strial, by = response) %>%
      add_p(test = list(
        age ~ "svy.vanderwaerden.test",
        ttdeath ~ "svy.kruskal.test",
        marker ~ "svy.median.test",
        trt ~ "svy.adj.wald.test",
        stage ~"svy.lincom.test",
        death ~ "svy.saddlepoint.test"
      )),
    NA
  )

  expect_equivalent(
    dplyr::filter(tbl2$meta_data, variable == "age")$p.value,
    survey::svyranktest(age ~ response, strial2, test = "vanderWaerden")$p.value %>% rlang::set_names(NULL)
  )

  expect_equivalent(
    dplyr::filter(tbl2$meta_data, variable == "ttdeath")$p.value,
    survey::svyranktest(ttdeath ~ response, strial2, test = "KruskalWallis")$p.value %>% rlang::set_names(NULL)
  )

  expect_equivalent(
    dplyr::filter(tbl2$meta_data, variable == "marker")$p.value,
    survey::svyranktest(marker ~ response, strial2, test = "median")$p.value %>% rlang::set_names(NULL)
  )

  expect_equivalent(
    dplyr::filter(tbl2$meta_data, variable == "trt")$p.value,
    survey::svychisq(~trt + response, strial2, statistic = "adjWald")$p.value %>% as.numeric()
  )

  expect_equivalent(
    dplyr::filter(tbl2$meta_data, variable == "stage")$p.value,
    survey::svychisq(~stage + response, strial2, statistic = "lincom")$p.value %>% as.numeric()
  )

  expect_equivalent(
    dplyr::filter(tbl2$meta_data, variable == "death")$p.value,
    survey::svychisq(~death + response, strial2, statistic = "saddlepoint")$p.value %>% as.numeric()
  )
})
