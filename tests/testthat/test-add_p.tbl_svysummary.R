skip_on_cran()
skip_if_not(broom.helpers::.assert_package("survey", pkg_search = "gtsummary", boolean = TRUE))

strial <- trial %>%
  survey::svydesign(data = ., ids = ~1, weights = ~1)
d <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

test_that("add_p creates output without error/warning", {
  expect_snapshot(
    tbl_svysummary(strial, by = grade) %>% add_p() %>% as.data.frame()
  )

  expect_snapshot(
    strial %>%
      tbl_svysummary(by = trt) %>%
      add_p() %>%
      as.data.frame()
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
  expect_snapshot(
    tbl_svysummary(strial, by = grade, type = all_continuous() ~ "continuous2") %>%
      add_p() %>%
      as.data.frame()
  )

  expect_snapshot(
    strial %>%
      tbl_svysummary(by = trt, type = all_continuous() ~ "continuous2") %>%
      add_p() %>%
      as.data.frame()
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
  skip_if_not(broom.helpers::.assert_package("flextable", pkg_search = "gtsummary", boolean = TRUE))
  expect_error(
    tbl1 <-
      tbl_svysummary(strial, by = response) %>%
      add_p(test = list(
        age ~ "svy.t.test",
        marker ~ "svy.wilcox.test",
        trt ~ "svy.chisq.test",
        stage ~ "svy.adj.chisq.test",
        death ~ "svy.wald.test"
      )),
    NA
  )
  expect_snapshot(tbl1 %>% as.data.frame())

  expect_equal(
    dplyr::filter(tbl1$meta_data, variable == "age")$p.value,
    survey::svyttest(age ~ response, strial)$p.value %>% rlang::set_names(NULL)
  )

  expect_equal(
    dplyr::filter(tbl1$meta_data, variable == "marker")$p.value,
    survey::svyranktest(marker ~ response, strial, test = "wilcoxon")$p.value %>% rlang::set_names(NULL)
  )

  expect_equal(
    dplyr::filter(tbl1$meta_data, variable == "trt")$p.value,
    survey::svychisq(~ trt + response, strial, statistic = "F")$p.value %>% rlang::set_names(NULL),
    tolerance = 10^-4 # see issue https://github.com/ddsjoberg/gtsummary/issues/702
  )

  expect_equal(
    dplyr::filter(tbl1$meta_data, variable == "stage")$p.value,
    survey::svychisq(~ stage + response, strial, statistic = "Chisq")$p.value %>% rlang::set_names(NULL)
  )

  # TO DO: fix test
  expect_equal(
    dplyr::filter(tbl1$meta_data, variable == "death")$p.value,
    survey::svychisq(~ death + response, strial, statistic = "Wald")$p.value %>% rlang::set_names(NULL) %>% .[[1]],
    tolerance = 10^-2 # see issue https://github.com/ddsjoberg/gtsummary/issues/702
  )



  expect_error(
    tbl2 <-
      tbl_svysummary(strial, by = response) %>%
      add_p(test = list(
        age ~ "svy.vanderwaerden.test",
        ttdeath ~ "svy.kruskal.test",
        marker ~ "svy.median.test",
        trt ~ "svy.adj.wald.test",
        stage ~ "svy.lincom.test",
        death ~ "svy.saddlepoint.test"
      )),
    NA
  )
  expect_snapshot(tbl2 %>% as.data.frame())
  expect_error(as_flex_table(tbl2), NA)

  expect_equal(
    dplyr::filter(tbl2$meta_data, variable == "age")$p.value,
    survey::svyranktest(age ~ response, strial, test = "vanderWaerden")$p.value %>% rlang::set_names(NULL)
  )

  expect_equal(
    dplyr::filter(tbl2$meta_data, variable == "ttdeath")$p.value,
    survey::svyranktest(ttdeath ~ response, strial, test = "KruskalWallis")$p.value %>% rlang::set_names(NULL)
  )

  expect_equal(
    dplyr::filter(tbl2$meta_data, variable == "marker")$p.value,
    survey::svyranktest(marker ~ response, strial, test = "median")$p.value %>% rlang::set_names(NULL)
  )

  expect_equal(
    dplyr::filter(tbl2$meta_data, variable == "trt")$p.value,
    survey::svychisq(~ trt + response, strial, statistic = "adjWald")$p.value %>% as.numeric(),
    tolerance = 10^-4 # see issue https://github.com/ddsjoberg/gtsummary/issues/702
  )

  expect_equal(
    dplyr::filter(tbl2$meta_data, variable == "stage")$p.value,
    survey::svychisq(~ stage + response, strial, statistic = "lincom")$p.value %>% as.numeric(),
    tolerance = 10^-2 # see issue https://github.com/ddsjoberg/gtsummary/issues/702
  )

  # TO DO: fix test
  expect_equal(
    dplyr::filter(tbl2$meta_data, variable == "death")$p.value,
    survey::svychisq(~ death + response, strial, statistic = "saddlepoint")$p.value %>% as.numeric(),
    tolerance = 10^-2 # see issue https://github.com/ddsjoberg/gtsummary/issues/702
  )
})
