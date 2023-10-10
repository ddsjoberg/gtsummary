skip_on_cran()
skip_if_not(broom.helpers::.assert_package("Hmisc", pkg_search = "gtsummary", boolean = TRUE))

test_that("add_ci() works", {
  expect_error(
    tbl1 <-
      trial %>%
      select(response, age, trt) %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_p() %>%
      add_ci(pattern = "{stat} ({ci})"),
    NA
  )
  expect_equal(
    as_tibble(tbl1) %>% names(),
    c(
      "**Characteristic**", "**Drug A**, N = 98 (**95% CI**)",
      "**Drug B**, N = 102 (**95% CI**)", "**p-value**"
    )
  )
  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% pull(stat_1),
    c("28 (29%) (21%, 40%)", "46 (37, 59) (44, 50)")
  )
  expect_snapshot(
    tbl1 %>%
      as.data.frame()
  )

  res <-
    trial %>%
    select(response, age, trt) %>%
    tbl_summary(by = trt, missing = "no") %>%
    add_p() %>%
    add_ci()
  expect_error(
    tbl1 <-
      res %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    names(tbl1),
    c("label", "stat_1", "ci_stat_1", "stat_2", "ci_stat_2", "p.value")
  )
  expect_equal(
    tbl1 %>% select(starts_with("ci_stat_")) %>% as.list(),
    list(
      ci_stat_1 = c("21%, 40%", "44, 50"),
      ci_stat_2 = c("25%, 44%", "45, 50")
    )
  )
  expect_snapshot(
    res %>% as.data.frame()
  )
  res <-
    trial %>%
    select(response, trt) %>%
    tbl_summary(missing = "no") %>%
    add_ci(
      method = everything() ~ "exact",
      statistic = everything() ~ "{conf.low} to {conf.high}"
    )
  expect_error(
    tbl2 <-
      res %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    names(tbl2),
    c("label", "stat_0", "ci_stat_0")
  )
  expect_equal(
    tbl2$ci_stat_0,
    c("25 to 39", NA, "42 to 56", "44 to 58")
  )
  expect_snapshot(
    res %>% as.data.frame()
  )
})

test_that("add_ci() works on a subset of variables", {
  expect_snapshot(
    trial %>%
      tbl_summary(
        include = c(response, age)
      ) %>%
      add_ci(include = age) %>%
      as_tibble()
  )
})

test_that("add_ci() column order respects add_overall(last=TRUE)", {
  expect_snapshot(
    trial %>%
      tbl_summary(
        by = trt,
        include = "stage") %>%
      add_overall(last = TRUE) %>%
      add_ci() %>%
      as_tibble(col_label = FALSE)
  )
})

test_that("add_ci() throws errors with bad arguments", {
  tbl0 <-
    trial %>%
    select(response, trt) %>%
    tbl_summary(missing = "no")

  expect_error(
    tbl0 %>%
      add_ci(method = everything() ~ "t.test")
  )

  expect_error(
    tbl0 %>%
      add_ci(
        style_fun = letters
      )
  )
  expect_error(
    tbl0 %>%
      add_ci(
        statistic = letters
      )
  )
  expect_error(
    tbl0 %>%
      add_ci(
        method = "letters"
      )
  )
  expect_error(
    add_ci(x = letters)
  )
})


test_that("add_ci() works with tbl_svysummary", {
  skip_if_not(assert_package("survey", boolean = TRUE))

  data(api, package = "survey")
  d <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1 %>% dplyr::mutate(logical = (yr.rnd == "No")),
    fpc = ~fpc
  )

  tbl <- d %>%
    tbl_svysummary(
      by = both,
      include = c(api00, hsg, stype, yr.rnd, logical),
      statistic = hsg ~ "{mean} ({sd})"
    )

  expect_error(
    svyres <- tbl %>% add_ci(method = api00 ~ "svymedian"),
    NA
  )
  expect_equal(
    as_tibble(svyres) %>% names(),
    c(
      "**Characteristic**", "**No**, N = 1,692", "**95% CI**",
      "**Yes**, N = 4,502", "**95% CI**"
    )
  )
  expect_equal(
    as_tibble(svyres, col_labels = FALSE) %>% dplyr::pull(ci_stat_1),
    c("547, 722", "13, 28", NA, "43%, 81%", "6.6%, 27%", "8.7%, 46%",
      "0.32%, 12%", "88%, 100%")
  )
  expect_message(tbl %>% add_ci())
  expect_snapshot(svyres %>% as_tibble())

  expect_error(
    res <-
      tbl %>%
      add_ci(
        method = list(
          api00 ~ "svymedian.beta",
          stype ~ "svyprop.mean"
        ),
        df = Inf
      ),
    NA
  )
  expect_snapshot(res %>% as_tibble())

  expect_error(
    res <- d %>%
      tbl_svysummary(
        by = both,
        include = stype,
        percent = "row"
      ) %>%
      add_overall() %>%
      add_ci(),
    NA
  )
  expect_equal(
    as_tibble(res, col_labels = FALSE) %>% dplyr::pull(ci_stat_1),
    c(NA, "17%, 29%", "27%, 73%", "20%, 71%")
  )
  expect_error(
    res <- d %>%
      tbl_svysummary(
        by = both,
        include = stype,
        percent = "cell"
      ) %>%
      add_overall() %>%
      add_ci(),
    NA
  )
  expect_equal(
    as_tibble(res, col_labels = FALSE) %>% dplyr::pull(ci_stat_1),
    c(NA, "13%, 24%", "1.5%, 9.2%", "2.5%, 14%")
  )
})
