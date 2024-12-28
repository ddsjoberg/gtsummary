skip_on_cran()

test_that("modify_caption(caption) works", {
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_caption("**Adding a caption** N = {N}", text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("caption"),
    "**Adding a caption** N = 200" |>
      structure(text_interpret = "html")
  )
})

test_that("modify_caption() works with tbl_svysummary()", {
  skip_if_not(is_pkg_installed(c("survey", "cardx")))

  expect_equal(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))|>
      add_overall() |>
      modify_caption("**Adding a caption** N = {N}", text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("caption"),
    "**Adding a caption** N = 2201" |>
      structure(text_interpret = "html")
  )
})

test_that("modify_caption() works with tbl_continuous()", {
  expect_equal(tbl_continuous(data = trial, variable = age, by = trt, include = grade)|>
                 add_overall() |>
                 modify_caption("**Adding a caption** N = {N}", text_interpret = "html") |>
                 getElement("table_styling") |>
                 getElement("caption"),
               "**Adding a caption** N = 200" |>
                 structure(text_interpret = "html")
  )
})


test_that("modify_caption() works with tbl_cross()", {
  expect_equal(tbl_cross(data = trial, row = trt, col = response) |>
                 modify_caption("**Adding a caption** N = {N}", text_interpret = "html") |>
                 getElement("table_styling") |>
                 getElement("caption"),
               "**Adding a caption** N = 200" |>
                 structure(text_interpret = "html")
  )
})

test_that("modify_caption() works with tbl_regression()", {
  skip_if_not(is_pkg_installed("broom.helpers"))

  expect_equal(glm(response ~ age + grade, trial, family = binomial()) |>
                 tbl_regression(exponentiate = TRUE) |>
                 modify_caption("**Adding a caption** N = {N} (Event N = {N_event})", text_interpret = "html") |>
                 getElement("table_styling") |>
                 getElement("caption"),
               "**Adding a caption** N = 183 (Event N = 58)" |>
                 structure(text_interpret = "html")
  )
})

test_that("modify_caption() works with tbl_uvregression()", {
  expect_equal(tbl_uvregression(trial, method = glm, y = response, method.args = list(family = binomial),
                                exponentiate = TRUE, include = c("age", "grade")) |>
                 modify_caption("**Adding a caption**", text_interpret = "html") |>
                 getElement("table_styling") |>
                 getElement("caption"),
               "**Adding a caption**" |>
                 structure(text_interpret = "html")
  )
})

test_that("modify_caption() works with vector of input", {
  expect_silent(
    tbl <- tbl_summary(trial, include = age) |>
      modify_caption(c("row one", "row two"))
  )
  expect_equal(
    as_gt(tbl) |>
      getElement("_options") |>
      dplyr::filter(parameter == "table_caption") |>
      dplyr::pull("value") |>
      getElement(1L),
    c("row one", "row two"),
    ignore_attr = TRUE
  )
})
