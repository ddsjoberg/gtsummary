testthat::context("crosswalk.R")

# Define helper function that tests a json object
test_json <- function(x, basename) {

  testthat::expect_s4_class(x, "json")
  testfile <- tempfile(pattern = "codemetatest", fileext = ".json")
  writeLines(x, testfile)
  testthat::expect_true(codemeta_validate(testfile))
  unlink(testfile)
}

# Define helper function that reads a json example file
read_example_json <- function(file) jsonlite::read_json(example_file(file))

testthat::test_that("we can call crosswalk", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_offline()

  read_example_json("github_format.json") %>%
    crosswalk("GitHub") %>%
    test_json("test")

  a <- read_example_json("package.json") %>%
    crosswalk("NodeJS")

  test_json(a, "nodejs")

  ## Test add and drop context
  drop_context(a) %>%
    add_context(getOption("codemeta_context", "http://purl.org/codemeta/2.0"))

  ## Test transforms between columns
  read_example_json("github_format.json") %>%
    crosswalk("GitHub", "Zenodo")

  crosswalk_table(from = "GitHub", to = c("Zenodo", "Figshare"))
})

testthat::test_that("R Package Description", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_offline()

  cm_list <- jsonlite::read_json(
    system.file("examples/codemeta.json", package="codemetar")
  )
  cm_json <- crosswalk(cm_list, "R Package Description")
  test_json(cm_json, "r_pkg_desc")

  ## Test add and drop context
  new_json <- drop_context(cm_json)
  expect_null(jsonlite::fromJSON(new_json)$`@context`, NULL)
  expect_type(
    jsonlite::fromJSON(
      add_context(new_json,
                  getOption("codemeta_context",
                            "http://purl.org/codemeta/2.0")))$`@context`,
      "list")

  ## Test transforms between columns
  expect_s4_class(
    crosswalk(cm_list, "R Package Description", "Zenodo"),
    "json")

  expect_s3_class(
    crosswalk_table(from = "R Package Description", to = c("Zenodo", "Figshare")),
    "data.frame"
  )

})
