testthat::context("validate")

testthat::test_that("we can validate this file", {
  skip_on_cran()
  skip_if_offline()
  path <- tempfile(pattern = "codemetatest", fileext = ".json")
  write_codemeta("codemetar", path, verbose = FALSE)
  testthat::expect_true(codemeta_validate(path))
  unlink(path)

})


testthat::test_that("we can create & validate codemeta for xml2 package", {
  skip_on_cran()
  skip_if_offline()
  path <- tempfile(pattern = "codemetatest", fileext = ".json")
  write_codemeta("xml2", path, verbose = FALSE)
  testthat::expect_true(codemeta_validate(path))
  unlink(path)

})
