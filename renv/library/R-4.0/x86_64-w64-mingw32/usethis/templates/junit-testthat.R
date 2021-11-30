library(testthat)
library({{{ name }}})

if (requireNamespace("xml2")) {
  test_check("{{{ name }}}", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("{{{ name }}}")
}
