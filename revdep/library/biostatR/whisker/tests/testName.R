library(testthat)
context("Extra tests")

test_that("names with . work",{
  a.test <- 'World'
  template <- template <- "Hello {{a.test}}!"
  expect_equal( whisker.render(template, list(a.test = a.test), strict=FALSE)
              , "Hello World!"
              )
})

test_that("referencing with $ works",{
  a <- list(test = 'World')
  template <- template <- "Hello {{a$test}}!"
  expect_equal( whisker.render(template, list(a=a), strict=FALSE)
                , "Hello World!"
  )
})

context("Github test")


test_that("newlines consistency",{
  template <- 
"{{#var}}
var is true
{{/var}}
{{^var}}
var is not true
{{/var}}"

  expect_equal("var is true\n",whisker.render(template, list(var=TRUE)))
  expect_equal("var is not true\n",whisker.render(template, list(var=FALSE)))
})
