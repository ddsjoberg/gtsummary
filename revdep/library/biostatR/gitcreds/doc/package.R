## -----------------------------------------------------------------------------
gitcreds::gitcreds_cache_envvar("https://github.com")

## -----------------------------------------------------------------------------
library(testthat)
test_that("bad credentials from git", {
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "bad"))
  # Test code that calls gitcreds_get(), potentially downstream.
  # gitcreds_get() will return `bad` as the password.
  # Illustration:
  expect_equal(
    gitcreds::gitcreds_get("https://github.com")$password,
    "bad"
  )
})

## -----------------------------------------------------------------------------
library(testthat)
test_that("another GitHub user", {
  cred <- paste0(
    "protocol:https:",
    "host:github.com:",
    "username:user1:",
    "password:secret"
  )
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = cred))
  # Your test code comes here. This is just an illustration:
  print(gitcreds::gitcreds_get())
  expect_equal(gitcreds::gitcreds_get()$username, "user1")
})

## -----------------------------------------------------------------------------
library(testthat)
test_that("no credentials from git", {
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "FAIL"))
  # The test code that calls gitcreds_get() comes here.
  # It will fail with error "gitcreds_no_credentials"
  expect_error(
    gitcreds::gitcreds_get("https://github.com"),
    class = "gitcreds_no_credentials"
  )
})

## -----------------------------------------------------------------------------
library(testthat)
test_that("no git installation", {
  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = "FAIL:gitcreds_nogit_error"
  ))
  # Test code that calls gitcreds_get() comes here.
  # Illustration:
  expect_error(
    gitcreds::gitcreds_get("https://github.com"),
    class = "gitcreds_nogit_error"
  )
})

