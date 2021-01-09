## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lifecycle)

## -----------------------------------------------------------------------------
deprecate_warn("1.0.0", "mypkg::foo()")

## -----------------------------------------------------------------------------
deprecate_warn("1.0.0", "mypkg::foo()", "new()")

## -----------------------------------------------------------------------------
# The new replacement
foobar_adder <- function(foo, bar) {
  foo + bar
}

# The old function still exported for compatibility
foobaz_adder <- function(foo, bar) {
  deprecate_warn("1.0.0", "foobaz_adder()", "foobar_adder()")
  foobar_adder(foo, bar)
}

## -----------------------------------------------------------------------------
deprecate_warn("1.0.0", "mypkg::foo(arg = )")

deprecate_warn("1.0.0", "mypkg::foo(arg = )", "mypkg::foo(new = )")

## -----------------------------------------------------------------------------
deprecate_warn("1.0.0", "mypkg::foo(arg = 'must be a scalar integer')")

## -----------------------------------------------------------------------------
foobar_adder <- function(foo, bar, baz = deprecated()) {
  # Check if user has supplied `baz` instead of `bar`
  if (lifecycle::is_present(baz)) {

    # Signal the deprecation to the user
    deprecate_warn("1.0.0", "foobar_adder(baz = )", "foobar_adder(bar = )")

    # Deal with the deprecated argument for compatibility
    bar <- baz
  }

  foo + bar
}

## ---- eval = FALSE------------------------------------------------------------
#  library(testthat)
#  library(mypackage)
#  
#  options(lifecycle_verbosity = "error")
#  test_check("mypackage")

## ---- eval = FALSE------------------------------------------------------------
#  # Force silence
#  options(lifecycle_verbosity = "quiet")
#  
#  # Force warnings
#  options(lifecycle_verbosity = "warning")
#  
#  # Force errors
#  options(lifecycle_verbosity = "error")

## ---- eval = FALSE------------------------------------------------------------
#  test_that("`baz` argument of `foobar_adder()` still works", {
#    withr::local_options(list(lifecycle_verbosity = "quiet"))
#    foobar_adder(1, baz = 2)
#  })

## ---- eval = FALSE------------------------------------------------------------
#  setup(options(lifecycle_verbosity = "quiet"))
#  teardown(options(lifecycle_verbosity = NULL))

## ---- eval = FALSE------------------------------------------------------------
#  test_that("`baz` argument of `foobar_adder()` is deprecated", {
#    expect_deprecated(foobar_adder(1, baz = 2))
#  })
#  
#  test_that("`foo()` is defunct", {
#    expect_defunct(foo())
#  })

