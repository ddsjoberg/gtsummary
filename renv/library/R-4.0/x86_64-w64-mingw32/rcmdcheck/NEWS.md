
# 1.3.3

* `cran_check_results()` has now a `quiet` argument, and the download
  progress bars are shown if it is set to `FALSE` (#17).

* Fix output when standard output does not support `\r`, typically when
  it is not a terminal (#94).

* Fix standard output and standard error mixup in the test cases,
  (#88, #96).

* Fix parsing test failures when multiple architectures are checked, (#97).

* `rcmdcheck()` has now better colors. WARNINGs are magenta, and NOTEs
  are blue (#103, @hadley).

# 1.3.2

* `rcmdcheck()` now correctly overwrites existing tarballs if they already
  exist in the check directory. This time for real.

# 1.3.1

* `rcmdcheck()` now correctly overwrites existing tarballs if they already
  exist in the check directory (#84 @jimhester).

* rcmdcheck now uses `sessioninfo::session_info()` to query session
  information for the check.

# 1.3.0

* New `rcmdcheck_process` class to run `R CMD check` in the background.

* `rcmdcheck()` now supports timeouts (default is 10 minutes).

* Checks now capture and print installation and test failures.

* Checks now record and print the duration of the check.

* Checks now record and print session information from the check
  session (#22).

* `rcmdcheck()` new keep files until the returned check object is
  deleted, if check was run in a temporary directory (the default) (#23).

* New `xopen()` to show the check file in a file browser window (#61).

* Checks now save `install.out` and also `DESCRIPTION` in the result,
  and save the standard error and the exit status as well.

* `rcmdcheck()` printing is now better: the message from the check that is
  actually _being performed_ is shown on the screen.

* `rcmdcheck()` now shows a spinner while running check.

* `rcmdcheck()` results now have a `summary()` method for check comparisons.

* `rcmdcheck()` results now have a new  `check_details()` method, to query
  the check results programmatically. (No need to use `$errors`,
  `$warnings`, etc. directly.)

* Checks now find package root automatically (#18).

* `rcmdcheck()` now has an `error_on` argument to throw an error on an
  `R CMD check` failure (#51).

* `rcmdcheck()` result printing is now better, the colors are
  consistent (#54).

# 1.2.1

* Compare two check results with `compare_checks` or compare check
  results to CRAN with `compare_to_cran`.

* The result object has more metadata: package name, version,
  R version and platform.

* Refined printing of the result.

* `rcmdcheck()` works on tarballs build via `R CMD build` now.

* Parse `R CMD check` results: `parse_check`, `parse_check_url`.

* Download and parse check results for CRAN packages.

* Report errors during the build, typically vignette errors.

* Use the `callr` package (https://github.com/r-lib/callr)
  for running `R CMD` commands.

# 1.1.0

* New arguments `libpath` and `repos` to set the library path
  and the default CRAN repository

* Do not run tests on CRAN.

# 1.0.0

First public release.
