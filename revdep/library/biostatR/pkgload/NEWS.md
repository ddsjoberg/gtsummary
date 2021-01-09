# pkgload 1.1.0

* `dev_example()` now works after removing an inconsistent call to `load_all()` (@riccardoporreca, #122).

* `load_all()` now issues a warning if exported objects conflict with objects defined in the global environment (#112)

* `run_example()` arguments `run` and `test` are deprecated in favor of the (hopefully) more clear `run_dontrun` and `run_donttest` (#107).

* Internal fixes for compatibility with the future 4.1.0 release.

# pkgload 1.0.2

* `shim_question()` now works for topics from the R base package that are passed with the double colon operator (e.g. `base::min`) (@mdequeljoe, #99).

* `load_all()` now allows using explicitly qualified, exported names in test
  helpers (@klmr, #95).

* `load_all()` gains a `compile` argument which controls more finely whether to
  compile the code or not. The `recompile` argument is now deprecated and will
  be removed in a future version of pkgload.

# pkgload 1.0.1

* `unload()` now only removes S4 classes which were generated in the package
  being unloaded (#75)

* `help()` will no longer error when trying to load package level help (#67).

* Trailing slashes now removed from all paths, which fixes issues on Windows (#73).

* `load_dll()` now fixed in R-devel (#77).

* The help shim's now work for `:::` inputs (#72).

# pkgload 1.0.0

* `load_all()` now updates imports of dependent packages when a package is
  reloaded (#59).

* `load_all()` now assigns `DESCRIPTION/Depends` to `.Depends` object of 
  package environment. (@yiufung pkgload#61)

* `load_all()` now attaches `testthat` if the `attach_testthat` option is
  `TRUE`. This allows `load_all()` to more closely mimic the testing
  environment. (#56)

* `check_dep_version()` and `check_suggested()` are now exported.

* `check_dep_version()` now emits a warning and returns `FALSE` rather than
  aborting. (#47)

* Package imports are now exported when using `load_all()`. This behavior can
  be disabled by using `load_all(export_imports = FALSE)`.

* The `as.package()` and `is.package()` functions have been removed.

* `load_code()`, `load_data()`, `load_dll()`, `load_all()`, `parse_ns_file()`
  all now take an explicit path rather than a path or a `package` object.

* `imports_env()`, `ns_env()`, `pkg_env()` and `unload()` now take a package
  name rather than a path or a `package` object.

* `run_example()` now works on R 3.1.

* `unload()` now unloads S4 classes for packages loaded with `library()` as
  well as `load_all()` (#46).

* `load_all()` gains a `helpers` option to specify whether or not to
  source testthat helpers. (@pitakakariki devtools #1202)

* `load_all()` now sources the testthat helpers in the namespace environment
  rather than the package environment (#40).

* `load_all()` now sets the `NOT_CRAN` environment variable when it
  sources testthat helpers. It also sets `DEVTOOLS_LOAD` to "true" so
  that you can check whether they are run during package loading.

* `dev_topic_path()` now only returns the last path found, fixing an error
  when a package has both a package function level help with the same name.
  (#21)

* New function `is_dev_package()` to determine if a given package has been loaded
  by `pkgload::load_all()` (#2).

* `load_all()` no longer updates the collate directive. Instead this
  functionality has been moved to `devtools::document()`.

* `dev_help()` now optionally takes a character vector of packages to
  search within.  This replaces `find_topic()`.

* `dev_topic_index_reset()` is now exported, and allows you to reset
  the topic index associated with a given package.

* Added a `NEWS.md` file to track changes to the package.

* Initial release from code spun off from devtools
