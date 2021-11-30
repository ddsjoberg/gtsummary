
# prettycode 1.1.0

* Use the colors of the current theme in RStudio.

* Color matching brackets to help distinguish between scopes
  (#8, @mdequeljoe).

* The withr package is now a development dependency, it is not needed
  for regular use.

# prettycode 1.0.2

* Add `prettycode::prettycode()`. Call this function to turn on
  pretty-printing of function objects. This is needed to work around
  the new S3 method search limits in R 3.5.x.

* If prettycode fails to highlight a function, it falls back to
  `base::print.function()` now (#3).

# prettycode 1.0.1

* Avoid registering the `print.function` S3 method. This is needed to
  avoid a new `R CMD check` check

# prettycode 1.0.0

First public release.
