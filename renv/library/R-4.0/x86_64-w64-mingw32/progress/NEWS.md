
# 1.2.2

* Fix two bugs in the C++ API that can potentially cause irregular
  behavior (#94, @jimhester).

# 1.2.1

* Add the `message_class` argument, to add extra classes to the messages
  thrown by progress.

* Add support for RKWard, running interactive session
  (#76, @devillemereuil).

* Add support for unicode characters and a right to left progress bar
  (#86, @jimhester).

* Fix a potential crash in the C++ API (#91, @jimhester).

# 1.2.0

* Option to change the 'current' character in the progress bar. #47

* New `:tick_rate` token, similar to `:rate`, but units are not assumed
  to be bytes. #45

* Total number of ticks can be set to `NA` now, if unknown. #48

* All progress bars (including the ones from the C++ API) can be turned
  off via the `progress_enabled` option #44

* The `stream` argument is deprecated and the progress bar is always
  printed with `message()` calls.

* Total number of iterations can be zero, to support special
  cases better.

* Add `finished` variable, which can be queried to see if a progress bar is
  finished. #54 @jimhester.

* Add a `terminate()` method, which can be called to explicitly remove the
  progress bar, #53 @jimhester.

* Outputs greater than the bar width are automatically trimmed, @jimhester.

* Add a `message()` method to print a message line(s) above the progress bar,
  @jimhester.

* :elapsedfull token: elapsed time in hh:mm:ss format.

* Fix C++ compiler warnings. #43

* Fix C++ API on Windows. #56

* Pass `self` to the callback function. #55 @jimhester

# 1.1.2

* Do not run tests on CRAN, Solaris does not have microbenchmark

# 1.1.1

* Move README.md, so that it is not parsed on CRAN
* Use https URLs instead of http, whenever possible

# 1.1.0

* Support for the `:spin` token which adds a simple ASCII spinner, @richfitz
* Respect custom token width for calculation of the bar width, @mllg

# 1.0.2

* Fix the C++ API on Windows, and on older compilers in general.

# 1.0.1

First version on CRAN.
