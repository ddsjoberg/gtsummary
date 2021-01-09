
# lifecycle 0.2.0

* Lifecycle warnings are now displayed once every 8 hours.

* Added experimental `signal_experimental()` and `signal_superseded()`
  functions.

* Added the "superseded" lifecycle stage to the documentation.

* `deprecate_stop()` now mentions that function is defunct (#28).

* New `expect_deprecated()` and `expect_defunct()` functions for
  testting lifecycle warnings and errors. `expect_deprecated()`
  automatically sets the `lifecycle_verbosity` option to `"warning"`
  to enforce warnings at each invokation rather than once per session.

* New syntax `"foo(arg = 'can\\'t be a baz')"` to describe that specific inputs
  for an argument are deprecated (#30, @krlmlr).

* New `is_present()` function to test whether the caller has supplied a
  `deprecated()` function.


# lifecycle 0.1.0

* Deprecated functions under the control of the developer now warn
  repeatedly in unit tests.

* Deprecation warnings now record a backtrace. Call
  `lifecycle::last_warnings()` and `lifecycle::last_warning()` to
  print the warnings that occurred during the last command, along with
  their backtraces.

* The naming scheme of signaller functions has been simplified:

  - `signal_soft_deprecated()` is now `deprecate_soft()`.
  - `warn_deprecated()` is now `deprecate_warn()`.
  - `stop_defunct()` is now `deprecate_stop()`.

* The signaller functions now take a version and two descriptors for
  the deprecated feature and its replacement (the latter is
  optional). The deprecation message is built from these
  components. You can pass a `details` argument to append additional
  information to the generated deprecation message.

* Helpers from rlang's `compat-lifecycle.R` drop-in file are now
  exported in this package.
