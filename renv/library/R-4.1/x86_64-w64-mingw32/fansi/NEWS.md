# fansi Release Notes

## v0.5.0

* [#65](https://github.com/brodieG/fansi/issues/65): `sgr_to_html` optionally
  converts CSI SGR to classes instead of inline styles (h/t @hadley).
* [#69](https://github.com/brodieG/fansi/issues/69): `sgr_to_html` is more
  disciplined about emitting unnecessary HTML (h/t @hadley).
* New functions:
    * `sgr_256`: Display all 256 8-bit colors.
    * `in_html`: Easily output HTML in a web page.
    * `make_styles`: Easily produce CSS that matches 8-bit colors.
* Adjust for changes to `nchar(..., type='width')` for C0-C1 control characters
  in R 4.1.
* Restore tests bypassed in 0.4.2.

## v0.4.2

* Temporarily bypass tests due to R bug introduced in R-devel 79799.

## v0.4.1

* Correctly define/declare global symbols as per WRE 1.6.4.1, (h/t Professor
  Ripley, Joshua Ulrich for example fixes).
* [#59](https://github.com/brodieG/fansi/issues/59): Provide a `split.nl` option
  to `set_knit_hooks` to mitigate white space issues when using blackfriday for
  the markdown->html conversion (@krlmlr).

## v0.4.0

* Systematized which control sequences are handled specially by adding the `ctl`
  parameter to most functions.  Some functions such as `strip_ctl` had existing
  parameters that did the same thing (e.g. `strip`, or `which`), and those have
  been deprecated in favor of `ctl`.  While technically this is a change in the
  API, it is backwards compatible (addresses
  [#56](https://github.com/brodieG/fansi/issues/56) among and other things).
* Added `*_sgr` version of most `*_ctl` functions.
* `nzchar_ctl` gains the `ctl` parameter.
* [#57](https://github.com/brodieG/fansi/issues/57): Correctly detect when CSI
  sequences are not actually SGR (previously would apply styles from some
  non-SGR CSI sequences).
* [#55](https://github.com/brodieG/fansi/issues/55): `strsplit_ctl` can now work
  with `ctl` parameters containing escape sequences provided those sequences
  are excluded from by the `ctl` parameter.
* [#54](https://github.com/brodieG/fansi/issues/54): fix `sgr_to_html` so that
  it can handle vector elements with un-terminated SGR sequences (@krlmlr).
* Fix bug in width computation of first line onwards in `strwrap_ctl` when
  indent/exdent/prefix/initial widths vary from first to second line.
* Fix wrapping in `strwrap2_*(..., strip.spaces=FALSE)`, including a bug when
  `wrap.always=TRUE` and a line started in a word-whitespace boundary.
* Add `term.cap` parameter to `unhandled_ctl`.

## v0.3.0

* `fansi::set_knit_hooks` makes it easy to automatically convert ANSI CSI SGR
  sequences to HTML in Rmarkdown documents.  We also add a vignette that
  demonstrates how to do this.
* [#53](https://github.com/brodieG/fansi/issues/53): fix for systems where
  'char' is signed (found and fixed by @QuLogic).
* [#52](https://github.com/brodieG/fansi/issues/52): fix bad compilation under
  ICC (@kazumits).
* [#51](https://github.com/brodieG/fansi/issues/51): documentation improvements
  (@krlmlr).
* [#50](https://github.com/brodieG/fansi/issues/50): run tests on R 3.1 - 3.4
  tests for the rc branch only (@krlmlr).
* [#48](https://github.com/brodieG/fansi/issues/48): malformed call to error
  in FANSI_check_enc (@msannell).
* [#47](https://github.com/brodieG/fansi/issues/47): compatibility with R
  versions 3.2.0 and 3.2.1 (@andreadega).

## v0.2.3

* [#45](https://github.com/brodieG/fansi/issues/45): add capability to run under
  R 3.1 [hadley](https://github.com/hadley), [Gábor
  Csárdi](https://github.com/gaborcsardi).
* [#44](https://github.com/brodieG/fansi/issues/44): include bright color
  support in HTML conversion (h/t [Will Landau](https://github.com/wlandau)).

Other minor fixes ([#43](https://github.com/brodieG/fansi/issues/43), [#46](https://github.com/brodieG/fansi/issues/46)).

## v0.2.2

* Remove valgrind uninitialized string errors by avoiding `strsplit`.
* Reduce R dependency to >= 3.2.x (@gaborcsardi).
* Update tests to handle potential change in `substr` behavior starting with
  R-3.6.

## v0.2.1

* All string inputs are now encoded to UTF-8, not just those that are used in
  width calculations.
* UTF-8 tests skipped on Solaris.

## v0.2.0

* Add `strsplit_ctl`.

## v0.1.0

Initial release.


