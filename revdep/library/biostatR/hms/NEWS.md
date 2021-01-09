# hms 0.5.3

- Use `vec_default_ptype2()`, remove `vec_ptype2.hms.unspecified()` (#80, @romainfrancois).
- `vec_ptype2.hms.default()` forwards to `vec_default_ptype2()` for compatibility with vctrs 0.2.1.
- Remove `as.data.frame.hms()`, handeld by vctrs.


# hms 0.5.2.9000

- Internal changes only.


# hms 0.5.2

- Work around parsing error that occurs on DST changeover dates (https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16764).


# hms 0.5.1

- Lossy casts from `character` vectors to `hms` now also trigger a warning if the cast succeeds in the first element of the vector but fails for other elements.


# hms 0.5.0

## Breaking changes

- Now based on vctrs >= 0.2.0 (#61). This adds support for `vec_cast()` and `vec_ptype2()`. Some operations (such as subset assignment) are now stricter. The `new_hms()` constructor permits safe construction of `hms` objects from a numeric vector.

- The new `is_hms()` deprecates the existing `is.hms()`. `as.hms()` is deprecated in favor of `vec_cast()` or the new `as_hms()` (which is just a thin wrapper around `vec_cast()`).

## Printing

- Always show seconds in a pillar if they are different from zero (#54).

- Values with nonzero hours, seconds and split-seconds are now displayed correctly in tibbles (#56), even with a very small distance to the full second (#64).

## Internal

- Don't test colored output on CRAN.
- Correct reference link on r4ds (#58, @evanhaldane).


# hms 0.4.2

- Adapted tests to pillar 1.2.1.


# hms 0.4.1

- Preserve `NA` when converting to `character` (#51, @jeroen).
- Adapted tests to pillar 1.1.0.


# hms 0.4.0

## Breaking changes

- `as.hms.POSIXt()` now defaults to the current time zone, the previous default was `"UTC"` and can be restored by calling `pkgconfig::set_config("hms::default_tz", "UTC")`.

## New features

- Pillar support, will display `hms` columns in tibbles in color on terminals
  that support it (#43).
- New `round_hms()` and `trunc_hms()` for rounding or truncating to a given multiple of seconds (#31).
- New `parse_hms()` and `parse_hm()` to parse strings in "HH:MM:SS" and "HH:MM" formats (#30).
- `as.hms.POSIXt()` gains `tz` argument, default `"UTC"` (#28).
- `as.hms.character()` and `parse_hms()` accept fractional seconds (#33).

## Bug fixes

- `hms()` now works correctly if all four components (days, hours, minutes, seconds) are passed (#49).
- `hms()` creates a zero-length object of class `hms` that prints as `"hms()"`.
- `hms(integer())` and `as.hms(integer())` both work and are identical to `hms()`.
- Values with durations of over 10000 hours are now printed correctly (#48).
- `c()` now returns a hms (#41, @qgeissmann).

## Documentation and error messages

- Fix and enhance examples in `?hms`.
- Documentation is in Markdown format now.
- Improved error message if calling `hms()` with a character argument (#29).


# hms 0.3

- Fix `lubridate` test for compatibility with 1.6.0 (#23, @vspinu).
- NA values are formatted as `NA` (#22).


# hms 0.2

Minor fixes and improvements.

- Subsetting keeps `hms` class (#16).
- `format.hms()` right-justifies the output by padding with spaces from the left, `as.character.hms()` remains unchanged.
- Times larger than 24 hours or with split seconds are now formatted correctly (#12, #13).
- Sub-second part is printed with up to six digits, for even smaller values trailing zeros are shown (#17).


# hms 0.1

First CRAN release.

- Values are stored as a numeric vector that contains the number of seconds
  since midnight.
    - Inherits from `difftime` class.
    - Updating units is a no-op, anything different from `"secs"` issues a warning.
- Supports construction from time values, coercion to and from various data
  types, and formatting.
    - Conversion from numeric treats input as seconds.
    - Negative times are formatted with a leading `-`.
- Can be used as a regular column in a data frame.
- Full test coverage.
    - Test for arithmetic with `Date`, `POSIXt` and `hms` classes.
    - Test basic compatibility with `lubridate` package (#5).
- Interface:
    - `hms()` (with rigorous argument checks)
    - `as.hms()` for `character`, `numeric`, `POSIXct` and `POSIXlt`
    - `as.xxx.hms()` for `character`, `numeric` (implicitly), `POSIXct` and
      `POSIXlt`
    - `is.hms()`
    - `as.data.frame.hms()` (forwards to `as.data.frame.difftime()`)
    - `format.hms()`
    - `print.hms()` (returns unchanged input invisibly)
