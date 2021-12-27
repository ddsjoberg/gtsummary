
# parsedate 1.2.1

No changes.

# parsedate 1.2.0

* `parse_date()` and `parse_iso_8601()` now dupport a default time zone,
  that will be used for dates that do not explicitly specify one.

* Reimplement `parse_iso_8601()` with vectorized code, for speed (#9).

* Fix `parse_date()` and `parse_iso_8601()` for zero-length input (#20).

* `parse_date()` parses strings with `+` characters correctly now (#23).

# Parsedate 1.1.3

* Update URLs in the README

# Parsedate 1.1.2

* Fix a vectorization bug in the ISO 8601 date parser
* Register native routines

# Parsedate 1.1.1

* Drop `lubridate` package dependency

* Fix parsing dates consisting of six or eight digits, e.g. `20140922` and `092214`

* `NA` is returned by `parse_date` for non-sensical numerical dates, e.g. `000202`

* Fix `parse_date` time zone that was wrong for some dates

* Fix `parse_date` for dates are not in DST

# Parsedate 1.0.3

* Fix a bug in `format_iso_8601`, on platforms that have a buggy `%z`

# Parsedate 1.0.2

* First release on CRAN
