
# rversions 2.1.1

No user visible changes.

# rversions 2.1.0

* `r_release_macos()` is now correct again for R 4.1.0.

# rversions 2.0.2

* `r_release_tarball()` now has the correct URL for R 4.x.x.

# rversions 2.0.1

* The date columns of all data.frames is now POSIXt, POSIXct, not POSIXlt, so you can convert the data.frames with `tibble::as_tibble()` directly (#20, @espinielli).

# rversions 2.0.0

## Breaking change

* `r_versions()` and all other functions now parse the date, so the date
  column is a POSIXt, not a character (#13).

# rversions 1.1.0

* All function outputs now also have a column with version nickname(s).

* R versions are now cached within each R session (@richfitz, #6).

# rversions 1.0.3

* Use `xml2::xml_find_first` instead of deprecated `xml2::xml_find_one`
  (Thanks to @jimhester)

# rversions 1.0.2

* Explicit import from base packages

# rversions 1.0.1

* Rewritten using `xml2` and `curl` instead of `XML` and `RCurl`
  (Thanks to @jeroenooms)

# rversions 1.0.0

First released version.
