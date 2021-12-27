
# sessioninfo

> R Session Information

[![Linux Build Status](https://travis-ci.org/r-lib/sessioninfo.svg?branch=master)](https://travis-ci.org/r-lib/sessioninfo)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/sessioninfo?svg=true)](https://ci.appveyor.com/project/gaborcsardi/sessioninfo)
[![](http://www.r-pkg.org/badges/version/sessioninfo)](http://www.r-pkg.org/pkg/sessioninfo)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/sessioninfo)](http://www.r-pkg.org/pkg/sessioninfo)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/sessioninfo/master.svg)](https://codecov.io/github/r-lib/sessioninfo?branch=master)

Query and print information about the current R session. It is similar to
`utils::sessionInfo()`, but includes more information about packages, and
where they were installed from.

## Installation

```r
devtools::install_github("r-lib/sessioninfo")
```

## Usage

Example output:

```r
sessioninfo::session_info()
```

```r
─ Session info ───────────────────────────────────────────────────────────────
 setting  value
 version  R version 3.5.0 (2018-04-23)
 os       macOS Sierra 10.12.6
 system   x86_64, darwin15.6.0
 ui       X11
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Brussels
 date     2018-08-09

─ Packages ───────────────────────────────────────────────────────────────────
 package     * version    date       source
 clisymbols    1.2.0      2017-05-21 CRAN (R 3.5.0)
 crayon        1.3.4      2017-09-16 CRAN (R 3.5.0)
 gitty         1.0.0      2018-07-16 Github (gaborcsardi/gitty@67a6e3e)
 memuse        4.0-0      2017-11-10 CRAN (R 3.5.0)
 prettycode  * 1.0.1      2017-12-12 CRAN (R 3.5.0)
 prompt        1.0.0      2018-07-16 Github (gaborcsardi/prompt@e5248b5)
 sessioninfo   1.0.1.9000 2018-08-09 local
 tracer      * 1.0.0      2017-01-28 CRAN (R 3.5.0)
 whisker       0.3-2      2013-04-28 CRAN (R 3.5.0)
 withr         2.1.2      2018-03-15 CRAN (R 3.5.0)
```

### Copying to the clipboard

You can use the
[`clipr` package](https://cran.rstudio.com/web/packages/clipr/) to copy
the session info to the clipboard:

```r
clipr::write_clip(session_info())
```

(The current `clipr` version prints a warning, but you can ignore that.)

## License

GPL-2
