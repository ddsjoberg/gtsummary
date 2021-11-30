
# prettycode

> Pretty Print R Code in the Terminal

[![Linux Build Status](https://travis-ci.org/r-lib/prettycode.svg?branch=master)](https://travis-ci.org/r-lib/prettycode)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/prettycode?svg=true)](https://ci.appveyor.com/project/gaborcsardi/prettycode)
[![](http://www.r-pkg.org/badges/version/prettycode)](http://www.r-pkg.org/pkg/prettycode)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/prettycode)](http://www.r-pkg.org/pkg/prettycode)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/prettycode/master.svg)](https://codecov.io/github/r-lib/prettycode?branch=master)

Replace the standard print method for functions with one that performs
syntax highlighting, using ANSI colors, if the terminal supports them.

## Installation

```r
install.packages("prettycode")
```

## Usage

Just call `prettycode::prettycode()` and start printing functions to the
screen. Long functions are automatically paged using the default pager.

![](/inst/screenshot.png)

## License

MIT © Gábor Csárdi
