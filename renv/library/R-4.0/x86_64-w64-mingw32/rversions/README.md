
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rversions <a href='https://r-hub.github.io/rversions'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

> Past and present R versions

<!-- badges: start -->

[![Linux Build
Status](https://travis-ci.org/r-hub/rversions.svg?branch=master)](https://travis-ci.org/r-hub/rversions)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/github/r-hub/rversions?svg=true)](https://ci.appveyor.com/project/gaborcsardi/rversions)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/rversions)](http://r-pkg.org/pkg/rversions)
[![CRAN
version](http://www.r-pkg.org/badges/version/rversions)](http://r-pkg.org/pkg/rversions)
[![CRAN
checks](https://cranchecks.info/badges/summary/rversions)](https://cran.r-project.org/web/checks/check_results_rversions.html)
[![Gitter
chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/r-hub/community)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The R version numbers and dates are extracted from the main R SVN
repository at <http://svn.r-project.org/R/>.

`rversions` is in particular useful for determining at any given moment
what versions “R-release” and “R-oldrel” refer to. This is useful for
e.g. [R-hub](https://docs.r-hub.io/).

## Installation

``` r
install.packages("rversions")
```

## Usage

### R-release and R-oldrel

``` r
library(rversions)
r_release()
```

    #>     version                date           nickname
    #> 114   3.6.1 2019-07-05 07:05:03 Action of the Toes

``` r
r_oldrel()
```

    #>     version                date    nickname
    #> 112   3.5.3 2019-03-11 08:04:49 Great Truth

### All R versions and release dates

``` r
r_versions()
```

    #>     version                date                 nickname
    #> 1      0.60 1997-12-04 08:47:58                     <NA>
    #> 2      0.61 1997-12-21 13:09:22                     <NA>
    #> 3    0.61.1 1998-01-10 00:31:55                     <NA>
    #> 4    0.61.2 1998-03-14 19:25:55                     <NA>
    #> 5    0.61.3 1998-05-02 07:58:17                     <NA>
    #> 6      0.62 1998-06-14 12:56:20                     <NA>
    #> 7    0.62.1 1998-06-14 22:13:25                     <NA>
    #> 8    0.62.2 1998-07-10 11:13:45                     <NA>
    #> 9    0.62.3 1998-08-28 09:02:19                     <NA>
    #> 10   0.62.4 1998-10-23 12:08:41                     <NA>
    #> 11     0.63 1998-11-13 14:37:19                     <NA>
    #> 12   0.63.1 1998-12-04 13:06:28                     <NA>
    #> 13   0.63.2 1999-01-11 12:55:50                     <NA>
    #> 14   0.63.3 1999-03-05 14:27:14                     <NA>
    #> 15     0.64 1999-04-07 13:19:41                     <NA>
    #> 16   0.64.1 1999-05-07 13:25:43                     <NA>
    #> 17   0.64.2 1999-07-02 12:23:15                     <NA>
    #> 18     0.65 1999-08-27 10:29:29                     <NA>
    #> 19   0.65.1 1999-10-06 12:13:04                     <NA>
    #> 20     0.90 1999-11-22 12:25:14                     <NA>
    #> 21   0.90.1 1999-12-15 12:29:07                     <NA>
    #> 22     0.99 2000-02-07 11:24:50                     <NA>
    #> 23      1.0 2000-02-29 08:55:23                     <NA>
    #> 24    1.0.1 2000-04-14 08:44:18                     <NA>
    #> 25      1.1 2000-06-15 08:43:21                     <NA>
    #> 26    1.1.1 2000-08-15 08:54:18                     <NA>
    #> 27      1.2 2000-12-15 10:19:25                     <NA>
    #> 28    1.2.1 2001-01-15 10:18:01                     <NA>
    #> 29    1.2.2 2001-02-26 12:43:25                     <NA>
    #> 30    1.2.3 2001-04-26 11:29:47                     <NA>
    #> 31      1.3 2001-06-22 10:41:02                     <NA>
    #> 32    1.3.1 2001-08-31 12:45:52                     <NA>
    #> 33      1.4 2001-12-19 10:14:54                     <NA>
    #> 34    1.4.1 2002-01-30 11:57:35                     <NA>
    #> 35    1.5.0 2002-04-29 10:01:26                     <NA>
    #> 36    1.5.1 2002-06-17 11:20:33                     <NA>
    #> 37    1.6.0 2002-10-01 10:06:31                     <NA>
    #> 38    1.6.1 2002-11-01 10:33:17                     <NA>
    #> 39    1.6.2 2003-01-10 15:34:34                     <NA>
    #> 40    1.7.0 2003-04-16 12:58:07                     <NA>
    #> 41    1.7.1 2003-06-16 09:54:39                     <NA>
    #> 42    1.8.0 2003-10-08 11:13:59                     <NA>
    #> 43    1.8.1 2003-11-21 12:00:21                     <NA>
    #> 44    1.9.0 2004-04-12 10:36:38                     <NA>
    #> 45    1.9.1 2004-06-21 11:09:39                     <NA>
    #> 46    2.0.0 2004-10-04 14:24:38                     <NA>
    #> 47    2.0.1 2004-11-15 14:16:30                     <NA>
    #> 48    2.1.0 2005-04-18 22:26:33                     <NA>
    #> 49    2.1.1 2005-06-20 09:27:13                     <NA>
    #> 50    2.2.0 2005-10-06 10:22:14                     <NA>
    #> 51    2.2.1 2005-12-20 10:35:21                     <NA>
    #> 52    2.3.0 2006-04-24 10:37:20                     <NA>
    #> 53    2.3.1 2006-06-01 08:25:33                     <NA>
    #> 54    2.4.0 2006-10-03 10:15:04                     <NA>
    #> 55    2.4.1 2006-12-18 09:49:23                     <NA>
    #> 56    2.5.0 2007-04-24 09:41:43                     <NA>
    #> 57    2.5.1 2007-06-28 11:17:06                     <NA>
    #> 58    2.6.0 2007-10-03 09:02:53                     <NA>
    #> 59    2.6.1 2007-11-26 14:14:04                     <NA>
    #> 60    2.6.2 2008-02-08 11:10:05                     <NA>
    #> 61    2.7.0 2008-04-22 07:45:29                     <NA>
    #> 62    2.7.1 2008-06-23 07:44:32                     <NA>
    #> 63    2.7.2 2008-08-25 08:53:56                     <NA>
    #> 64    2.8.0 2008-10-20 09:24:01                     <NA>
    #> 65    2.8.1 2008-12-22 09:03:17                     <NA>
    #> 66    2.9.0 2009-04-17 08:32:48                     <NA>
    #> 67    2.9.1 2009-06-26 12:10:57                     <NA>
    #> 68    2.9.2 2009-08-24 08:22:34                     <NA>
    #> 69   2.10.0 2009-10-26 09:02:22                     <NA>
    #> 70   2.10.1 2009-12-14 10:28:24                     <NA>
    #> 71   2.11.0 2010-04-22 08:11:21                     <NA>
    #> 72   2.11.1 2010-05-31 08:10:25                     <NA>
    #> 73   2.12.0 2010-10-15 08:41:57                     <NA>
    #> 74   2.12.1 2010-12-16 09:12:04                     <NA>
    #> 75   2.12.2 2011-02-25 11:07:19                     <NA>
    #> 76   2.13.0 2011-04-13 08:31:27                     <NA>
    #> 77   2.13.1 2011-07-08 09:37:08                     <NA>
    #> 78   2.13.2 2011-09-30 07:05:56                     <NA>
    #> 79   2.14.0 2011-10-31 08:09:09            Great Pumpkin
    #> 80   2.14.1 2011-12-22 08:10:18      December Snowflakes
    #> 81   2.14.2 2012-02-29 08:10:10      Gift-Getting Season
    #> 82   2.15.0 2012-03-30 07:16:05            Easter Beagle
    #> 83   2.15.1 2012-06-22 07:09:44     Roasted Marshmallows
    #> 84   2.15.2 2012-10-26 07:11:16           Trick or Treat
    #> 85   2.15.3 2013-03-01 08:28:29         Security Blanket
    #> 86    3.0.0 2013-04-03 07:12:36            Masked Marvel
    #> 87    3.0.1 2013-05-16 07:11:33               Good Sport
    #> 88    3.0.2 2013-09-25 07:11:09          Frisbee Sailing
    #> 89    3.0.3 2014-03-06 08:12:33               Warm Puppy
    #> 90    3.1.0 2014-04-10 07:11:10             Spring Dance
    #> 91    3.1.1 2014-07-10 07:11:09            Sock it to Me
    #> 92    3.1.2 2014-10-31 08:11:32           Pumpkin Helmet
    #> 93    3.1.3 2015-03-09 08:12:20          Smooth Sidewalk
    #> 94    3.2.0 2015-04-16 07:13:33      Full of Ingredients
    #> 95    3.2.1 2015-06-18 07:15:04   World-Famous Astronaut
    #> 96    3.2.2 2015-08-14 07:13:18              Fire Safety
    #> 97    3.2.3 2015-12-10 08:13:08    Wooden Christmas-Tree
    #> 98    3.2.4 2016-03-10 08:15:45       Very Secure Dishes
    #> 99    3.2.5 2016-04-14 15:59:38 Very, Very Secure Dishes
    #> 100   3.3.0 2016-05-03 07:13:28   Supposedly Educational
    #> 101   3.3.1 2016-06-21 07:21:38         Bug in Your Hair
    #> 102   3.3.2 2016-10-31 08:13:15    Sincere Pumpkin Patch
    #> 103   3.3.3 2017-03-06 08:16:31            Another Canoe
    #> 104   3.4.0 2017-04-21 07:14:45      You Stupid Darkness
    #> 105   3.4.1 2017-06-30 07:04:11            Single Candle
    #> 106   3.4.2 2017-09-28 07:04:35             Short Summer
    #> 107   3.4.3 2017-11-30 08:05:05         Kite-Eating Tree
    #> 108   3.4.4 2018-03-15 08:04:27       Someone to Lean On
    #> 109   3.5.0 2018-04-23 07:04:38           Joy in Playing
    #> 110   3.5.1 2018-07-02 07:04:31            Feather Spray
    #> 111   3.5.2 2018-12-20 08:04:40           Eggshell Igloo
    #> 112   3.5.3 2019-03-11 08:04:49              Great Truth
    #> 113   3.6.0 2019-04-26 07:05:03       Planting of a Tree
    #> 114   3.6.1 2019-07-05 07:05:03       Action of the Toes

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi)
