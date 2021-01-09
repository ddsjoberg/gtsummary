


# desc

> Parse DESCRIPTION files

[![Linux Build Status](https://travis-ci.org/r-lib/desc.svg?branch=master)](https://travis-ci.org/r-lib/desc)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/desc?svg=true)](https://ci.appveyor.com/project/gaborcsardi/desc)
[![](http://www.r-pkg.org/badges/version/desc)](http://www.r-pkg.org/pkg/desc)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/desc)](http://www.r-pkg.org/pkg/desc)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/desc/master.svg)](https://codecov.io/github/r-lib/desc?branch=master)

Parse, manipulate and reformat DESCRIPTION files. The package
provides two APIs, one is object oriented, the other one is
procedural and manipulates the files *in place*.

---

  - [Installation](#installation)
  - [The object oriented API](#the-oo-api)
    - [Introduction](#introduction)
    - [Loading or creating new `DESCRIPTION` files](#loading-or-creating-new-description-files)
	- [Normalizing `DESCRIPTION` files](#normalizing-description-files)
	- [Querying, changing and removing fields](#querying-changing-and-removing-fields)
	- [Dependencies](#dependencies)
	- [Collate fields](#collate-fields)
	- [Authors](#authors)
  - [The procedural API](#the-procedural-api)
  - [License](#license)

## Installation


```r
source("https://install-github.me/r-lib/desc")
```

## The object oriented API


```r
library(desc)
```

### Introduction

The object oriented API uses [R6](https://github.com/wch/R6) classes.

### Loading or creating new `DESCRIPTION` files

A new `description` object can be created by reading a `DESCRPTION`
file form the disk. By default the `DESCRIPTION` file in the current
directory is read:


```r
desc <- description$new()
desc
```

```
#> Package: desc
#> Title: Manipulate DESCRIPTION Files
#> Version: 1.0.0
#> Author: Gábor Csárdi
#> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
#> Description: Tools to read, write, create, and manipulate DESCRIPTION
#>     files.  It is intented for packages that create or manipulate other
#>     packages.
#> License: MIT + file LICENSE
#> URL: https://github.com/r-lib/desc
#> BugReports: https://github.com/r-lib/desc/issues
#> Imports:
#>     R6
#> Suggests:
#>     testthat,
#>     whoami,
#>     newpackage
#> Encoding: UTF-8
#> LazyData: true
#> RoxygenNote: 5.0.0
```

A new object can also be created from scratch:


```r
desc2 <- description$new("!new")
desc2
```

```
#> Package: {{ Package }}
#> Title: {{ Title }}
#> Version: 1.0.0
#> Authors@R (parsed):
#>     * Jo Doe <jodoe@dom.ain> [aut, cre]
#> Maintainer: {{ Maintainer }}
#> Description: {{ Description }}
#> License: {{ License }}
#> URL: {{ URL }}
#> BugReports: {{ BugReports }}
#> Encoding: UTF-8
#> LazyData: true
```

### Normalizing `DESCRIPTION` files

Most `DESCRIPTION` fields may be formatted in multiple equivalent
ways. `desc` does not reformat fields, unless they are
updated or reformatting is explicitly requested via a call to
the `normalize()` method or using the `normalize` argument of the
`write()` method.

### Querying, changing and removing fields

`get()` and `set()` queries or updates a field:


```r
desc$set("Package", "foo")
desc$get("Package")
```

```
#> Package 
#>   "foo"
```

They work with multiple fields as well:


```r
desc$set(Package = "bar", Title = "Bar Package")
desc$get(c("Package", "Title"))
```

```
#>       Package         Title 
#>         "bar" "Bar Package"
```

### Dependencies

Package dependencies can be set and updated via an easier API:


```r
desc$get_deps()
```

```
#>       type    package version
#> 1 Suggests   testthat       *
#> 2 Suggests     whoami       *
#> 3 Suggests newpackage       *
#> 4  Imports         R6       *
```

```r
desc$set_dep("mvtnorm")
desc$set_dep("Rcpp", "LinkingTo")
desc$get_deps()
```

```
#>        type    package version
#> 1  Suggests   testthat       *
#> 2  Suggests     whoami       *
#> 3  Suggests newpackage       *
#> 4   Imports         R6       *
#> 5   Imports    mvtnorm       *
#> 6 LinkingTo       Rcpp       *
```

```r
desc
```

```
#> Package: bar
#> Title: Bar Package
#> Version: 1.0.0
#> Author: Gábor Csárdi
#> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
#> Description: Tools to read, write, create, and manipulate DESCRIPTION
#>     files.  It is intented for packages that create or manipulate other
#>     packages.
#> License: MIT + file LICENSE
#> URL: https://github.com/r-lib/desc
#> BugReports: https://github.com/r-lib/desc/issues
#> Imports:
#>     R6,
#>     mvtnorm
#> Suggests:
#>     testthat,
#>     whoami,
#>     newpackage
#> LinkingTo:
#>     Rcpp
#> Encoding: UTF-8
#> LazyData: true
#> RoxygenNote: 5.0.0
```

### Collate fields

Collate fields can be queried and set using simple character
vectors of file names:


```r
desc$set_collate(list.files("../R"))
desc$get_collate()
```

```
#>  [1] "assertions.R"       "authors-at-r.R"     "classes.R"         
#>  [4] "collate.R"          "constants.R"        "deps.R"            
#>  [7] "description.R"      "encoding.R"         "latex.R"           
#> [10] "non-oo-api.R"       "package-archives.R" "read.R"            
#> [13] "remotes.R"          "str.R"              "syntax_checks.R"   
#> [16] "urls.R"             "utils.R"            "validate.R"        
#> [19] "version.R"
```

### Authors

Authors information, when specified via the `Authors@R` field,
also has a simplified API:


```r
desc <- description$new("DESCRIPTION2")
desc$get_authors()
```

```
#> [1] "Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph]"
#> [2] "Peter Danenberg <pcd@roxygen.org> [aut, cph]"        
#> [3] "Manuel Eugster [aut, cph]"                           
#> [4] "RStudio [cph]"
```

```r
desc$add_author("Bugs", "Bunny", email = "bb@acme.com")
desc$add_me()
desc$get_authors()
```

```
#> [1] "Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph]"
#> [2] "Peter Danenberg <pcd@roxygen.org> [aut, cph]"        
#> [3] "Manuel Eugster [aut, cph]"                           
#> [4] "RStudio [cph]"                                       
#> [5] "Bugs Bunny <bb@acme.com>"                            
#> [6] "Gabor Csardi <csardi.gabor@gmail.com> [ctb]"
```

## The procedural API

The procedural API is simpler to use for one-off `DESCRIPTION`
manipulation, since it does not require dealing with
`description` objects. Each object oriented method has a
procedural counterpart that works on a file, and potentially
writes its result back to the same file.

For example, adding a new dependency to `DESCRIPTION` in the
current working directory can be done with


```r
desc_set_dep("newpackage", "Suggests")
```

```
#> Package: desc
#> Title: Manipulate DESCRIPTION Files
#> Version: 1.0.0
#> Author: Gábor Csárdi
#> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
#> Description: Tools to read, write, create, and manipulate DESCRIPTION
#>     files.  It is intented for packages that create or manipulate other
#>     packages.
#> License: MIT + file LICENSE
#> URL: https://github.com/r-lib/desc
#> BugReports: https://github.com/r-lib/desc/issues
#> Imports:
#>     R6
#> Suggests:
#>     testthat,
#>     whoami,
#>     newpackage
#> Encoding: UTF-8
#> LazyData: true
#> RoxygenNote: 5.0.0
```

This added `newpackage` to the `Suggests` field:


```r
desc_get("Suggests")
```

```
#>                                       Suggests 
#> "\n    testthat,\n    whoami,\n    newpackage"
```

So the full list of dependencies are now


```r
desc_get_deps()
```

```
#>       type    package version
#> 1 Suggests   testthat       *
#> 2 Suggests     whoami       *
#> 3 Suggests newpackage       *
#> 4  Imports         R6       *
```

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi),
      [RStudio Inc](https://github.com/rstudio)
