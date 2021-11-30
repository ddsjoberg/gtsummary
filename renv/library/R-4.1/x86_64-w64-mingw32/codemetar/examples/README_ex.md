
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Travis-CI Build Status](https://travis-ci.org/codemeta/codemetar.svg?branch=master)](https://travis-ci.org/codemeta/codemetar) [![Coverage Status](https://img.shields.io/codecov/c/github/codemeta/codemetar/master.svg)](https://codecov.io/github/codemeta/codemetar?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/codemetar)](https://cran.r-project.org/package=codemetar)

<!-- README.md is generated from README.Rmd. Please edit that file -->
codemetar
=========

The goal of codemetar is to generate the JSON-LD file, `codemeta.json` containing software metadata describing an R package

Installation
------------

You can install codemetar from github with:

``` r
# install.packages("devtools")
devtools::install_github("codemeta/codemetar")
```

``` r
library("codemetar")
```

Example
-------

This is a basic example which shows you how to generate a `codemeta.json` for an R package (e.g. for `testthat`):

``` r
write_codemeta("testthat")
```

`codemetar` can take the path to the package root instead. This may allow `codemetar` to glean some additional information that is not available from the description file alone.

``` r
write_codemeta(".")
```

    {
      "@context": "https://raw.githubusercontent.com/codemeta/codemeta/master/codemeta.jsonld",
      "@type": "SoftwareSourceCode",
      "identifier": "codemetar",
      "description": "Codemeta defines a 'JSON-LD' format for describing software metadata.\n    This package provides utilities to generate, parse, and modify codemeta.jsonld\n    files automatically for R packages.",
      "name": "codemetar: Generate CodeMeta Metadata for R Packages",
      "codeRepository": "https://github.com/codemeta/codemetar",
      "issueTracker": "https://github.com/codemeta/codemetar/issues",
      "license": "https://spdx.org/licenses/MIT",
      "version": "0.1.0",
      "programmingLanguage": {
        "@type": "ComputerLanguage",
        "name": "R",
        "version": "3.4.0",
        "url": "https://r-project.org"
      },
      "runtimePlatform": "R version 3.4.0 (2017-04-21)",
      "provider": {
        "@id": "https://cran.r-project.org",
        "@type": "Organization",
        "name": "Central R Archive Network (CRAN)",
        "url": "https://cran.r-project.org"
      },
      "author": [
        {
          "@type": "Person",
          "givenName": "Carl",
          "familyName": "Boettiger",
          "email": "cboettig@gmail.com",
          "@id": "http://orcid.org/0000-0002-1642-628X"
        }
      ],
      "copyrightHolder": [
        {
          "@type": "Person",
          "givenName": "Carl",
          "familyName": "Boettiger",
          "email": "cboettig@gmail.com",
          "@id": "http://orcid.org/0000-0002-1642-628X"
        }
      ],
      "maintainer": {
        "@type": "Person",
        "givenName": "Carl",
        "familyName": "Boettiger",
        "email": "cboettig@gmail.com",
        "@id": "http://orcid.org/0000-0002-1642-628X"
      },
      "softwareSuggestions": [
        {
          "@type": "SoftwareApplication",
          "name": "testthat",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "jsonvalidate",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "covr",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "knitr",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "rmarkdown",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "httr",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "magrittr",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "readr",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "xml2",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        }
      ],
      "softwareRequirements": [
        {
          "@type": "SoftwareApplication",
          "name": "jsonlite",
          "version": "1.3",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "jsonld",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "git2r",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "devtools",
          "provider": {
            "@id": "https://cran.r-project.org",
            "@type": "Organization",
            "name": "Central R Archive Network (CRAN)",
            "url": "https://cran.r-project.org"
          }
        },
        {
          "@type": "SoftwareApplication",
          "name": "methods"
        },
        {
          "@type": "SoftwareApplication",
          "name": "R",
          "version": "3.0.0"
        }
      ],
      "contIntegration": "https://travis-ci.org/codemeta/codemetar",
      "developmentStatus": "wip",
      "releaseNotes": "https://github.com/codemeta/codemetar/blob/master/README.md",
      "readme": "https://github.com/codemeta/codemetar/blob/master/README.md",
      "fileSize": "119.263KB"
    }

Enriching CodeMeta metadata
---------------------------

The best way to ensure `codemeta.json` is as complete as possible is to begin by making full use of the fields that can be set in an R package DESCRIPTION file, such as `BugReports` and `URL`. Using the `Authors@R` notation allows a much richer specification of author roles, correct parsing of given vs family names, and email addresses.

In the current implementation, developers may specify an ORCID url for an author in the optional `comment` field of `Authors@R`, e.g.

    Authors@R: person("Carl", "Boettiger", role=c("aut", "cre", "cph"), email="cboettig@gmail.com", comment="http://orcid.org/0000-0002-1642-628X")

which will allow `codemetar` to associate an identifier with the person. This is clearly something of a hack since R's `person` object lacks an explicit notion of `id`, and may be frowned upon.

Additional metadata can be added by creating and manipulating a `codemeta` list in R:

``` r
cm <- create_codemeta(".")
cm$keywords <- list("metadata", "ropensci")
write_codemeta(cm)
```
