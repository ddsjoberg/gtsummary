
 [![Travis-CI Build Status](https://travis-ci.org/ropensci/codemetar.svg?branch=master)](https://travis-ci.org/ropensci/codemetar) [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/csawpip238vvbd72/branch/master?svg=true)](https://ci.appveyor.com/project/cboettig/codemetar/branch/master) [![Coverage Status](https://img.shields.io/codecov/c/github/ropensci/codemetar/master.svg)](https://codecov.io/github/ropensci/codemetar?branch=master)  [![](http://badges.ropensci.org/130_status.svg)](https://github.com/ropensci/onboarding/issues/130) [![DOI](https://zenodo.org/badge/86626030.svg)](https://zenodo.org/badge/latestdoi/86626030)

<!-- README.md is generated from README.Rmd. Please edit that file -->
codemetar
=========

The goal of codemetar is to generate the JSON-LD file, `codemeta.json` containing software metadata describing an R package. For more general information about the CodeMeta Project for defining software metadata, see <https://codemeta.github.io>. In particular, new users might want to start with the [User Guide](https://codemeta.github.io/user-guide/), while those looking to learn more about JSON-LD and consuming existing codemeta files should see the [Developer Guide](https://codemeta.github.io/developer-guide/).

Installation
------------

You can install the latest version from CRAN using:

``` r
install.packages("codemetar")
```

You can also install the development version of `codemetar` from github with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci/codemetar")
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

Which creates a file looking like so (first 10 lines; see full [codemeta.json here](https://github.com/codemeta/codemetar/blob/master/codemeta.json)):

    {
      "@context": [
        "http://purl.org/codemeta/2.0",
        "http://schema.org"
      ],
      "@type": "SoftwareSourceCode",
      "identifier": "codemetar",
      "description": "The 'Codemeta' Project defines a 'JSON-LD' format for describing\n  software metadata, as detailed at <https://codemeta.github.io>. This package\n  provides utilities to generate, parse, and modify 'codemeta.json' files \n  automatically for R packages, as well as tools and examples for working with\n  'codemeta.json' 'JSON-LD' more generally.",
      "name": "codemetar: Generate 'CodeMeta' Metadata for R Packages",
      "issueTracker": "https://github.com/ropensci/codemetar/issues",

Modifying or enriching CodeMeta metadata
----------------------------------------

The best way to ensure `codemeta.json` is as complete as possible is to begin by making full use of the fields that can be set in an R package DESCRIPTION file, such as `BugReports` and `URL`. Using the `Authors@R` notation allows a much richer specification of author roles, correct parsing of given vs family names, and email addresses.

In the current implementation, developers may specify an ORCID url for an author in the optional `comment` field of `Authors@R`, e.g.

    Authors@R: person("Carl", "Boettiger", role=c("aut", "cre", "cph"), email="cboettig@gmail.com", comment="http://orcid.org/0000-0002-1642-628X")

which will allow `codemetar` to associate an identifier with the person. If the package is hosted on CRAN, including the ORCiD in this way will cause an ORCiD logo and link to the ORCiD page to be added to the package CRAN webpage.

### Using the DESCRIPTION file

The DESCRIPTION file is the natural place to specify any metadata for an R package. The `codemetar` package can detect certain additional terms in the [CodeMeta context](https://codemeta.github.io/terms). Almost any additional codemeta field (see `codemetar:::additional_codemeta_terms` for a list) and can be added to and read from the DESCRIPTION into a `codemeta.json` file.

CRAN requires that you prefix any additional such terms to indicate the use of `schema.org` explicitly, e.g. `keywords` would be specified in a DESCRIPTION file as:

    X-schema.org-keywords: metadata, codemeta, ropensci, citation, credit, linked-data

Where applicable, these will override values otherwise guessed from the source repository. Use comma-separated lists to separate multiple values to a property, e.g. keywords.

See the [DESCRIPTION](https://github.com/codemeta/codemetar/blob/master/DESCRIPTION) file of the `codemetar` package for an example.

Going further
-------------

Check out all the [codemetar vignettes](https://codemeta.github.io/codemetar/articles/index.html) for tutorials on other cool stuff you can do with codemeta and json-ld.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
