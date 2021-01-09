# sass 0.2.0

* Added new `sass_layer()` and `sass_layer_merge()` functions. See [here](https://rstudio.github.io/sass/articles/sass.html#layers) for more details.

* The objects that `sass()` and `as_sass()`) return now have better print methods in **rmarkdown**. See [here](https://rstudio.github.io/sass/articles/sass.html#rmarkdown) for more details.

* Added the ability for `sass()` to retain `htmltools::htmlDependency()`s attached to it's `input`.  

* Fixed an issue with incorrect handling of length 2 or more character vector input (#37).

# sass 0.1.2

* No significant changes other than CRAN compliance.

# sass 0.1.1

* First release.
