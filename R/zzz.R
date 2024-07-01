.onLoad <- function(...) {
  vctrs::s3_register("pkgdown::pkgdown_print", "gtsummary")
}
