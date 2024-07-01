.onLoad <- function(...) {
  if ("pkgdown" %in% rownames(installed.packages())) {
    vctrs::s3_register("pkgdown::pkgdown_print", "gtsummary")
  }
}
