.onLoad <- function(...) {
  # this registers as_flextable.gtsummary function
  vctrs::s3_register("flextable::as_flextable", "gtsummary")

  invisible()
}
