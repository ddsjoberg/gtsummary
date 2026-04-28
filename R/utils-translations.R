# translation function ---------------------------------------------------------

# Fast language lookup that avoids eval_tidy() overhead in get_theme_element().
# The language theme element is always a plain string, so direct access is safe.
.get_language <- function() {
  env_gtsummary_theme[["pkgwide-str:language"]] %||% "en"
}

translate_string <- function(x, language = .get_language()) {
  if (language == "en" || is_empty(x)) return(x) # styler: off

  .translate_grab_one(x, language)
}

translate_vector <- function(x, language = .get_language()) {
  if (language == "en" || is_empty(x)) return(x) # styler: off

  env <- lst_translations[[language]]
  if (is.null(env)) return(x)

  res <- x
  for (x_val in unique(x)) {
    translated <- env[[x_val]]
    if (!is.null(translated)) {
      res[res %in% x_val] <- translated
    }
  }

  res
}

.translate_grab_one <- function(x, language) {
  if (!nzchar(x)) return(x)
  translated <- lst_translations[[language]][[x]]
  if (!is.null(translated)) return(translated)
  x
}
