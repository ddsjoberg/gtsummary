# translation function ---------------------------------------------------------
translate_string <- function(x, language = get_theme_element("pkgwide-str:language", default = "en")) {
  if (language == "en" || is_empty(x)) return(x) # styler: off

  .translate_grab_one(x, language)
}

translate_vector <- function(x, language = get_theme_element("pkgwide-str:language", default = "en")) {
  if (language == "en" || is_empty(x)) return(x) # styler: off

  res <- x
  for (x_val in unique(x)) {
    res[res %in% x_val] <- .translate_grab_one(x_val, language)
  }

  res
}

.translate_grab_one <- function(x, language) {
  # extract the translation (returns NA if no match)
  x_translated <- df_translations[df_translations$en == x, language, drop = TRUE][1]

  # return original text if translation is NA
  if (!is.na(x_translated)) return(x_translated)
  x
}
