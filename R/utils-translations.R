# translation function ---------------------------------------------------------
translate_text <- function(x, language = get_theme_element("pkgwide-str:language", default = "en")) {
  if (language == "en" || identical(x, character(0))) {
    return(x)
  }

  # sub-setting on row of text to translate
  df_text <- dplyr::filter(df_translations, .data$en == x)

  # if no rows selected OR translation is not provided return x, otherwise the translated text
  ifelse(nrow(df_text) != 1 || is.na(df_text[[language]]), x, df_text[[language]])
}
