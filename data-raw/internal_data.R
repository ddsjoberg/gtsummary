# internal data ----------------------------------------------------------------
df_theme_elements <- readr::read_csv("data-raw/gtsummary_theme_elements.csv")

df_add_p_tests <-
  readr::read_csv("data-raw/gtsummary_tests.csv") |>
  dplyr::mutate(
    test_fun = purrr::map(test_fun, ~ switch(!is.na(.x),
      rlang::parse_expr(.x)
    )),
    fun_to_run = purrr::map(fun_to_run, ~ rlang::parse_expr(.x)),
    pseudo_code = stringr::str_glue("`{pseudo_code}`")
  )

df_translations <-
  readxl::read_excel("data-raw/gtsummary_translated.xlsx") |>
  as.data.frame()
if (nrow(df_translations) != nrow(df_translations |> dplyr::select(en) |> dplyr::distinct())) {
  stop("STOOOOOOOOOPPPPP, error in the translations data")
}

# Build hashed environments for O(1) lookup per language.
# Each language gets an environment mapping English strings to translations.
languages <- setdiff(names(df_translations), c("context", "en"))
lst_translations <- lapply(
  stats::setNames(languages, languages),
  function(lang) {
    en <- df_translations$en
    translated <- df_translations[[lang]]
    # only include non-NA translations
    keep <- !is.na(translated)
    env <- new.env(hash = TRUE, parent = emptyenv(), size = sum(keep))
    mapply(
      function(key, val) assign(key, val, envir = env),
      en[keep], translated[keep]
    )
    env
  }
)

special_char <- list()
special_char$interpunct <- "·"

usethis::use_data(
  df_theme_elements,
  lst_translations,
  special_char,
  df_add_p_tests,
  internal = TRUE, overwrite = TRUE
)
