# internal data ----------------------------------------------------------------
df_theme_elements <- readr::read_csv("data-raw/gtsummary_theme_elements.csv")
df_translations <- readxl::read_excel("data-raw/gtsummary_translated.xlsx")
df_add_p_tests <-
  readr::read_csv("data-raw/gtsummary_tests.csv") %>%
  dplyr::mutate(
    test_fun = purrr::map(test_fun, ~switch(!is.na(.x), rlang::parse_expr(.x))),
    fun_to_run = purrr::map(fun_to_run, ~rlang::parse_expr(.x)),
    pseudo_code = stringr::str_glue("`{pseudo_code}`")
  )

special_char <- list()
special_char$interpunct <- "·"

usethis::use_data(df_theme_elements,
                  df_translations,
                  special_char,
                  df_add_p_tests,
                  internal = TRUE, overwrite = TRUE)
