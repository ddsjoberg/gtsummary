# internal data ----------------------------------------------------------------
df_theme_elements <- readr::read_csv("data-raw/gtsummary_theme_elements.csv")
df_translations <- readr::read_csv("data-raw/gtsummary_translated.csv")

usethis::use_data(df_theme_elements, df_translations,
                  internal = TRUE, overwrite = TRUE)
