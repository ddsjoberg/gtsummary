# internal data ----------------------------------------------------------------
df_theme_elements <- readr::read_csv("data-raw/gtsummary_theme_elements.csv")
df_translations <- readxl::read_excel("data-raw/gtsummary_translated.xlsx")

special_char <- list()
special_char$interpunct <- "·"

usethis::use_data(df_theme_elements,
                  df_translations,
                  special_char,
                  internal = TRUE, overwrite = TRUE)
