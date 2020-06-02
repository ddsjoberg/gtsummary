# internal data ----------------------------------------------------------------
df_theme_elements <- readr::read_csv("data-raw/gtsummary_theme_elements.csv")

usethis::use_data(df_theme_elements, internal = TRUE, overwrite = TRUE)
