library(tidyverse)

## necessary only during gargle development to get devtools' shim for
## system.file()
load_all()
ddi_dir <- system.file("discovery-doc-ingest", package = "gargle")

source(fs::path(ddi_dir, "ingest-functions.R"))

x <- download_discovery_document("discovery:v1")
ee <- read_discovery_document(x)

# api-wide parameters
ap <- ee %>%
  pluck("parameters") %>%
  enframe(name = "property", value = "info") %>%
  mutate(info = map(info, enframe)) %>%
  unnest(cols = info) %>%
  spread(key = name, value = value, convert = TRUE) %>%
  select(property, type, description, everything()) %>%
  write_csv(fs::path(ddi_dir, "api-wide-parameters.csv"))

# properties of a method
mp <- ee %>%
  pluck("schemas", "RestMethod", "properties") %>%
  enframe(name = "property", value = "info") %>%
  mutate(info = map(info, enframe)) %>%
  unnest(cols = info) %>%
  spread(key = name, value = value, convert = TRUE) %>%
  select(property, type, description, everything()) %>%
  write_csv(fs::path(ddi_dir, "method-properties.csv"))

# properties of a parameter of a method
pp <- ee %>%
  pluck("schemas", "JsonSchema", "properties") %>%
  enframe(name = "property", value = "info") %>%
  mutate(info = map(info, enframe)) %>%
  unnest(cols = info) %>%
  spread(key = name, value = value, convert = TRUE) %>%
  select(property, type, description, everything()) %>%
  write_csv(fs::path(ddi_dir, "parameter-properties.csv"))

make_humane_table <- function(df) {
  pad <- function(.x, .n) {
    length(.x) <- .n
    .x
  }
  df %>%
    select(property, type, description) %>%
    mutate(
      description = str_wrap(description, width = 45),
      description = str_split(description, pattern = "\n"),
      n = lengths(description),
      property = map2(property, n, pad),
      type =     map2(type, n, pad),
      n = NULL
    ) %>%
    unnest(cols = c(property, type, description)) %>%
    replace_na(list(property = "", type = "")) %>%
    modify_at(c("property", "type"), ~ format(.x, justify = "left")) %>%
    glue::glue_data("{property} {type} {description}")
}

make_humane_table(ap) %>%
  write_lines(fs::path(ddi_dir, "api-wide-parameters-humane.txt"))

make_humane_table(mp) %>%
  write_lines(fs::path(ddi_dir, "method-properties-humane.txt"))

make_humane_table(pp) %>%
  write_lines(fs::path(ddi_dir, "parameter-properties-humane.txt"))

write_lines(mp$property, fs::path(ddi_dir, "api-wide-parameter-names.txt"))

write_lines(mp$property, fs::path(ddi_dir, "method-property-names.txt"))

write_lines(pp$property, fs::path(ddi_dir, "parameter-property-names.txt"))
