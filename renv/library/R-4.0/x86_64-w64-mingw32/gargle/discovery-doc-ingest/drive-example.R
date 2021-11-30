library(tidyverse)

## necessary only during gargle development to get devtools' shim for
## system.file()
load_all()

source(
  system.file("discovery-doc-ingest", "ingest-functions.R", package = "gargle")
)

x <- download_discovery_document("drive:v3")
dd <- read_discovery_document(x)

methods <- get_raw_methods(dd)

methods <- methods %>% map(groom_properties,  dd)
methods <- methods %>% map(add_schema_params, dd)
methods <- methods %>% map(add_global_params, dd)

## duplicate two methods to create a companion for media
## simpler to do this here, in data, than in wrapper functions
mediafy <- function(target_id, methods) {
  new <- target_method <- methods[[target_id]]

  new$id <- paste0(target_id, ".media")
  new$path <-
    pluck(target_method, "mediaUpload", "protocols", "simple", "path")
  new$parameters <- c(
    new$parameters,
    uploadType = list(list(type = "string", required = TRUE, location = "query"))
  )

  methods[[new$id]] <- new
  methods
}

methods <- mediafy("drive.files.update", methods)
methods <- mediafy("drive.files.create", methods)

.endpoints <- methods
attr(.endpoints, "base_url") <- dd$rootUrl
## View(.endpoints)

# usually you would execute this from *within* the target package,
# but I cannot do so in this example
# please excuse the shenanigans to temporarily target the googledrive project
usethis::with_project(
  "~/rrr/googledrive",
  # line below is the important one!
  usethis::use_data(.endpoints, internal = TRUE, overwrite = TRUE)
)
