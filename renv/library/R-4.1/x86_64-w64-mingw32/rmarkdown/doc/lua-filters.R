## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----modifying-format---------------------------------------------------------
custom_format <- function(...) {
  base_format <- rmarkdown::html_document(...)
  # prepending a new Lua filter to html_document() ones
  base_format$pandoc$lua_filters <- c(
    rmarkdown::pandoc_path_arg("new.lua"), 
    base_format$pandoc$lua_filters)
  base_format
}

basename(custom_format()$pandoc$lua_filters)

## ----creating-format----------------------------------------------------------
custom_format <- function(toc = TRUE, ...) {
  rmarkdown::output_format(
    knitr = rmarkdown::knitr_options(),
    # a new filter will be appended to base_format ones
    pandoc = rmarkdown::pandoc_options(to = "html", lua_filters = "new.lua"),
    base_format = rmarkdown::html_document(toc = toc, ...)
  )
}

basename(custom_format()$pandoc$lua_filters)

