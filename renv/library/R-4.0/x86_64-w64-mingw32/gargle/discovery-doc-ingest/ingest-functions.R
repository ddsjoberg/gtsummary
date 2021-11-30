library(tidyverse)

#' Get versioned IDs from API Discovery Service
#'
#' @return A character vector.
#' @keywords internal
#' @examples
#' get_discovery_ids()
#' grep("drive", get_discovery_ids(), value = TRUE)
#' grep("sheets", get_discovery_ids(), value = TRUE)
#' grep("gmail", get_discovery_ids(), value = TRUE)
#' grep("bigquery", get_discovery_ids(), value = TRUE)
get_discovery_ids <- function() {
  apis <- httr::content(
    httr::GET("https://www.googleapis.com/discovery/v1/apis")
  )
  map_chr(apis[["items"]], "id")
}

#' Form the URL for a Discovery Document
#'
#' @param id Versioned ID string for target API. Use [get_discovery_ids()] to
#'   see them all and find the one you want.
#' @return A URL to a JSON file.
#' @keywords internal
#' @examples
#' make_discovery_url("sheets:v4")
make_discovery_url <- function(id) {
  av <- set_names(as.list(strsplit(id, split =":")[[1]]), c("api", "version"))
  ## https://developers.google.com/discovery/v1/reference/apis/getRest
  getRest_url <-
    "https://www.googleapis.com/discovery/v1/apis/{api}/{version}/rest"
  glue::glue_data(av, getRest_url)
}

#' List (likely) Discovery Documents in a local folder
#'
#' @param id Optional ID string, possibly versioned, for target API. Use
#'   [get_discovery_ids()] to see them all and find the one you want.
#' @param path Optional directory in which to look. Defaults to `data-raw`
#'   within the current project.
#'
#' @return Files whose names "look like" a Discovery Document
#' @keywords internal
#' @examples
#' list_discovery_documents()
#' list_discovery_documents("sheets")
#' list_discovery_documents("sheets:v4")
list_discovery_documents <- function(id = NULL, path = NULL) {
  path <- path %||% rprojroot::find_package_root_file("data-raw")
  if (!is.null(id)) {
    if (!grepl(":", id)) {
      id <- glue::glue("{id}:v[[:digit:]]+")
    }
    id <- sub(":", "-", id)
  }
  id <- id %||% "[[:alnum:]]+[-][[:alnum:]]+"
  date <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
  regexp <- glue::glue("{id}_{date}[.]json")
  fs::dir_ls(path = path, regexp = regexp) %>%
    fs::path_rel(path)
}

#' Download a Discovery Document
#'
#' @param id Versioned ID string for target API. Use [get_discovery_ids()] to
#'   see them all and find the one you want.
#' @param path Target filepath. Default filename is formed from the API's
#'   versioned ID and the Discovery Document's revision date. Default parent
#'   directory is the current package's `data-raw/` directory, if such exists,
#'   or current working directory, otherwise.
#'
#' @return Filepath
#' @keywords internal
#' @examples
#' download_discovery_document("drive:v3")
#' download_discovery_document("sheets:v4")
#' download_discovery_document("gmail:v1")
#' download_discovery_document("bigquery:v2")
#' download_discovery_document("docs:v1")
#' download_discovery_document("youtube:v3")
download_discovery_document <- function(id, path = NULL) {
  url <- make_discovery_url(id)
  dd <- httr::GET(url)
  httr::stop_for_status(dd, glue::glue("find Discovery Document for ID '{id}'"))

  if (is.null(path)) {
    dd_content <- httr::content(dd)
    api_date <- dd_content[c("revision", "id")]
    api_date <- c(
      id = sub(":", "-", api_date$id),
      revision = as.character(as.Date(api_date$revision, format = "%Y%m%d"))
    )
    json_filename <- fs::path(paste(api_date, collapse = "_"), ext = "json")
    data_raw <- rprojroot::find_package_root_file("data-raw")
    path <- if (fs::dir_exists(data_raw)) {
      fs::path(data_raw, json_filename)
    } else {
      json_filename
    }
  }

  writeLines(httr::content(dd, as = "text"), path)
  path
}

#' Read a Discovery Document
#'
#' @param path Path to a JSON Discovery Document
#'
#' @return A list
#' @examples
#' drive <- "data-raw/drive-v3_2019-02-07.json"
#' dd <- read_discovery_document(drive)
read_discovery_document <- function(path) {
  jsonlite::fromJSON(path)
}

#' Get raw methods
#'
#' https://developers.google.com/discovery/v1/using#discovery-doc-methods
#'
#' @param dd List representing a Discovery Document
#'
#' @return a named list with one element per method
#' @examples
#' drive <- "data-raw/drive-v3_2019-02-07.json"
#' dd <- read_discovery_document(drive)
#' ee <- get_raw_methods(dd)
get_raw_methods <- function(dd) {
  dd %>%
    pluck("resources") %>%
    map("methods") %>%
    flatten() %>%
    set_names(map_chr(., "id"))
}

#' Groom method properties
#'
#' Tweak raw method properties to make them more useful to us downstream:
#'
#'   * Prepend the API's `servicePath` to `path`s.
#'   * Remove the constant stem `"https://www.googleapis.com/auth/"` from
#'     scopes and collapse multiple scopes into one comma-separated string.
#'   * Elevate any `$ref` part of `request` or `response` to be the actual
#'     data for `request` or `response`.
#'   * Reorder the properties so they appear in a predictable order. However,
#'     we do not turn missing properties into explicitly missing properties,
#'     i.e. we don't guarantee all methods have the same properties.
#'
#' We don't touch the `parameters` list here, because it needs enough work to
#' justify having separate functions for that.
#'
#' @param methods A named list of raw methods, from [get_raw_methods()]
#' @param dd A Discovery Document as a list, from [read_discovery_document()]
#'
#' @return A named list of "less raw" methods
groom_properties <- function(method, dd) {
  method$path <- fs::path(dd$servicePath, method$path)

  condense_scopes <- function(scopes) {
    scopes %>%
      str_remove("https://www.googleapis.com/auth/") %>%
      str_c(collapse = ", ")
  }
  method$scopes <- condense_scopes(method$scopes)

  ## I am currently ignoring the fact that `request` sometimes has both
  ## a `$ref` and a `parameterName` part in the original JSON
  if (has_name(method, "request")) {
    method$request <- method$request$`$ref`
  }
  if (has_name(method, "response")) {
    method$response <- method$response$`$ref`
  }

  # all of the properties in the RestMethod schema, in order of usefulness
  property_names <- c(
    "id", "httpMethod", "path", "parameters", "scopes", "description",
    "request", "response",
    "mediaUpload", "supportsMediaDownload", "supportsMediaUpload",
    "useMediaDownloadService",
    "etagRequired", "parameterOrder", "supportsSubscription"
  )

  method[intersect(property_names, names(method))]
}

#' Expand schema placeholders
#'
#' Adds the properties associated with a `request` schema to a method's
#' parameter list.
#'
#' Some methods can send an instance of a API resource in the body of a request.
#' This is indicated by the presence of a schema in the method's `request`
#' property. For example, the `drive.files.copy` method permits a "Files
#' resource" in the request body. This is how you convey the desired `name` of
#' the new copy.
#'
#' In practice, this means you can drop such metadata in the body. That is, you
#' don't actually have to label this explicitly as having `kind = drive#file`
#' (although that would probably be more proper!), nor do you have to include
#' all the possible pieces of metadata that constitute a "Files resource". Just
#' specify the bits that you need to.
#'
#' https://developers.google.com/drive/api/v3/reference/files/copy
#' https://developers.google.com/drive/api/v3/reference/files#resource
#'
#' This function consults the method's `request` and, if it holds a schema, the
#' schema metadata is appended to the method's existing parameters. This way our
#' request building functions recognize the keys and know that such info belongs
#' in the body (vs. the url or the query).
#'
#' @param method A single method
#' @param dd A Discovery Document as a list, from [read_discovery_document()]
#'
#' @return The input method, but with a potentially expanded parameter list.
add_schema_params <- function(method, dd) {
  req <- pluck(method, "request")
  if (is.null(req)) {
    return(method)
  }

  id <- method$id
  schema_params <- dd[[c("schemas", req, "properties")]]
  schema_params <- modify(schema_params, ~ `[[<-`(.x, "location", "body"))

  message(glue::glue("{id} gains {req} schema params\n"))
  method$parameters <- c(method$parameters, schema_params)
  method
}

#' Add API-wide parameters
#'
#' Certain parameters are sensible for any request to a specific API and,
#' indeed, are usually common across APIs. Examples are "fields", "key", and
#' "oauth_token". This function appends these parameters to a method's parameter
#' list. Yes, this means some info is repeated in all methods, but this way our
#' methods are more self-contained and our request building functions can be
#' simpler.
#'
#' @param method A single method
#' @param dd A Discovery Document as a list, from [read_discovery_document()]
#'
#' @return The input method, but with an expanded parameter list.
add_global_params <- function(method, dd) {
  method[["parameters"]] <- c(method[["parameters"]], dd[["parameters"]])
  method
}
