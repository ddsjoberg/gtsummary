# crosswalk --------------------------------------------------------------------

#' Crosswalk
#'
#' Crosswalk between different metadata fields used by different repositories,
#' registries and archives. For more details see
#' https://codemeta.github.io/crosswalk This function requires an internet
#' connection to obtain the latest crosswalk table. This function essentially
#' applies the crosswalk table shown by [crosswalk_table()] to a given
#' JSON metadata record.
#' @param x a JSON list or file with data fields to be crosswalked
#' @param from the corresponding column name from the crosswalk table.
#' @param to the column to translate into, assumes "codemeta" by default
#' @param codemeta_context the address or contents of codemeta context. If not
#'   specified, the default "https://doi.org/10.5063/schema/codemeta-2.0" is
#'   used. The default is taken from the option "codemeta_context" and can thus
#'   be overridden by setting this option with `options(codemeta_context =
#'   <your_context_url>)`.
#' @return a `json` object containing a valid codemeta.json file created by
#'   crosswalking the input JSON
#' @export
#' @examples
#' \dontrun{
#' ## Crosswalk data returned by the GitHub API into CodeMeta format
#' r <- gh::gh("/repos/:owner/:repo", owner = "ropensci", repo = "EML")
#' crosswalk(r, "GitHub")
#' }
#'
crosswalk <- function(x, from, to = "codemeta", codemeta_context = NULL) {

  if (!requireNamespace("jsonld", quietly = TRUE)) {
    stop("Package jsonld required. Please install before re-trying.")
  }

  codemeta_context <- default_context_if_null(codemeta_context)

  from_context <- crosswalk_table(from) %>%
    get_crosswalk_context(codemeta_context)

  to_context <- if (to != "codemeta") {

    crosswalk_table(to) %>%
      get_crosswalk_context(codemeta_context) %>%
      toJSON(auto_unbox = TRUE, pretty = TRUE)

  } else {

    codemeta_context
  }

  # ids need to be coerced to character in order to be compacted by jsonld, but
  # shouldn't be coerced if ids don't exist otherwise NULL ids are created and
  # expansion results in error.
  if (!is.null(x$id)) {
    x$id <- as.character(x$id)
  } else if (!is.null(x$owner$id)) {
    x$owner$id <- as.character(x$owner$id)
  } else if (!is.null(x$organization$id)) {
    x$organization$id <- as.character(x$organization$id)
  }

  crosswalk_transform(
    x, crosswalk_context = from_context, codemeta_context = to_context
  )
}

# default_context_if_null ------------------------------------------------------

#' Return Given Context or Default Context if NULL
#'
#' @param codemeta_context context to be checked for `NULL`
#' @return \describe{
#'   \item{`codemeta_context`}{if `codemeta_context` is not `NULL`,}
#'   \item{the value of option `codemeta_context`}{if thisoption is set,}
#'   \item{the value returned by `codemetar:::url_codemeta_schema()`}{else.}
#' }
#' @noRd
default_context_if_null <- function(codemeta_context)
{
  if (is.null(codemeta_context)) {

    getOption("codemeta_context", url_codemeta_schema())

  } else {

    codemeta_context
  }
}

# crosswalk_table --------------------------------------------------------------

#' Crosswalk Table
#'
#' Return a subset of the crosswalk table containing codemeta properties and
#' matching column
#' @param from the name of a column in the crosswalk table to map from.
#' @param to the name of one or more columns in the crosswalk table to map into
#' @param full_crosswalk Path or URL (requires internet!) of the full crosswalk
#'   table.
#' @param trim drop rows giving properties not found in the 'from' column?
#' @importFrom readr read_csv cols
#' @return a tibble containing the trimmed crosswalk table, listing property (in
#'   CodeMeta), and the corresponding terms in both from and to columns.
#' @examples \donttest{
#' crosswalk_table(from = "GitHub", to = c("Zenodo", "Figshare"))
#' }
#' @export
crosswalk_table <- function(
  from, to = NULL, full_crosswalk = NULL, trim = TRUE) {

  if (is.null(full_crosswalk)) {

    github_path <- "codemeta/codemeta/raw/master/crosswalk.csv"

    full_crosswalk <- get_url_github(github_path)
  }

  df <- readr::read_csv(full_crosswalk, col_types = readr::cols(.default = "c"))

  df <- df[c("Property", from, to)]

  if (trim) {

    df[! is.na(df[[from]]), ] # trim to "from" argument fields

  } else {

    df
  }
}

# get_crosswalk_context --------------------------------------------------------

#' Get Crosswalk Context
#'
#' Use a crosswalk table subset to create a context file for the input data
#' This is a JSON-LD representation of the crosswalk for the desired data.
#'
#' @importFrom jsonlite read_json
#' @noRd
get_crosswalk_context <- function(df, codemeta_context = NULL) {

  codemeta_context <- default_context_if_null(codemeta_context)

  context <- jsonlite::read_json(codemeta_context)[[1]]

  context[["id"]] <- NULL ## avoid collisions with @id

  # Continue with a data frame (not with a tibble) so that indexing a column
  # returns a vector
  df <- as.data.frame(df)

  # Lookup the "keys" in the names of the context
  indices <- match(df[, 1], names(context))

  # Which keys have been found?
  found <- ! is.na(indices)

  # Create a new list of key/value pairs
  additional_context <- stats::setNames(context[indices[found]], df[found, 2])

  base_context <- list(
    schema = "http://schema.org/",
    codemeta = "https://codemeta.github.io/terms/"
  )

  list("@context" = c(base_context, additional_context))
}

# crosswalk_transform ----------------------------------------------------------

#' Crosswalk Transform
#'
#' Perform JSON-LD expansion of input, followed by compaction into the codemeta
#' context
#'
#' @inheritParams crosswalk
#' @param crosswalk_context Context to be added to x
#' @return a valid codemeta json description.
#' @importFrom jsonlite toJSON
#' @noRd
crosswalk_transform <- function(
  x, crosswalk_context = NULL, codemeta_context = NULL
) {

  codemeta_context <- default_context_if_null(codemeta_context)

  if (is.null(x$`@context`)) {

    add_context(x, context = crosswalk_context) %>%
      jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
      jsonld::jsonld_expand() %>%
      jsonld::jsonld_compact(context = codemeta_context)

  } else {

    jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE) %>%
      jsonld::jsonld_expand() %>%
      jsonld::jsonld_compact(context = codemeta_context)

  }


}

# drop_context -----------------------------------------------------------------

#' Drop Context
#'
#' Drop context element from json list or json string
#'
#' @param x a JSON list (from [jsonlite::read_json()] /
#'   [jsonlite::fromJSON()]) or json object (from
#'   [jsonlite::toJSON()])
#' @param json_output logical, should output be a json object or a list?
#' @return a list or json object with the "@context" element removed
#' @importFrom jsonlite toJSON fromJSON
#' @export
drop_context <-function(x, json_output = FALSE) {

  # Remove context by setting the corresponding list element to NULL
  set_context(x, NULL, json_output)
}

# add_context ------------------------------------------------------------------

#' Add Context
#'
#' Add context element to json list or json string
#'
#' @param x a JSON list (from [jsonlite::read_json()] /
#'   [jsonlite::fromJSON()]) or json object (from
#'   [jsonlite::toJSON()])
#' @param json_output logical, should output be a json object or a list?
#' @param context context to be added, in same format as x
#' @return a list or json object with "@context" element added
#' @export
add_context <- function(x, context, json_output = FALSE) {

  new_context <- from_json_if(is_json_or_character(context), context)

  set_context(x, new_context, json_output)
}

# set_context ------------------------------------------------------------------

#' Set Context
#'
#' Set context element in json list or json string
#'
#' @param x a JSON list (from read_json / fromJSON) or json object (from toJSON)
#' @param new_context new value for the context element
#' @param json_output logical, should output be a json object or a list?
#' @return a list or json object with "@context" element set to `context`
#' @noRd
set_context <- function(x, new_context, json_output = FALSE) {

  # is x of class "json" or character?
  json_input <- is_json_or_character(x)

  # if yes, call fromJSON() on x
  x <- from_json_if(json_input, x)

  ## TODO: make sure context doesn't already have a "@context" property
  x[["@context"]] <- new_context

  # convert x to json if requested or if the input was json
  to_json_if(json_output || json_input, x, auto_unbox = TRUE, pretty = TRUE)
}
