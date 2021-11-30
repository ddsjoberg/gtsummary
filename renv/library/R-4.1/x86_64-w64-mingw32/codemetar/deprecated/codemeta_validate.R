#' codemeta validate
#'
#' Checks that a round-trip of expanding and compacting json-ld matches the
#' original document.  Incorrect schema terms or types (or spelling errors of
#' properties) will cause this round-trip to fail by not fully compacting.
#'
#' @param codemeta path/filename to a codemeta.json file, or json-ld text string
#' @param context URL (or path or json string) for the codemeta context. Leave
#'   as default or use appropriate DOI for the version; see details.
#' @details by default, validation will use the original context from the import
#'   file.
#' @importFrom jsonlite toJSON read_json fromJSON
#' @export
#' @examples
#' ex <- system.file("examples/codemeta.json", package="codemetar")
#' codemeta_validate(ex)
#'
codemeta_validate <- function(codemeta = "codemeta.json", context = NULL) {

  if(!pingr::is_online())
    return(warning("validation requires an internet connection",
                   call. = FALSE))

  if (!requireNamespace("jsonld", quietly = TRUE)) {
    stop("Package jsonld required. Please install before re-trying.")
  }

  A <- if (file.exists(codemeta)) {

    read_json(codemeta)

  } else if (is(codemeta, "json")) {

    fromJSON(codemeta)
  }
  # else ?

  if (is.null(context)) {

    context <- A$`@context`

    if (length(context) > 1) {

      context <- jsonlite::toJSON(list("@context" = context), auto_unbox = TRUE)
    }
  }

  ## Expand and Compact
  testfile <- tempfile(fileext = ".json")

  codemeta %>%
    jsonld::jsonld_expand() %>%
    jsonld::jsonld_compact(context) %>%
    writeLines(con = testfile)

  ## Same properties in each
  B <- jsonlite::read_json(testfile)

  unlink(testfile)

  ## drop context, we don't care if one is literal and one the URL
  A$`@context` <- NULL
  B$`@context` <- NULL

  ## Same number of properties as we started with?
  same_n_properties <- identical(length(unlist(A)), length(unlist(B)))

  ## Did any properties fail to compact back?
  compaction_fail <- any(grepl(names(unlist(B)) , pattern = ":"))

  same_n_properties && ! compaction_fail
}
