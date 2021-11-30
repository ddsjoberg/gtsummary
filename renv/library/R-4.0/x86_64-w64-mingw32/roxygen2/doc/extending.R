## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ----setup--------------------------------------------------------------------
library(roxygen2)

## -----------------------------------------------------------------------------
roxy_tag("name", "Hadley")
str(roxy_tag("name", "Hadley"))

## -----------------------------------------------------------------------------
text <- "
  #' This is a title
  #'
  #' This is the description.
  #'
  #' @param x,y A number
  #' @export
  f <- function(x, y) x + y
"

# parse_text() returns a list of blocks, so I extract the first
block <- parse_text(text)[[1]]
block

## -----------------------------------------------------------------------------
#' @tip The mean of a logical vector is the proportion of `TRUE` values.
#' @tip You can compute means of dates and date-times!

## -----------------------------------------------------------------------------
roxy_tag_parse.roxy_tag_tip <- function(x) {
  tag_markdown(x)
}

## ---- include = FALSE---------------------------------------------------------
# Needed for vignette
registerS3method("roxy_tag_parse", "roxy_tag_tip", roxy_tag_parse.roxy_tag_tip)

## -----------------------------------------------------------------------------
text <- "
  #' Title
  #'
  #' @tip The mean of a logical vector is the proportion of `TRUE` values.
  #' @tip You can compute means of dates and date-times!
  #' @md
  f <- function(x, y) {
    # ...
  }
"
block <- parse_text(text)[[1]]
block

str(block$tags[[2]])

## -----------------------------------------------------------------------------
roxy_tag_rd.roxy_tag_tip <- function(x, base_path, env) {
  rd_section("tip", x$val)
}

## ---- include = FALSE---------------------------------------------------------
# Needed for vignette
registerS3method("roxy_tag_rd", "roxy_tag_tip", roxy_tag_rd.roxy_tag_tip)

## -----------------------------------------------------------------------------
format.rd_section_tip <- function(x, ...) {
  paste0(
    "\\section{Tips and tricks}{\n",
    "\\itemize{\n",
    paste0("  \\item ", x$value, "\n", collapse = ""),
    "}\n",
    "}\n"
  )
}

## ---- include = FALSE---------------------------------------------------------
# Needed for vignette
registerS3method("format", "rd_section_tip", format.rd_section_tip)

## -----------------------------------------------------------------------------
topic <- roc_proc_text(rd_roclet(), text)[[1]]
topic$get_section("tip")

## -----------------------------------------------------------------------------
roxy_tag_parse.roxy_tag_memo <- function(x) {
  if (!grepl("^\\[.*\\].*$", x$raw)) {
    roxy_tag_warning(x, "Invalid memo format")
    return()
  }

  parsed <- stringi::stri_match(str = x$raw, regex = "\\[(.*)\\](.*)")[1, ]

  x$val <- list(
    header = parsed[[2]], 
    message = parsed[[3]]
  )
  x
}

## ---- include = FALSE---------------------------------------------------------
# Needed for vignette
registerS3method("roxy_tag_parse", "roxy_tag_memo", roxy_tag_parse.roxy_tag_memo)

## -----------------------------------------------------------------------------
text <- "
  #' @memo [TBI] Remember to implement this!
  #' @memo [API] Check best API
  f <- function(x, y) {
    # ...
  }
"
block <- parse_text(text)[[1]]
block

str(block$tags[[1]])

## -----------------------------------------------------------------------------
memo_roclet <- function() {
  roclet("memo")
}

## -----------------------------------------------------------------------------
roclet_process.roclet_memo <- function(x, blocks, env, base_path) {
  results <- list()
  
  for (block in blocks) {
    tags <- block_get_tags(block, "memo")

    for (tag in tags) {
      msg <- paste0("[", tag$file, ":", tag$line, "] ", tag$val$message)
      results[[tag$val$header]] <- c(results[[tag$val$header]], msg)
    }
  }
  
  results
}

## -----------------------------------------------------------------------------
roclet_output.roclet_memo <- function(x, results, base_path, ...) {
  for (header in names(results)) {
    messages <- results[[header]]
    cat(paste0(header, ": ", "\n"))
    cat(paste0(" * ", messages, "\n", collapse = ""))
  }

  invisible(NULL)
}

## ---- include = FALSE---------------------------------------------------------
# Needed for vignette
registerS3method("roclet_process", "roclet_memo", roclet_process.roclet_memo)
registerS3method("roclet_output", "roclet_memo", roclet_output.roclet_memo)

## -----------------------------------------------------------------------------
results <- roc_proc_text(memo_roclet(), "
#' @memo [TBI] Remember to implement this!
#' @memo [API] Check best API
f <- function(x, y) {
  # ...
}

#' @memo [API] Consider passing z option
g <- function(x, y) {
  # ...
}
")
roclet_output(memo_roclet(), results)

