# Standalone file: do not edit by hand
# Source: <https://github.com/ddsjoberg/standalone/blob/main/R/standalone-stringr.R>
# ----------------------------------------------------------------------
#
# ---
# file: standalone-stringr.R
# last-updated: 2024-06-05
# license: https://unlicense.org
# imports: rlang
# ---
#
# This file provides a minimal shim to provide a stringr-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# ## Changelog
#
# nocov start
# styler: off

str_trim <- function(string, side = c("both", "left", "right")) {
  side <- rlang::arg_match(side)
  trimws(x = string, which = side, whitespace = "[ \t\r\n]")
}

str_squish <- function(string, fixed = FALSE) {
  string <- gsub("\\s+", " ", string)  # Replace multiple white spaces with a single white space
  string <- gsub("^\\s+|\\s+$", "", string)  # Trim leading and trailing white spaces
  return(string)
}

str_remove <- function (string, pattern, fixed = FALSE) {
  sub (x = string, pattern = pattern, replacement = "", fixed = fixed)
}

str_remove_all <- function(string, pattern, fixed = FALSE) {
  gsub(x = string, pattern = pattern, replacement = "", fixed = fixed)
}

str_extract <- function(string, pattern, fixed = FALSE) {
  res <- rep(NA_character_, length.out = length(string))
  res[str_detect(string, pattern, fixed = fixed)] <-
    regmatches(x = string, m = regexpr(pattern = pattern, text = string, fixed = fixed))

  res
}

str_extract_all <- function(string, pattern, fixed = FALSE) {
  regmatches(x = string, m = gregexpr(pattern = pattern, text = string, fixed = fixed))
}

str_detect <- function(string, pattern, fixed = FALSE) {
  grepl(pattern = pattern, x = string, fixed = fixed)
}

str_replace <- function(string, pattern, replacement, fixed = FALSE){
  sub(x = string, pattern = pattern, replacement = replacement, fixed = fixed)
}

str_replace_all <- function (string, pattern, replacement, fixed = FALSE){
  gsub(x = string, pattern = pattern, replacement = replacement, fixed = fixed, perl = !fixed)
}

word <- function(string, start, end = start, sep = " ", fixed = TRUE) {
  # Handle vectorized string input
  if (length(string) > 1) {
    return(sapply(string, word, start, end, sep, fixed, USE.NAMES = FALSE))
  }

  words <- unlist(strsplit(string, split = sep, fixed = fixed))
  words <- words[words != ""]  # Remove empty strings

  # Adjust negative indices
  n <- length(words)
  if (start < 0) {
    start <- n + start + 1
  }
  if (end < 0) {
    end <- n + end + 1
  }

  # Validate indices
  if (start < 1 || end > n || start > end) {
    return(NA)
  } else {
    extracted_words <- words[start:end]
    return(paste(extracted_words, collapse = sep))
  }
}

str_sub <- function(string, start = 1L, end = -1L){
  str_length <- nchar(string)

  # Adjust start and end indices for negative values
  if (start < 0) {
    start <- str_length + start + 1
  }
  if (end < 0) {
    end <- str_length + end + 1
  }

  substr(x = string, start = start, stop = end)
}

str_sub_all <- function(string, start = 1L, end = -1L){
  lapply(string, function(x) substr(x, start = start, stop = end))
}

str_pad <- function(string, width, side = c("left", "right", "both"), pad = " ", use_width = TRUE){
  side <- match.arg(side, c("left", "right", "both"))

  if (side == "both") {
    pad_left <- (width - nchar(string)) %/% 2
    pad_right <- width - nchar(string) - pad_left
    padded_string <- paste0(strrep(pad, pad_left), string, strrep(pad, pad_right))
  } else {
    format_string <- ifelse(side == "right", paste0("%-", width, "s"),
                            ifelse(side == "left", paste0("%", width, "s"),
                                   paste0("%", width, "s")))

    padded_string <- sprintf(format_string, string)
  }

  return(padded_string)
}

# nocov end
# styler: on
