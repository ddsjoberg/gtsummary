## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  results = "hide"
)

knitr::knit_engines$set(list(
  styler = function(options) {
    options$comment <- ""
    knitr::engine_output(
      options,
      {
        before <- options$code
        after <- as.character(styler::style_text(options$code, strict = FALSE))
        if (!identical(trimws(before, "right"), after)) {
          stop(
            "Before unlike after. Before:", paste(before, sep = "\n"),
            "After: ", paste(after, sep = "\n")
          )
        }
        after
      },
      ""
    )
  }
))

## ----setup--------------------------------------------------------------------
library(styler)

