## ---- echo = FALSE------------------------------------------------------------
library(huxtable)
data(jams)
jams$Sugar <- c("Sugar content", "40%", "35%", "50%")

## ---- results = "asis", echo = FALSE------------------------------------------
cat(to_html(jams))

## ---- results = "asis", echo = FALSE------------------------------------------

themes <- paste0("theme_", c(
  "plain",
  "basic",
  "compact",
  "striped",
  "article",
  "bright",
  "grey",
  "blue",
  "orange",
  "green",
  "mondrian"
))

for (th in themes) {
  cat("<div>\n")
  cat("<h2><pre>", th, "</pre></h2>\n", sep = "")
  th_fn <- get(th, pos = "package:huxtable")
  cat(to_html(th_fn(jams)))
  cat("</div>\n\n")
}

