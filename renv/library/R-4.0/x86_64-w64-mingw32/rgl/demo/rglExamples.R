dirname <- tempfile()
dir.create(dirname)
olddir <- setwd(dirname)

show <- c() # list topics to show only those
skip <- c("rgl-package", "shinyGetPar3d", "tkpar3dsave", "tkrgl",
          "tkspin3d", "tkspinControl") # Ones to skip

library(tools)
db <- Rd_db("rgl")
names <- names(db)
if (length(show))
  names <- names[sub("[.]Rd$", "", names) %in% show]

Rmdnames <- sub("[.]Rd$", ".Rmd", names)
htmlnames <- sub("[.]Rd$", ".html", names)

# These functions are based on similar ones from tools
.Rd_deparse <- function (x, tag = TRUE) 
{
    if (!tag) 
        attr(x, "Rd_tag") <- "Rd"
    paste(as.character(x), collapse = "")
}
.Rd_drop_nodes_with_tags <- function (x, tags) 
{
    recurse <- function(e) {
        if (is.list(e)) 
            structure(lapply(e[is.na(match(RdTags(e), tags))], 
                recurse), Rd_tag = attr(e, "Rd_tag"))
        else e
    }
    recurse(x)
}
.Rd_drop_comments <- function (x) 
.Rd_drop_nodes_with_tags(x, "COMMENT")

RdTags <- function (Rd) 
{
    res <- sapply(Rd, attr, "Rd_tag")
    if (!length(res)) 
        res <- character()
    res
}
.Rd_get_section <- function (x, which, predefined = TRUE) 
{
    if (predefined) 
        x <- x[RdTags(x) == paste0("\\", which)]
    else {
        x <- x[RdTags(x) == "\\section"]
        if (length(x)) {
            ind <- sapply(x, function(e) .Rd_get_text(e[[1L]])) == 
                which
            x <- lapply(x[ind], `[[`, 2L)
        }
    }
    if (!length(x)) 
        x
    else structure(x[[1L]], class = "Rd")
}
.Rd_get_example_code <- function (x) 
{
    x <- .Rd_get_section(x, "examples")
    if (!length(x)) 
        return(character())
    x <- .Rd_drop_comments(x)
    recurse <- function(e) {
        if (is.list(e)) {
            unlist(lapply(e[is.na(match(RdTags(e), c(#"\\donttest",
                                                     "\\dontrun")))], 
                recurse))
        }
        else e
    }
    .Rd_deparse(recurse(x), tag = FALSE)
}

writeIndex <- function(names, htmlnames, cols = 4) {
  result <- character()
  if (!is.null(text)) {
    o <- order(names)
    names <- names[o]
    htmlnames <- htmlnames[o]
    entries <- paste0("[", names, "](", htmlnames, ")")
    len <- length(entries)
    padding <- ((len + cols - 1) %/% cols) * cols - len
    if (padding)
      entries <- c(entries, rep("", length.out=padding))
    result <- c(result, '\n<div class="nostripes">\n')
    result <- c(result, knitr::kable(matrix(entries, ncol=cols), format="markdown",
                                     col.names = rep(" ", cols)))
    result <- c(result, "</div>\n")
  }
}

library(rgl)
saveopts <- options(rgl.useNULL = TRUE)
prevlink <- "[Prev](index.html)"
indexlink <- "[Index](index.html)"

for (i in seq_along(names)) {
  Rmd <- file(Rmdnames[i], open = "wt")
  nextlink <- if (i < length(htmlnames)) paste0("[Next](", htmlnames[i+1], ")") else ""
  writeLines(c('---', paste0('title: ', names[i]), 
'output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
initialWd <- getwd()
saveopts <- options()
options(rgl.useNULL = TRUE)
library(rgl)
setupKnitr(autoprint = TRUE)
example <- function(...) {
  saveopts <- options(rgl.printRglwidget = FALSE)
  on.exit(options(saveopts))
  utils::example(...)
  lowlevel(numeric())
}
options(ask = FALSE, examples.ask = FALSE, device.ask.default = FALSE)
```
'), Rmd)
  writeLines(paste(prevlink, nextlink, indexlink), Rmd)
  if (file_path_sans_ext(Rmdnames[i]) %in% skip)
    writeLines(
'```{r eval = FALSE}
# This example is skipped in the demo.', Rmd)
  else
    writeLines('```{r}', Rmd)
  
  code <- .Rd_get_example_code(db[[names[i]]])
  if (length(code))
    writeLines(code, Rmd)
  else
    writeLines("# No example code", Rmd)
  writeLines(
'```
```{r echo=FALSE,include=FALSE}
setwd(initialWd)
while(length(rgl.dev.list())) close3d()
rm(examples)
options(saveopts)
```
', Rmd)
  writeLines(paste(prevlink, nextlink, indexlink), Rmd)
  close(Rmd)
  prevlink <- paste0("[Prev](", htmlnames[i], ")")
  rmarkdown::render(Rmdnames[i])
  while(length(rgl.dev.list())) close3d()
}

indexname <- "index.Rmd"
index <- file(indexname, open = "wt")
writeLines(c(
'---
title: "rgl Examples"
author: "Duncan Murdoch"
output: html_document
---
	
These files show examples from almost every help file
in `rgl`.  
',
writeIndex(names, htmlnames)), index)
close(index)

browseURL(rmarkdown::render(indexname))
options(saveopts)
setwd(olddir)
