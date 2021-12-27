library(rjson)
library(stringr)
library(whisker)

#TODO extract comments and introduce sections.

cases <- c(readLines("testcases.docopt"), "\n")
cases <- paste0(cases, collapse="\n")

# remove comments (TODO improve)
cases <- str_replace_all(cases, "#.+?\n", "")
cases <- str_replace_all(cases, ",\n", ",")
cases <- unlist(str_split(cases, "\nr"))

#case1 <- cases[1]

p_usage <- 'r?\\"\\"\\"(.*?)\\"\\"\\"\n'
p_test <- '^([^\n]+)\n(.+?)\n\n'
p_prog <- '\\$ prog([^\n]*)\n([^\n]*)\n'

i <- 1
cases <- lapply(cases, function(x){
  case <- list()
  # extract usage text
  case$usage <- str_trim(str_match(x, p_usage)[,2])
  case$name <- str_c("doc", str_pad(i, 2, pad="0"))
  i <<- i + 1
  
  tests <- str_match_all(x, p_prog)[[1]]

  if (length(tests)==0){
    case$failed <- str_replace_all(x, "(^|\n)", "\\1\t\t#")
    return(case)
  }
  
  case$tests <- apply(tests, 1, function(r){
    if (str_sub(r[3],1,1) == "{"){
      json <- fromJSON(r[3])
      output <- deparse(json, control="keepNA", width.cutoff = 500)
      error <- FALSE
    } else {
      json <- list()
      output <- r[3]
      error <- TRUE
    }
    test <- str_replace_all(str_trim(r[1]), "(^|\n)", "\\1\t\t#")
    keys <- deparse(names(json))
    list(test=test, args=str_trim(r[2]), output=output, error=error, keys=keys
        , len=length(json))
  })
  case
})



template <- "
{{#cases}}


#####################

context('{{{name}}}')
doc <- 
'{{{usage}}}'

  {{#failed}}
    #
    # TEST GENERATION FAILED
    #
{{{.}}}
    test_that('failed', stop())
  {{/failed}}
  {{#tests}}
    test_that('parsing \"{{{args}}}\" works',{
{{{test}}}
{{^error}}
      res <- docopt(doc, '{{{args}}}', strict=TRUE)
      expect_equivalent(length(res), {{{len}}})
      expect_equivalent(res[{{{keys}}}], {{{output}}})
{{/error}}
{{#error}}
      expect_error(docopt(doc, '{{{args}}}', strict=TRUE))
{{/error}}
    })
  {{/tests}}
{{/cases}}
"

writeLines(whisker.render(template), "../../tests/testthat/test_specs.R")
