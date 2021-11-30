library("gh")
library("jsonlite")
library("jsonld")
library("codemetar")
library("magrittr")
library("readr")
library("dplyr")


codemeta <- "https://doi.org/10.5063/schema/codemeta-2.0"
#github <- codemetar::crosswalk("GitHub")

github <-
  "https://github.com/codemeta/codemeta/raw/master/crosswalk.csv" %>%
  read_csv() %>%
  select(Property, `GitHub`) %>%
  filter(!is.na(`GitHub`))

codemeta_context <- read_json(codemeta)
properties <- names(codemeta_context[[1]])

## replace property names according to crosswalk data.frame
for(i in 1:dim(github)[2]){
 # properties github
}



r <- gh("/repos/:owner/:repo", owner = "ropensci", repo = "EML")
r$`@context` <- github

toJSON(r, auto_unbox = TRUE, pretty = TRUE) %>%
  jsonld_expand() %>%
  jsonld_compact(context = codemeta) -> out



toJSON(r, auto_unbox = TRUE, pretty = TRUE) %>%
  jsonld_expand()

#l <- fromJSON(j)


cm <- codemetar:::new_codemeta()

cm$identifier <- r$id
