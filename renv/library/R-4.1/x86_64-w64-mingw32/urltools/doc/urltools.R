## ---- eval=FALSE---------------------------------------------------------
#  URLdecode("test%gIL")
#  Error in rawToChar(out) : embedded nul in string: '\0L'
#  In addition: Warning message:
#  In URLdecode("%gIL") : out-of-range values treated as 0 in coercion to raw

## ---- eval=FALSE---------------------------------------------------------
#  URLencode("https://en.wikipedia.org/wiki/Article", reserved = TRUE)
#  [1] "https%3a%2f%2fen.wikipedia.org%2fwiki%2fArticle"

## ---- eval=FALSE---------------------------------------------------------
#  library(urltools)
#  url_decode("test%gIL")
#  [1] "test"
#  url_encode("https://en.wikipedia.org/wiki/Article")
#  [1] "https://en.wikipedia.org%2fwiki%2fArticle"

## ---- eval=FALSE---------------------------------------------------------
#  > parsed_address <- url_parse("https://en.wikipedia.org/wiki/Article")
#  > str(parsed_address)
#  'data.frame':	1 obs. of  6 variables:
#   $ scheme   : chr "https"
#   $ domain   : chr "en.wikipedia.org"
#   $ port     : chr NA
#   $ path     : chr "wiki/Article"
#   $ parameter: chr NA
#   $ fragment : chr NA

## ---- eval=FALSE---------------------------------------------------------
#  > url_compose(parsed_address)
#  [1] "https://en.wikipedia.org/wiki/article"

## ---- eval=FALSE---------------------------------------------------------
#  url <- "https://en.wikipedia.org/wiki/Article"
#  scheme(url)
#  "https"
#  scheme(url) <- "ftp"
#  url
#  "ftp://en.wikipedia.org/wiki/Article"

## ---- eval=FALSE---------------------------------------------------------
#  > url <- "https://en.wikipedia.org/wiki/Article"
#  > domain_name <- domain(url)
#  > domain_name
#  [1] "en.wikipedia.org"
#  > str(suffix_extract(domain_name))
#  'data.frame':	1 obs. of  4 variables:
#   $ host     : chr "en.wikipedia.org"
#   $ subdomain: chr "en"
#   $ domain   : chr "wikipedia"
#   $ suffix      : chr "org"

## ---- eval=FALSE---------------------------------------------------------
#  domain_name <- domain("https://en.wikipedia.org/wiki/Article")
#  updated_suffixes <- suffix_refresh()
#  suffix_extract(domain_name, updated_suffixes)

## ---- eval=FALSE---------------------------------------------------------
#  domain_name <- domain("https://en.wikipedia.org/wiki/Article")
#  host_extract(domain_name)

## ---- eval=FALSE---------------------------------------------------------
#  > str(param_get(urls = "http://en.wikipedia.org/wiki/api.php?action=parse&pageid=1023&export=json",
#                       parameter_names = c("pageid","export")))
#  'data.frame':	1 obs. of  2 variables:
#   $ pageid: chr "1023"
#   $ export: chr "json"

## ---- eval=FALSE---------------------------------------------------------
#  url <- "http://en.wikipedia.org/wiki/api.php?action=parse&pageid=1023&export=json"
#  url <- param_set(url, key = "pageid", value = "12")
#  url
#  # [1] "http://en.wikipedia.org/wiki/api.php?action=parse&pageid=12&export=json"

## ---- eval=FALSE---------------------------------------------------------
#  url <- "http://en.wikipedia.org/wiki/api.php"
#  url <- param_set(url, key = "pageid", value = "12")
#  url
#  # [1] "http://en.wikipedia.org/wiki/api.php?pageid=12"

## ---- eval=FALSE---------------------------------------------------------
#  url <- "http://en.wikipedia.org/wiki/api.php?action=parse&pageid=1023&export=json"
#  url <- param_remove(url, keys = c("action","export"))
#  url
#  # [1] "http://en.wikipedia.org/wiki/api.php?pageid=1023"

