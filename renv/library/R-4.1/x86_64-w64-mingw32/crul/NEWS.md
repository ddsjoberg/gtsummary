crul 1.1
========

### NEW FEATURES

* `Paginator` gains support for query parameter combination `page`/`per_page`  to automatically paginate (#145)

### MINOR IMPROVEMENTS

* fix typo (#149) thanks @dpprdan
* Change to how numbers are handled in query parameters. We unfortunately hadn't tested this package with large numbers, which were being converted to scientific notation with a certain number of digits before a decimal. Fixed handling of query parameters to avoid this problem. Fix for `Paginator` as well as for `HttpClient` (#151) (#152) (#153) thanks @ateucher

### BUG FIXES

* sometimes weird response headers are returned in an HTTP response that can not be easily parsed; `crul` would raise an error when this header parsing happens, but now we raise a warning instead (#150)


crul 1.0
========

### ok related changes

* `ok()` can now accept more than 1 status code so that you can check if the status of a url is within a set of status codes rather than equal to 1 status code (#124)
* `ok()` gains a parameter `verb` to use either head or get requests. in addition added more documentation (#125) to the function on how to get the "right answer" for whether a url is ok/up  (#123) (#127)
* `ok()` gains parameter `ua_random`, which if `TRUE`, will use a random user agent string pulled from a vector of 50 user agent strings generated from `charlatan::UserAgentProvider` (#138)

### NEW FEATURES

* gains new async class `AsyncQueue` for doing async requests with rate limits (#139)
* gains new functions `curl_verbose()` and `set_verbose()`. `curl_verbose()` can be set by passing to the initialize step (e.g., `HttpClient$new(url, verbose=curl_verbose())`), and gets more compact verbose curl output, while also getting request body information (and response body optionally). `set_verbose()` is sets `curl_verbose()` globally (#141)
* gains new vignette "How to choose a client" for choosing which crul class to use (e.g., `HttpClient` vs. `Async`)  (#133) (#143)

### MINOR IMPROVEMENTS

* package sticker done, shown in README (#42)
* improve function/class reference page in docs site (#131)
* improvements to the best practices vignette (#132)
* removed unused private variable in the `AsyncVaried` class (#140)
* fix inaccuracy in documentation for the RETRY method (#130)
* `HttpRequest` now adds the query (if present) to the printed url in the print method for the class (it was absent before now) (#128)
* use new roxygen2 support for R6 classes (#126)
* removed `delete-requests` and `post-requests` manual files - mostly redundant with other documentation


crul 0.9.0
==========

### NEW FEATURES

* `HttpResponse` response object gains new methods for checking response content types, includes: `raise_for_ct`, `raise_for_ct_html`, `raise_for_ct_json`, `raise_for_ct_xml`. these behave similarly to `raise_for_status`, and can behave as a warning or raise an error through stop (#119) (#120)

### MINOR IMPROVEMENTS

* fix to prep_body internal function to handle various body inputs; now avoids warning about `as.character.form_file` when both httr and crul are loaded (#112)
* finish off "Failing with fauxpas" section of the "API package best practices" vignette (#121)

### BUG FIXES

* the `head()` verb on `HttpClient` was no capturing `auth` when set on initialization (#122)


crul 0.8.4
==========

### MINOR IMPROVEMENTS

* `jsonlite` package moved to Imports (#112)
* the `parse()` method in the `HttpResponse` object now checks whether the response raw bytes can be converted to character, and if not just returns raw bytes (#115) (#116)
* give vignettes titles (#113) (#114)

### BUG FIXES

* no longer setting `cainfo` curl option, fixes problem arising from change in recent libcurl version (#117)


crul 0.8.0
==========

### NEW FEATURES

* you can now pass on parameters through the `parse()` method of an `HttpResponse` class to the internally called function `iconv()` to finely control the usage of `iconv` for cases in which normal encoding conversion doesn't work  (#110)

### MINOR IMPROVEMENTS

* use `httpcode` package instead of `fauxpas` package within `ok()` function (#108) (#109) thanks @maelle !
* fix links to http testing book - ropensci -> ropenscilabs (#111)


crul 0.7.4
==========

### NEW FEATURES

* event hooks added to `HttpClient`. both request and response hooks supported. not supported in async methods for now (#76) (#107)

### MINOR IMPROVEMENTS

* improve `$parse()` behavior (in the `HttpResponse` object) when using disk or stream. `$parse()` was throwing a warning when using disk and an error when using stream. and improves behavior when doing async requests (#104)
* `Paginator` gains optional progress bar through the new `progress` parameter. In addition, the `cat()` calls inside the method were removed, so as not to insert newlines with each page and to not print "OK" when done (#106) thanks @boshek

### BUG FIXES

* passing on opts/headers now works with `Async` (#101) (#103)
* streaming was broken in `AsyncVaried` with `curl` of a certain version, works now (#102) (#103)


crul 0.7.0
==========

### NEW FEATURES

* `HttpClient` gains a `retry` method: retries any request verb until successful (HTTP response status < 400) or a condition for giving up is met. (#89) (#95) thanks @hlapp
* `HttpClient`, `HttpRequest`, and `Async` classes gain `verb` method for doing HTTP requests specifying any of the supported HTTP verbs  (#97)
* `HttpClient` and `Paginator` gain a `url_fetch` method: get the URL that would be sent in an HTTP request without sending the HTTP request. Useful for getting the URL before executing an HTTP request if you need to check something about the URL first. (#92)
* new vignette for "API package best practices" (#65)
* Package gains manual files for each HTTP verb to facilitate linking to package documentation for information on each HTTP verb (#98)
* Intermediate headers (e.g., those in redirect chains) are now given back in a new slot in the `HttpResponse` class as `$response_headers_all` as an unnamed list, with each element a named list of headers; the last list in the set is the final response headers that match those given in the `$response_headers` slot (#60) (#99)

### BUG FIXES

* some dangling file connections were left open - now fixed (#93) (#95)
* fix `url_parse`: lacked check that input was a string, and that it was length 1 - this PR fixed that (#100) thanks @aaronwolen

### DEFUNCT

* `HttpStubbedResponse` was removed from the package - it may have been used at some point, but is not used in the package anymore (#88)

crul 0.6.0
==========

### NEW FEATURES

* `Async` and `AsyncVaried` now support simple auth, see `?auth`  (#70)
* gains new function `ok()` to ping a URL to see if it's up or not, returns a single boolean (#71) (#73)
* `HttpClient` and `HttpRequest` gain new parameter `progress` that accepts a function to use to construct a progress bar. For now accepts `httr::progress()` but will accept other options in the future (#20) (#81)
* gains a new vignette for curl options (#7)
* can now set curl options globally using new functions `set_auth()`, `set_headers()`, `set_opts()`, `set_proxy()`, and `crul_settings()` (#48) (#85)

### MINOR IMPROVEMENTS

* explicitly import `httpcode::http_code` (#80)
* fix vignette names to make them more clear and add numbers to order them (#64)
* change print function for `Async` and `AsyncVaried` to print max of 10 and tell user how many total and remaining not shown (#72)
* added support to `proxy()` for socks, e.g. to use with TOR (#79)
* now when `Async` and `AsyncVaried` requests fail, they don't error but instead we capture the error and pass it back in the result. this way any failure requests don't stop progress of the entire async request suite (#74) (#84)


crul 0.5.2
==========

### MINOR IMPROVEMENTS

* Fixed handling of user agent: you can pass a UA string 
as a curl option or a header. Previously, we were wrongly overwriting
the user input UA if given as a curl option - but were not doing 
so if given as a header. This is fixed now.  (#63) thx to @maelle and @dpprdan

### BUG FIXES

* Fix to `Paginator` - it wasn't handling pagination correctly. 
In addition, fixed to hopefully handle all scenarios now. added
more tests (#62)
* Fixed handling of query parameters. We were using `urltools::url_encode`
to encode strings, but it wasn't encoding correctly in some locales. Using `curl::curl_escape` fixes the problem. Encoding is done on query values and names  (#67) (#68)

crul 0.5.0
==========

### NEW FEATURES

* Gains a new R6 class `Paginator` to help users automatically paginate through multiple requests. It only supports query parameter based paginating for now. We'll add support later for other types including cursors (e.g., used in Solr servers), and for link headers (e.g., used in the GitHub API). Please get in touch if you find any problems with `Paginator`. (#56)
* Async classes `Async` and `Asyncvaried` gain ability to write to disk and stream data (to disk or elsewhere, e.g. R console or to an R object) (#46) thanks @artemklevtsov for the push to do this

### MINOR IMPROVEMENTS

* Improved documentation for `auth` to indicate that `user` and `pwd` are indeed required - and to further indicate that one can pass in `NULL` to those parameters (similar to an empty string `""` in `httr::authenticate`) when one e.g. may want to use `gssnegotiate` method (#43)
* Fixed query builder so that one can now protect query parameters by wrapping them in `I()` (#55)

### BUG FIXES

* Fixed bug in `head` requests with `HttpClient` when passing `query` parameter - it was failing previously. Added `query` parameter back. (#52)


crul 0.4.0
==========

### NEW FEATURES

* file uploads now work, see new function `upload()` and examples (#25)

### MINOR IMPROVEMENTS

* fixes to reused curl handles - within a connection object only,
not across connection objects (#45)
* `crul` now drops any options passed in to `opts` or to `...` that 
are not in set of allowed curl options, see `curl::curl_options()` (#49)
* cookies should now be persisted across requests within 
a connection object, see new doc `?cookies` for how to set cookies (#44)
* gather cainfo and use in curl options when applicable (#51)
* remove `disk` and `stream` from `head` method in `HttpClient` 
and `HttpRequest` as no body returned in a HEAD request


crul 0.3.8
==========

### BUG FIXES

* Fixed `AsyncVaried` to return async responses in the order that
they were passed in. This also fixes this exact same behavior in 
`Async` because `Async` uses `AsyncVaried` internally. (#41)
thanks @dirkschumacher for reporting



crul 0.3.6
==========

* Note: This version gains support for integration with 
`webmockr`, which is now on CRAN.

### NEW FEATURES

* New function `auth()` to do simple authentication (#33)
* New function `HttpStubbedResponse` for making a stubbed 
response object for the `webmockr` integration (#4)
* New function `mock()` to turn on mocking - it's off by 
default. If `webmockr` is not installed but user attempts 
to use mocking we error with message to install 
`webmockr` (#4)

### MINOR IMPROVEMENTS

* Use `gzip-deflate` by deafult for each request 
to make sure gzip compression is used if the server 
can do it (#34)
* Change `useragent` to `User-Agent` as default user 
agent header (#35)
* Now we make sure that user supplied headers override the 
default headers if they are of the same name (#36)



crul 0.3.4
==========

### NEW FEATURES

* New utility functions `url_build` and `url_parse` (#31)

### MINOR IMPROVEMENTS

* Now using markdown for documentation (#32)
* Better documentation for `AsyncVaried` (#30)
* New vignette on how to use `crul` in realistic
scenarios rather than brief examples to demonstrate
individual features (#29)
* Better documentation for `HttpRequest` (#28)
* Included more tests

### BUG FIXES

* Fixed put/patch/delete as weren't passing body
correctly in `HttpClient` (#26)
* DRY out code for preparing requests - simplify to
use helper functions (#27)


crul 0.3.0
==========

### NEW FEATURES

* Added support for asynchronous HTTP requests, including two new
R6 classes: `Async` and `AsyncVaried`. The former being a simpler
interface treating all URLs with same options/HTTP method, and the latter
allowing any type of request through the new R6 class `HttpRequest` (#8) (#24)
* New R6 class `HttpRequest` to support `AsyncVaried` - this method
only defines a request, but does not execute it. (#8)

### MINOR IMPROVEMENTS

* Added support for proxies (#22)

### BUG FIXES

* Fixed parsing of headers from FTP servers (#21)





crul 0.2.0
==========

### MINOR IMPROVEMENTS

* Created new manual files for various tasks to document
usage better (#19)
* URL encode paths - should fix any bugs where spaces between words
caused errors previously (#17)
* URL encode query parameters - should fix any bugs where spaces between words
caused errors previously (#11)
* request headers now passed correctly to response object (#13)
* response headers now parsed to a list for easier access (#14)
* Now supporting multiple query parameters of the same name, wasn't
possible in last version (#15)




crul 0.1.6
==========

### NEW FEATURES

* Improved options for using curl options. Can manually add
to list of curl options or pass in via `...`. And we
check that user doesn't pass in prohibited options
(`curl` package takes care of checking that options
are valid) (#5)
* Incorporated `fauxpas` package for dealing with HTTP
conditions. It's a Suggest, so only used if installed (#6)
* Added support for streaming via `curl::curl_fetch_stream`.
`stream` param defaults to `NULL` (thus ignored), or pass in a
function to use streaming. Only one of memory, streaming or
disk allowed. (#9)
* Added support for streaming via `curl::curl_fetch_disk`.
`disk` param defaults to `NULL` (thus ignored), or pass in a
path to write to disk instead of use memory. Only one of memory,
streaming or disk allowed. (#12)

### MINOR IMPROVEMENTS

* Added missing `raise_for_status()` method on the
`HttpResponse` class (#10)

### BUG FIXES

* Was importing `httpcode` but wasn't using it in the package.
Now using the package in `HttpResponse`






crul 0.1.0
==========

### NEW FEATURES

* Released to CRAN.
