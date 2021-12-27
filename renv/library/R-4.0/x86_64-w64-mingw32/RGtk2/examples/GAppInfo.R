file <- gFileNewForCommandlineArg(uri_from_commandline)

uri <- file$getUri()
identical(uri, uri_from_commandline) # FALSE

if (file$hasUriScheme("cdda"))
  {
    ## do something special with uri
  }
