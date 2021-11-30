
args <- commandArgs(TRUE)

input <- args[[1]]
output <- args[[2]]

data <- readRDS(input)
unlink(input)

for (type in data$type) {

  urls <- contrib.url(data$repos, type)
  for (url in urls) {

    db <- withCallingHandlers(
      tryCatch(available.packages(contriburl = url), error = identity),
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) invokeRestart("muffleMessage")
    )

    if (inherits(db, "error"))
      next

    # save to tempfile
    tmpfile <- tempfile(pattern = "renv-cache-", tmpdir = output)
    on.exit(unlink(tmpfile), add = TRUE)
    saveRDS(db, file = tmpfile)

    # rename from tempfile to cache file
    name <- sprintf("repos_%s.rds.cache", URLencode(url, reserved = TRUE))
    path <- file.path(output, name)
    file.rename(tmpfile, path)

  }

}
