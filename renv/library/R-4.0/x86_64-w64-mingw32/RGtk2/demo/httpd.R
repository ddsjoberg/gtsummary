PORT <- 8080
tss <- gSocketService()
addr <- gInetAddressNewAny(GSocketFamily["ipv4"])
saddr <- gInetSocketAddress(addr, PORT)
tss$addAddress(saddr, GSocketType["stream"], GSocketProtocol["tcp"])

setClass("HTTPRequest",
         representation(command = "character",
                        path = "character",
                        args = "character"))

setMethod("show", "HTTPRequest", function(object) {
              cat(object@command, object@path, 
                  if (length(object@args) > 0L)
                      paste("where", paste0(names(object@args), "=",
                                            object@args, collapse=", ")),
                  "\n")
          })

parseRequest <- function(line) {
    parts <- strsplit(line, " ", fixed=TRUE)[[1L]]
    command <- parts[[1L]]
    if (command != "GET") {
        stop("only GET supported")
    }
    url <- XML::parseURI(parts[[2L]])
    query <- strsplit(url$query, "&", fixed=TRUE)[[1L]]
    queryParts <- strsplit(query, "=", fixed=TRUE)
    queryMat <- matrix(as.character(unlist(queryParts)), 2L)
    args <- setNames(queryMat[2L,], queryMat[1L,])
    new("HTTPRequest", command=command, path=url$path, args=args)
}

handleConnection <- function(service, socket, sourceobj) {
    con <- giocon(socket)
    line <- readLines(con, n=1L)
    request <- parseRequest(line)
    print(request)
    writeLines(c("HTTP/1.1 204 No Content", ""), con)
    TRUE
}

gSignalConnect(tss, "incoming", handleConnection)
tss$start()
