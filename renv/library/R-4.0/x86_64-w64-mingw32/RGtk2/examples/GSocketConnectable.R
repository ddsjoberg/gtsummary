connect_to_host <- function(hostname, port, cancellable)
{
  addr <- gNetworkAddress(hostname, port)
  enumerator <- addr$enumerate()
  
  ## Try each sockaddr until we succeed. 

  conn <- NULL
  while (is.null(conn) && (!is.null(sockaddr <- enumerator$next(cancellable))))
    conn <- connect_to_sockaddr(sockaddr$retval)
  
  conn
}
