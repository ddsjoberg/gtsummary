socket <- gtkSocket()
parent$add(socket)

## The following call is only necessary if one of
## the ancestors of the socket is not yet visible.
socket$realize()
print(paste("The ID of the sockets window is", socket$getId()))
