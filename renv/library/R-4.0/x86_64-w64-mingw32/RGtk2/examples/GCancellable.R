## Make sure we don't do any unnecessary work if already cancelled
if (cancellable$setErrorIfCancelled())
  return()
## Set up all the data needed to be able to
## handle cancellation of the operation
my_data <- myData(...)

id <- 0
if (!is.null(cancellable))
  id <- cancellable$connect(cancelled_handler, data, NULL)

## cancellable operation here...

cancellable$disconnect(id)
