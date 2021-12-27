text_editing_started <- function(cell, editable, path, data)
{
  checkPtrType(editable, "GtkEntry")
  ## ... create a GtkEntryCompletion
  editable$setCompletion(completion)
}

