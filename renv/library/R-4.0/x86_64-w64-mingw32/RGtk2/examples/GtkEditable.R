insert_text_handler <- function(editable, text, length, position, id)
{
  result <- toupper(text)
  gSignalHandlerBlock(editable, id)
  editable$insertText(result, length, position)
  gSignalHandlerUnblock(editable, id)
}
