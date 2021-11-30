library(RGtk2)
countend <- 0

scroll_to_end <- function(textview)
{

  buffer <- textview$getBuffer()
  
  # Get "end" mark. It's located at the end of buffer because 
  # of right gravity
  mark <- buffer$getMark("end")
  iter <- buffer$getIterAtMark(mark)$iter

  # and insert some text at its position, the iter will be 
  # revalidated after insertion to point to the end of inserted text

  countend <<- countend + 1
  spaces <- paste(rep(" ", countend), collapse = "")
  buffer$insert(iter, "\n")
  buffer$insert(iter, spaces)
  buffer$insert(iter, "Scroll to end scroll to end scroll to end scroll to end")
  

  # Now scroll the end mark onscreen.

  textview$scrollMarkOnscreen(mark)

  # Emulate typewriter behavior, shift to the left if we 
  # are far enough to the right.
  
  if (countend > 150)
    countend <<- 0

  return(TRUE)
}

countbot <- 0

# Scroll to the bottom of the buffer.
scroll_to_bottom <- function(textview)
{
  buffer <- textview$getBuffer()
  
  ## Get end iterator
  iter <- buffer$getEndIter()$iter

  # and insert some text at it, the iter will be revalidated
  # after insertion to point to the end of inserted text

  countbot <<- countbot + 1
  spaces <- paste(rep(" ", countbot), collapse = "")
  
  buffer$insert(iter, "\n")
  buffer$insert(iter, spaces)
  buffer$insert(iter, "Scroll bottom scroll bottom scroll bottom scroll bottom")

  # Move the iterator to the beginning of line, so we don't scroll 
  # in horizontal direction
  iter$setLineOffset(0)
  
  # and place the mark at iter. the mark will stay there after we
  # insert some text at the end because it has right gravity.
  mark <- buffer$getMark("scroll")
  buffer$moveMark(mark, iter)
  
  # Scroll the mark onscreen.
  textview$scrollMarkOnscreen(mark)
  
  # Shift text back if we got enough to the right.
  if (countbot > 40)
    countbot <<- 0

  return(TRUE)
}

setup_scroll <- function(textview, to_end)
{
  buffer <- textview$getBuffer()
  iter <- buffer$getEndIter()$iter

  if (to_end)
  {
    ## If we want to scroll to the end, including horizontal scrolling,
    ## then we just create a mark with right gravity at the end of the 
    ## buffer. It will stay at the end unless explicitely moved with 
    ## gtk_text_buffer_move_mark.

    buffer$createMark("end", iter, FALSE)
    
    ## Add scrolling timeout.
    return(gTimeoutAdd(50, scroll_to_end, textview))
  }
  else
  {
    ## If we want to scroll to the bottom, but not scroll horizontally, 
    ## then an end mark won't do the job. Just create a mark so we can 
    ## use it with gtk_text_view_scroll_mark_onscreen, we'll position it
    ## explicitely when needed. Use left gravity so the mark stays where 
    ## we put it after inserting new text.
    
    buffer$createMark("scroll", iter, TRUE)
    
    ## Add scrolling timeout.
    return(gTimeoutAdd (100, scroll_to_bottom, textview))
  }
}

remove_timeout <- function(window, timeout)
{
  gSourceRemove(timeout)
}

create_text_view <- function(hbox, to_end)
{
  swindow <- gtkScrolledWindow()
  hbox$packStart(swindow)
  textview <- gtkTextView()
  swindow$add(textview)

  timeout <- setup_scroll (textview, to_end)

 ## Remove the timeout in destroy handler, so we don't try to
 ## scroll destroyed widget. 
  gSignalConnect(textview, "destroy", remove_timeout, timeout)
}


window <- gtkWindow(show = FALSE)
window$setDefaultSize(600, 400)
hbox <- gtkHBox(TRUE, 6)
window$add(hbox)
create_text_view(hbox, TRUE)
create_text_view(hbox, FALSE)

window$showAll()
