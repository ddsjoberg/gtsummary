drag_motion <- function(widget, context, x, y, time)
{
  state <- widget$getData("drag-state")
  
  if (!state$drag_highlight) {
    state$drag_highlight <- T
    gtkDragHighlight(widget)
  }
  
  target <- gtkDragDestFindTarget(widget, context, NULL)
  if (target == 0)
    gdkDragStatus(context, 0, time)
  else {
    state$pending_status <- context[["suggestedAction"]]
    gtkDragGetData(widget, context, target, time)
  }
  
  widget$setData("drag-state", state)
  
  return(TRUE)
}

drag_data_received <- function(widget, context, x, y, selection_data, info,
                               time)
{
  state <- widget$getData("drag-state")
  
  if (state$pending_status) { 
    ## We are getting this data due to a request in drag_motion,
    ## rather than due to a request in drag_drop, so we are just
    ## supposed to call gdk_drag_status(), not actually paste in the data.

    str <- gtkSelectionDataGetText(selection_data)
    if (!data_is_acceptable (str)) 
      gdkDragStatus(context, 0, time)
    else
      gdkDragStatus(context, state$pending_status, time)
    
    state$pending_status <- 0
  }
  else {
    ## accept the drop
  }
  
  widget$setData("drag-state", state)
}
