drag_data_received <- function(widget, drag_context, x, y, data, info, time)
{
  if (data$getLength() > 0L)
    {
      if (drag_context$getAction() == "ask")
        {
          dialog <- gtkMessageDialog(NULL,
                                     c("modal", "destroy-with-parent"),
                                     "info", "yes-no", "Move the data ?\n")
          response <- dialog$run()
          dialog$destroy()

### FIXME: setAction() not yet supported
          if (response == GtkResponseType["yes"])
            drag_context$setAction("move")
          else
            drag_context$setAction("copy")
        }

      gtkDragFinish(drag_context, TRUE, FALSE, time)
    }

  gtkDragFinish (drag_context, FALSE, FALSE, time)
}
