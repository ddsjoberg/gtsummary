on_drop_zone_drag_data_received <- function(widget, context, x, y,
                                            selection_data, info, time,
                                            user_data)
{
  notebook <- context$getWidget()
  child <- selection_data$data
  # unfortunately, it's not possible to actually use 'child' - there
  # would need to be a way to derefernce it and make an externalptr
  # if you need this functionality, please let the RGtk2 maintainer know.
  # process_widget(child)
  # notebook$remove(child)
}
