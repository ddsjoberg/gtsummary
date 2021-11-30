expander <- gtkExpanderNewWithMnemonic("_More Options")
gSignalConnect(expander, "notify::expanded", expander_callback)

...


expander_callback <- (expander, param_spec, user_data)
{
  if (expander$getExpanded()) {
    # Show or create widgets
  } else {
    # Hide or destroy widgets
  }
}
