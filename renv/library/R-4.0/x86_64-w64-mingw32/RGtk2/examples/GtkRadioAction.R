while (more_actions) {
  action <- gtkRadioAction(...)
  
  action$setGroup(group)
  group <- action$getGroup()
}
