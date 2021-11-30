context <- gdkAppLaunchContext()
context$setScreen(my_screen)
context$setTimestamp(event$time)
gAppInfoLaunchDefaultForUri("http://www.gtk.org", context)
