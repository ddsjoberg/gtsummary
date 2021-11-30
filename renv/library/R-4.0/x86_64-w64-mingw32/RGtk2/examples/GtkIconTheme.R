icon_theme <- gtkIconThemeGetDefault()
result <- icon_theme$loadIcon("my-icon-name", 48, 0)
if (!result[[1]]) {
  warning("Couldn't load icon: ", result$error$message)
} else {
  pixbuf <- result[[1]]
  ## Use the pixbuf
}
