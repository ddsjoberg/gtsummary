# Let's make two toggle buttons
make_toggles <- function() {
  dialog <- gtkDialog(show = F)
  toggle1 <- gtkToggleButton("Hi, i'm a toggle button.")
  
  ## Makes this toggle button invisible
  toggle1$setMode(TRUE)
   
  gSignalConnect(toggle1, "toggled", output_state)
  dialog[["actionArea"]]$packStart(toggle1, FALSE, FALSE, 2)

  toggle2 <- gtkToggleButton("Hi, i'm another button.")
  toggle2$setMode(FALSE)
  gSignalConnect(toggle2, "toggled", output_state)
  dialog[["actionArea"]]$packStart(toggle2, FALSE, FALSE, 2)

  dialog$showAll()
}
