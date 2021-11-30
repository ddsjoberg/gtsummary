# Creating two radio buttons
create_radio_buttons <- function() {
  
  window <- gtkWindow("toplevel", show = F)
  box <- gtkVBoxNew(TRUE, 2)
  
  ## Create a radio button with a GtkEntry widget 
  radio1 <- gtkRadioButton()
  entry <- gtkEntry()
  radio1$add(entry)
   
  ## Create a radio button with a label
  radio2 <- gtkRadioButtonNewWithLabelFromWidget(radio1,
                                                 "I'm the second radio button.")
  
  ## Pack them into a box, then show all the widgets
  box$packStart(radio1, TRUE, TRUE, 2)
  box$packStart(radio2, TRUE, TRUE, 2)
  window$add(box)
  window$showAll()
}
