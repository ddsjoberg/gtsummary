# Provides a function to retrieve a floating point value from a
# GtkSpinButton, and creates a high precision spin button.

grab_value <- function(a_spinner, user_data) {
   return(a_spinner$getValue())
}

create_floating_spin_button <- function() {

  spinner_adj <- gtkAdjustment(2.500, 0.0, 5.0, 0.001, 0.1, 0.1)
  
  window <- gtkWindow("toplevel", show = F)
  window$setBorderWidth(5)
  
  ## creates the spinner, with three decimal places
  spinner <- gtkSpinner(spinner_adj, 0.001, 3)
  window$add(spinner)
  
  window$showAll()
}
