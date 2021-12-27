## Provides a function to retrieve an integer value from a GtkSpinButton
## and creates a spin button to model percentage values.

grab_int_value <- function(a_spinner, user_data) {
   return(a_spinner$getValueAsInt())
}

create_integer_spin_button <- function() {

  spinner_adj <- gtkAdjustment(50.0, 0.0, 100.0, 1.0, 5.0, 5.0)
  
  window <- gtkWindow("toplevel", show = F)
  window$setBorderWidth(5)
   
  ## creates the spinner, with no decimal places
  spinner <- gtkSpinner(spinner_adj, 1.0, 0)
  window$add(spinner)
  
  window$showAll()
}
