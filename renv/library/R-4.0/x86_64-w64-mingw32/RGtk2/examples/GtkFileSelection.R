# Getting a filename from a user
# Note how much easier GtkFileChooser is to use

store_filename <- function(widget, file_selector) {
   selected_filename <- file_selector$getFilename();
   print(paste("Selected filename:", selected_filename))
}

create_file_selection <- function() {

  ## Create the selector
   
  file_selector <- gtkFileSelection("Please select a file for editing.",
                                    show = FALSE)
   
  gSignalConnect(file_selector[["ok_button"]], "clicked", store_filename,
                 file_selector)
   			   
  ## Ensure that the dialog box is destroyed when the user clicks a button.
   
  gSignalConnect(file_selector[["ok_button"]], "clicked", gtkWidgetDestroy, 
                 file_selector, user.data.first = TRUE)
  
  gSignalConnect(file_selector[["cancel_button"]], "clicked", gtkWidgetDestroy,
                 file_selector, user.data.first = TRUE) 
   
  ## Display that dialog
   
  file_selector$show()
}
