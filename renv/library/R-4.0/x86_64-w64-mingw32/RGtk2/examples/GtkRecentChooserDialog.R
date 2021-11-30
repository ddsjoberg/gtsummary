dialog <- gtkRecentChooserDialog("Recent Documents", parent_window,
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-open", GtkResponseType["accept"])
					
if (dialog$run() == GtkResponseType["accept"])
{
  info <- dialog$getCurrentItem()
  open_file(info$getUri())
}
