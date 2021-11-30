## icon and progress in a GtkEntry

search_progress_id <- 0
finish_search_id <- 0

show_find_button <- function()
{
  notebook$setCurrentPage(0)
}

show_cancel_button <- function()
{
  notebook$setCurrentPage(1)
}

search_progress <- function(data)
{
  data$progressPulse()
  return(TRUE)
}

search_progress_done <- function(entry)
{
  entry$setProgressFraction(0.0)
}

finish_search <- function(entry)
{
  show_find_button()
  
  gSourceRemove(search_progress_id)
  search_progress_id <<- 0
  search_progress_done(entry)
  
  return(FALSE)
}

start_search_feedback <- function(data)
{
  search_progress_id <<- gTimeoutAdd(100, search_progress, data)
  return(FALSE)
}

start_search <- function(button, entry)
{
  show_cancel_button()
  search_progress_id <<- gTimeoutAdd(1000, start_search_feedback, entry)
  finish_search_id <<- gTimeoutAdd(15000, finish_search, entry)
}

stop_search <- function(entry, data) {
  gSourceRemove(finish_search_id)
  finish_search(entry)
}

clear_entry <- function(entry)
{
  entry$setText("")
}

search_by_name <-function(item, entry)
{
  entry$setIconFromStock(GtkEntryIconPosition["primary"], GTK_STOCK_FIND)
  entry$setIconTooltipText(GtkEntryIconPosition["primary"],
                           "Search by name\nClick here to change the search type")
}

search_by_description <- function(item, entry)
{
  entry$setIconFromStock(GtkEntryIconPosition["primary"], GTK_STOCK_EDIT)
  entry$setIconTooltipText(GtkEntryIconPosition["primary"],
                           "Search by description\nClick here to change the search type")
}

search_by_file <- function(item, entry)
{

  entry$setIconFromStock(GtkEntryIconPosition["primary"], GTK_STOCK_OPEN)
  entry$setIconTooltipText(GtkEntryIconPosition["primary"],
                           "Search by file name\nClick here to change the search type")
}

create_search_menu <- function(entry)
{
  menu <- gtkMenu()

  item <- gtkImageMenuItemNewWithMnemonic("Search by _name")
  image <- gtkImage(stock = GTK_STOCK_FIND, size = GtkIconSize["menu"])
  item$setImage(image)
  item$setAlwaysShowImage(TRUE)
  gSignalConnect(item, "activate", search_by_name, entry)
  menu$append(item)

  item <- gtkImageMenuItemNewWithMnemonic("Search by _description")
  image <- gtkImage(stock = GTK_STOCK_EDIT, size = GtkIconSize["menu"])
  item$setImage(image)
  item$setAlwaysShowImage(TRUE)
  gSignalConnect(item, "activate", search_by_description, entry)
  menu$append(item)

  item <- gtkImageMenuItemNewWithMnemonic("Search by _file name")
  image <- gtkImage(stock = GTK_STOCK_OPEN, size = GtkIconSize["menu"])
  item$setImage(image)
  item$setAlwaysShowImage(TRUE)
  gSignalConnect(item, "activate", search_by_file, entry)
  menu$append(item)

  menu$showAll()
  return(menu)
}

icon_press_cb <- function(entry, position, event, data)
{
  if (position == GtkEntryIconPosition["primary"])
    menu$popup(NULL, NULL, NULL, NULL, event[["button"]], event[["time"]])
  else clear_entry(entry)
}

text_changed_cb <- function(entry, pspec, button)
{
  has_text <- entry$getTextLength() > 0
  entry$setIconSensitive(GtkEntryIconPosition["secondary"], has_text)
  button["sensitive"] <- has_text
}

activate_cb <- function(entry, button)
{
  if (search_progress_id != 0)
    return()
  start_search(button, entry)
}

search_entry_destroyed <- function(widget)
{
  if (finish_search_id != 0)
    gSourceRemove(finish_search_id)

  if (search_progress_id != 0)
    gSourceRemove(search_progress_id)

  window <- NULL
}

entry_populate_popup <- function(entry, menu, user_data)
{
  has_text <- entry$getTextLength() > 0

  item <- gtkSeparatorMenuItem()
  menu$append(item)

  item <- gtkMenuItemNewWithMnemonic("C_lear")
  gSignalConnect(item, "activate", clear_entry, entry, user.data.first=TRUE)
  menu$append(item)
  item["sensitive"] <- has_text

  search_menu <- create_search_menu(entry)
  item <- gtkMenuItem("Search by")
  item$setSubmenu(search_menu)
  menu$append(item)
}

window <- gtkDialog("Search Entry", NULL, 0, GTK_STOCK_CLOSE,
                    GtkResponseType["none"])
window["resizable"] <- FALSE

gSignalConnect(window, "response", gtkWidgetDestroy)

content_area <- window$getContentArea()

vbox <- gtkVBox(FALSE, 5)
content_area$packStart(vbox)
vbox$setBorderWidth(5)

label <- gtkLabel()
label$setMarkup("Search entry demo")
vbox$packStart(label, FALSE, FALSE)

hbox <- gtkHBox(FALSE, 10)
vbox$packStart(hbox)
vbox$setBorderWidth(0)

## Create our entry
entry <- gtkEntry()
hbox$packStart(entry, FALSE, FALSE)

## Create the find and cancel buttons
notebook <- gtkNotebook()
notebook$setShowTabs(FALSE)
notebook$setShowBorder(FALSE)
hbox$packStart(notebook, FALSE, FALSE)

find_button <- gtkButton("Find")
gSignalConnect(find_button, "clicked", start_search, entry)
notebook$appendPage(find_button)

cancel_button <- gtkButton("Cancel")
gSignalConnect(cancel_button, "clicked", stop_search, entry,
               user.data.first=TRUE)
notebook$appendPage(cancel_button)

## Set up the search icon
search_by_name(NULL, entry)

## Set up the clear icon
entry$setIconFromStock(GtkEntryIconPosition["secondary"], GTK_STOCK_CLEAR)
text_changed_cb (entry, NULL, find_button)

gSignalConnect(entry, "icon-press", icon_press_cb)
gSignalConnect(entry, "notify::text", text_changed_cb, find_button)
gSignalConnect(entry, "activate", activate_cb)

## Create the menu 
menu <- create_search_menu(entry)
menu$attachToWidget(entry)

## add accessible alternatives for icon functionality
gSignalConnect(entry, "populate-popup", entry_populate_popup)
