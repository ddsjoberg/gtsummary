apply_changes_gradually <- function(data) {
  fraction <- progress_bar$getFraction() + 0.05
  if (fraction < 1.0) {
    progress_bar$setFraction(fraction)
    TRUE
  } else {
    assistant$destroy()
    FALSE
  }
}


on_assistant_apply <- function(widget, data)
{
  gTimeoutAdd(100, apply_changes_gradually)
}

on_assistant_prepare <- function(widget, page, data)
{
  current_page <- widget$getCurrentPage()
  n_pages <- widget$getNPages()

  title <- sprintf("Sample assistant (%d of %d)", current_page + 1, n_pages)
  widget$setTitle(title)

  if (current_page == 3)
    widget$commit()
}

on_entry_changed <- function(widget, assistant)
{
  page_number <- assistant$getCurrentPage()
  current_page <- assistant$getNthPage(page_number)
  text <- widget$getText()

  if (!is.null(text) && nchar(text) > 0)
    assistant$setPageComplete(current_page, TRUE)
  else
    assistant$setPageComplete(current_page, FALSE)
}


create_page1 <- function(assistant)
{
  box <- gtkHBox(FALSE, 12)
  box$setBorderWidth(12)

  label <- gtkLabel("You must fill out this entry to continue:")
  box$packStart(label, FALSE, FALSE, 0)

  entry <- gtkEntry()
  box$packStart(entry, TRUE, TRUE, 0)
  gSignalConnect(entry, "changed", on_entry_changed, assistant)

  assistant$appendPage(box)
  assistant$setPageTitle(box, "Page 1")
  assistant$setPageType(box, "intro")
  
  pixbuf <- assistant$renderIcon(GTK_STOCK_DIALOG_INFO, "dialog")
  assistant$setPageHeaderImage(box, pixbuf)
}

create_page2 <- function(assistant)
{
  box <- gtkVBox(FALSE, 12)
  box$setBorderWidth(12)

  checkbutton <- gtkCheckButton(paste("This is optional data, you may continue",
						 "even if you do not check this"))
  box$packStart(checkbutton, FALSE, FALSE, 0)

  assistant$appendPage(box)
  assistant$setPageComplete(box, TRUE)
  assistant$setPageTitle(box, "Page 2")

  pixbuf <- assistant$renderIcon(GTK_STOCK_DIALOG_INFO, "dialog")
  assistant$setPageHeaderImage(box, pixbuf)
}

create_page3 <- function(assistant)
{
  label <- gtkLabel("This is a confirmation page, press 'Apply' to apply changes")

  assistant$appendPage(label)
  assistant$setPageType(label, "confirm")
  assistant$setPageComplete(label, TRUE)
  assistant$setPageTitle(label, "Confirmation")

  pixbuf <- assistant$renderIcon(GTK_STOCK_DIALOG_INFO, "dialog")
  assistant$setPageHeaderImage(label, pixbuf)
}

create_page4 <- function(assistant) {
  page <- gtkAlignment(0.5, 0.5, 0.5, 0.0)

  progress_bar <- gtkProgressBar()
  page$add(progress_bar)
  page$showAll()

  assistant$appendPage(page)

  assistant$setPageType(page, "progress")
  assistant$setPageTitle(page, "Applying changes")

  ## This prevents the assistant window from being
  ## closed while we're "busy" applying changes.
  page$setPageComplete(FALSE)
}

assistant <- gtkAssistant(show = F)

assistant$setDefaultSize(-1, 300)

create_page1(assistant)
create_page2(assistant)
create_page3(assistant)
create_page4(assistant)

gSignalConnect(assistant, "cancel", gtkWidgetDestroy)
gSignalConnect(assistant, "close", gtkWidgetDestroy)
gSignalConnect(assistant, "apply", on_assistant_apply)
gSignalConnect(assistant, "prepare", on_assistant_prepare)

assistant$showAll()
