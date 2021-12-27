# appWindow demo from GTK

window <- NULL

# define some callbacks

activate.radio.action <- function(action, current)
{
    name <- action$getName()
    typename <- class(action)[1]
    value <- current$getCurrentValue()
    if (current$getActive()) {
       text <- sprintf("You activated radio action: \"%s\" of type \"%s\".\nCurrent value: %d", name, typename, value)
       messagelabel$setText(text)
       infobar$setMessageType(value)
       infobar$show()
    }
}

activate.action <- function(action, w)
{
    name <- action$getName()
    typename <- class(action)[1] # parent, dialog mode, message type, buttons, message
    dialog <- gtkMessageDialogNew(window, "destroy-with-parent", "info", "close",
        "You activated action:", name, "of type", typename)
    gSignalConnect(dialog, "response", gtkWidgetDestroy)
}
activate.email <- function(about, link, data)
{
    print(paste("send mail to", link))
}
activate.url <- function(about, link, data)
{
    print(paste("show url", link))
}
about.cb <- function(action, window)
{
    filename <- imagefile("rgtk-logo.gif")
    if (file.exists(filename)) {
        pixbuf <- gdkPixbufNewFromFile(filename)[[1]]
        # make white space transparent
        transparent <- pixbuf$addAlpha(TRUE, 253, 253, 253)
    }

    gtkAboutDialogSetEmailHook(activate.email)
    gtkAboutDialogSetUrlHook(activate.url)

    gtkShowAboutDialog(window, program_name="RGtk Example", version="2.0",
                       copyright="(C) M. Lawrence and D. Temple Lang",
                       license="GPL",
                       website="http://www.omegahat.net/RGtk",
                       comments="An example of RGtk2",
                       authors=c("Michael <mflawren@fhcrc.org>", "Duncan"),
                       documenters="See authors", logo=transparent)
    
}
update.statusbar <- function(buffer, statusbar)
{
    statusbar$pop(0)
    count <- buffer$getCharCount()
    # get an "iter" describing the position of the cursor
    mark <- buffer$getInsert()
    iter <- buffer$getIterAtMark(mark)$iter
    row <- iter$getLine()
    col <- iter$getLineOffset()
    msg <- paste("Cursor at row", row, "column", col, "-", count, "chars in document")
    statusbar$push(0, msg)
}

mark.set.callback <- function(buffer, new.location, mark, data)
{
    update.statusbar(buffer, data)
}

update.resize.grip <- function(widget, event, statusbar)
{
  max_full_mask <- GdkWindowState["maximized"] | GdkWindowState["fullscreen"]
  if (event[["changedMask"]] & max_full_mask)
  {
      statusbar$setHasResizeGrip(!(event[["newWindowState"]] & max_full_mask))
  }
  return(FALSE)
}

registered <- FALSE
register.stock.icons <- function()
{
    if (registered)
        return

    item <- list(c("rgtk-logo", "_RGtk!", 0, 0, NULL))

    gtkStockAdd(item) # register as stock

    # now we also have to add it to an icon factory
    factory <- gtkIconFactoryNew() # make our own factory
    gtkIconFactoryAddDefault(factory) # make it a default factory

    filename <- imagefile("rgtk-logo.gif")
    if (file.exists(filename)) {
        pixbuf <- gdkPixbufNewFromFile(filename)[[1]]
        # make white space transparent
        transparent <- pixbuf$addAlpha(TRUE, 255, 255, 255)
        # make an icon from the image and add it to factory using stock id
        icon.set <- gtkIconSetNewFromPixbuf(transparent)
        factory$add("rgtk-logo", icon.set)
    } else warning("Could not load the RGtk logo")

    registered = TRUE
}

# create the actions

entries <- list(
    # name, stock.id (prefab icons), label (. signifies mneumonic)
    list("FileMenu", NULL, "_File"),
    list("PreferencesMenu", NULL, "_Preferences"),
    list("ColorMenu", NULL, "_Color"),
    list("ShapeMenu", NULL, "_Shape"),
    list("HelpMenu", NULL, "_Help"), # accelerator (kb shortcut), tooltip, callback
    list("New", "gtk-new", "_New", "<control>N", "Create a new file", activate.action),
    list("Open", "gtk-open", "_Open", "<control>O", "Open a file", activate.action),
    list("Save", "gtk-save", "_Save", "<control>S", "Save current file", activate.action),
    list("SaveAs", "gtk-save", "Save _As...", NULL, "Save to a file", activate.action),
    list("Quit", "gtk-quit", "_Quit", "<control>Q", "Quit", activate.action),
    list("About", NULL, "_About", "<control>A", "About", about.cb),
    list("Logo", "rgtk-logo", NULL, NULL, "RGtk", activate.action)
)

# create radio actions for choosing a color and a shape

color.entries <- list(
    list("Red", NULL, "_Red", "<control>R", "Blood", 0), # value
    list("Green", NULL, "_Green", "<control>G", "Grass", 1),
    list("Blue", NULL, "_Blue", "<control>B", "Sky", 2)
)
shape.entries <- list(
    list("Square", NULL, "_Square", "<control>S", "Square", 0),
    list("Rectangle", NULL, "_Rectangle", "<control>R", "Rectangle", 1),
    list("Oval", NULL, "_Oval", "<control>O", "Egg", 2)
)

# create some toggle (on/off) actions
toggle.entries <- list(c("Bold", "gtk-bold", "_Bold", "<control>B", "Bold", activate.action, TRUE)) # active?

# add the RGtk logo to themeable icons
register.stock.icons()

# create a window
window <- gtkWindowNew("toplevel")
window$setDefaultSize(200, 200)
window$setTitle("RGtk is in business")

# add a table layout
table <- gtkTableNew(1, 5, FALSE)
window$add(table)

agroup <- gtkActionGroupNew("AppWindowActions")

# add actions to group with the window widget passed to callbacks
agroup$addActions(entries, window)
agroup$addToggleActions(toggle.entries)
agroup$addRadioActions(color.entries, 0, activate.radio.action)
agroup$addRadioActions(shape.entries, 0, activate.radio.action)

# create a UI manager to read in menus specified in XML
manager <- gtkUIManagerNew()

window$setData("ui-manager", manager)
manager$insertActionGroup(agroup, 0)

window$addAccelGroup(manager$getAccelGroup())

# Define some XML
uistr <- paste(
"<ui>",
"  <menubar name='MenuBar'>",
"    <menu action='FileMenu'>",
"      <menuitem action='New'/>",
"      <menuitem action='Open'/>",
"      <menuitem action='Save'/>",
"      <menuitem action='SaveAs'/>",
"      <separator/>",
"      <menuitem action='Quit'/>",
"    </menu>",
"    <menu action='PreferencesMenu'>",
"      <menu action='ColorMenu'>",
"	<menuitem action='Red'/>",
"	<menuitem action='Green'/>",
"	<menuitem action='Blue'/>",
"      </menu>",
"      <menu action='ShapeMenu'>",
"        <menuitem action='Square'/>",
"        <menuitem action='Rectangle'/>",
"        <menuitem action='Oval'/>",
"      </menu>",
"      <menuitem action='Bold'/>",
"    </menu>",
"    <menu action='HelpMenu'>",
"      <menuitem action='About'/>",
"    </menu>",
"  </menubar>",
"  <toolbar  name='ToolBar'>",
"    <toolitem action='Open'/>",
"    <toolitem action='Quit'/>",
"    <separator action='Sep1'/>",
"    <toolitem action='Logo'/>",
"  </toolbar>",
"</ui>", sep="\n")

manager$addUiFromString(uistr)
menubar <- manager$getWidget("/MenuBar")
menubar$show() # location, layout behavior, padding
table$attach(menubar, 0, 1, 0, 1, yoptions = 0)

bar <- manager$getWidget("/ToolBar")
bar$show()
table$attach(bar, 0, 1, 1, 2, yoptions = 0)

infobar <- gtkInfoBar()
infobar$setNoShowAll(TRUE)
messagelabel <- gtkLabel("")
infobar$getContentArea()$packStart(messagelabel)
infobar$addButton("gtk-ok", GtkResponseType["ok"])
gSignalConnect(infobar, "response", gtkWidgetHide)
table$attach(infobar, 0, 1, 2, 3, yoptions = 0)

# now let the user put some text in the scrolling window

scrolled.window <- gtkScrolledWindow()
scrolled.window$setPolicy("automatic", "automatic")
scrolled.window$setShadowType("in")
table$attach(scrolled.window, 0, 1, 3, 4)

contents <- gtkTextViewNew()
contents$grabFocus()
scrolled.window$add(contents)

# how about a cool status bar?

statusbar <- gtkStatusbarNew() # squeeze it in at the bottom
table$attach(statusbar, 0, 1, 4, 5, yoptions = 0)

buffer <- contents$getBuffer() # statusbar listens to buffer
gSignalConnect(buffer, "changed", update.statusbar, statusbar)
gSignalConnect(buffer, "mark_set", mark.set.callback, statusbar)
# link resize grip to window state
gSignalConnect(window, "window_state_event", update.resize.grip, statusbar)

update.statusbar(buffer, statusbar)
