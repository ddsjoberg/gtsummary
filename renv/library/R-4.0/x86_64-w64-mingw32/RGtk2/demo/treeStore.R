# source("/home/larman/research/RGtk/inst/demo/treeStore.R")

# columns
COLUMN <- c(holiday.name = 0, alex = 1, havoc = 2, tim = 3, owen = 4, dave = 5, visible = 6, world = 7)

nameTreeItem <- function(item) {
	names(item) <- c("label", "alex", "havoc", "tim", "owen", "dave", "world.holiday", "children")
	item
}

# tree data
january <- lapply(list(
  list("New Years Day", TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NULL),
  list("Presidential Inauguration", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL),
  list("Martin Luther King Jr. day", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL)
), nameTreeItem)

february <- lapply(list(
  list("Presidents' Day", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL),
  list("Groundhog Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Valentine's Day", FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, NULL)
), nameTreeItem)

march <- lapply(list(
  list("National Tree Planting Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("St Patrick's Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL)
), nameTreeItem)

april <- lapply(list(
  list("April Fools' Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL),
  list("Army Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Earth Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL),
  list("Administrative Professionals' Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL)
), nameTreeItem)

may <- lapply(list(
  list("Nurses' Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("National Day of Prayer", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Mothers' Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL ),
  list("Armed Forces Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Memorial Day", TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NULL)
), nameTreeItem)

june <- lapply(list(
  list("June Fathers' Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL),
  list("Juneteenth (Liberation of Slaves)", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Flag Day", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL)
), nameTreeItem)

july <- lapply(list(
  list("Parents' Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL),
  list("Independence Day", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL)
), nameTreeItem)

august <- lapply(list(
  list("Air Force Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Coast Guard Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Friendship Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL)
), nameTreeItem)

september <- lapply(list(
  list("Grandparents' Day", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL),
  list("Citizenship Day or Constitution Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Labor Day", TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NULL)
), nameTreeItem)

october <- lapply(list(
  list("National Children's Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Bosses' Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Sweetest Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Mother-in-Law's Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Navy Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Columbus Day", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL),
  list("Halloween", FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NULL)
), nameTreeItem)

november <- lapply(list(
  list("Marine Corps Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Veterans' Day", TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NULL),
  list("Thanksgiving", FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NULL)
), nameTreeItem)

december <- lapply(list(
  list("Pearl Harbor Remembrance Day", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL),
  list("Christmas", TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NULL),
  list("Kwanzaa", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NULL)
), nameTreeItem)

toplevel <- lapply(list(
  list("January", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, january),
  list("February", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, february),
  list("March", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, march),
  list("April", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, april),
  list("May", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, may),
  list("June", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, june),
  list("July", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, july),
  list("August", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, august),
  list("September", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, september),
  list("October", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, october),
  list("November", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, november),
  list("December", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, december)
), nameTreeItem)

create.model <- function()
{
  # create tree store
  model <- gtkTreeStoreNew("gchararray", "gboolean", "gboolean", "gboolean", "gboolean",
			      "gboolean", "gboolean", "gboolean")

  month <- toplevel
  
  # add data to the tree store
  sapply(toplevel, function(month)
    {
      holidays <- month$children

      iter <- model$append(NULL)$iter
      model$set(iter,
	  		  COLUMN["holiday.name"], month$label,
			  COLUMN["alex"], FALSE,
			  COLUMN["havoc"], FALSE,
			  COLUMN["tim"], FALSE,
			  COLUMN["owen"], FALSE,
			  COLUMN["dave"], FALSE,
			  COLUMN["visible"], FALSE,
			  COLUMN["world"], FALSE)

      # add children
      sapply(holidays, function(holiday)
	{
	  child.iter <- model$append(iter)$iter
	  model$set(child.iter,
			      COLUMN["holiday.name"], holiday$label,
			      COLUMN["alex"], holiday$alex,
			      COLUMN["havoc"], holiday$havoc,
			      COLUMN["tim"], holiday$tim,
			      COLUMN["owen"], holiday$owen,
			      COLUMN["dave"], holiday$dave,
			      COLUMN["visible"], TRUE,
			      COLUMN["world"], holiday$world.holiday)
	})
    })

  return(model)
}

item.toggled <- function(cell, path.str, data)
{
  checkPtrType(data, "GtkTreeModel")
  model <- data
  
  path <- gtkTreePathNewFromString(path.str)
  
  column <- cell$getData("column")

  # get toggled iter
  iter <- model$getIter(path)$iter
  toggle.item <- model$get(iter, column)[[1]]

  # do something with the value
  toggle.item <- !toggle.item

  # set new value
  model$set(iter, column, toggle.item)
}

add.columns <- function(treeview)
{
  model <- gtkTreeViewGetModel(treeview)

  # column for holiday names
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)

  col.offset <- treeview$insertColumnWithAttributes(-1, "Holiday", renderer, 
  								text = COLUMN[["holiday.name"]])
								
  column <- treeview$getColumn(col.offset - 1)
  column$setClickable(TRUE)

  # alex column
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$setData("column", COLUMN["alex"])

  gSignalConnect(renderer, "toggled", item.toggled, model)

  col.offset <- treeview$insertColumnWithAttributes(-1, "Alex", renderer,
							    active = COLUMN[["alex"]],
							    visible = COLUMN[["visible"]],
							    activatable = COLUMN[["world"]])

  column <- treeview$getColumn(col.offset - 1)
  column$setSizing("fixed")
  column$setFixedWidth(50)
  column$setClickable(TRUE)

  # havoc column
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$setData("column", COLUMN["havoc"])

  gSignalConnect(renderer, "toggled", item.toggled, model)

  col.offset <- treeview$insertColumnWithAttributes(-1, "Havoc", renderer,
							    active = COLUMN[["havoc"]],
							    visible = COLUMN[["visible"]],
							    activatable = COLUMN[["world"]])

  column <- treeview$getColumn(col.offset - 1)
  column$setSizing("fixed")
  column$setFixedWidth(50)
  column$setClickable(TRUE)

  # tim column
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$setData("column", COLUMN["tim"])

  gSignalConnect(renderer, "toggled", item.toggled, model)

  col.offset <- treeview$insertColumnWithAttributes(-1, "Tim", renderer,
							    active = COLUMN[["tim"]],
							    visible = COLUMN[["visible"]],
							    activatable = COLUMN[["world"]])

  column <- treeview$getColumn(col.offset - 1)
  column$setSizing("fixed")
  column$setFixedWidth(50)
  column$setClickable(TRUE)

  # owen column
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$setData("column", COLUMN["owen"])

  gSignalConnect(renderer, "toggled", item.toggled, model)

  col.offset <- treeview$insertColumnWithAttributes(-1, "Owen", renderer,
							    active = COLUMN[["owen"]],
							    visible = COLUMN[["visible"]],
							    activatable = COLUMN[["world"]])

  column <- treeview$getColumn(col.offset - 1)
  column$setSizing("fixed")
  column$setFixedWidth(50)
  column$setClickable(TRUE)

  # dave column
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$setData("column", COLUMN["dave"])

  gSignalConnect(renderer, "toggled", item.toggled, model)

  col.offset <- treeview$insertColumnWithAttributes(-1, "Dave", renderer,
							    active = COLUMN[["dave"]],
							    visible = COLUMN[["visible"]],
							    activatable = COLUMN[["world"]])

  column <- treeview$getColumn(col.offset - 1)
  column$setSizing("fixed")
  column$setFixedWidth(50)
  column$setClickable(TRUE)
}

# create window, etc
window <- gtkWindowNew("toplevel", show = F)
window$setTitle("Card planning sheet")

vbox <- gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(8)
window$add(vbox)

vbox$packStart(gtkLabelNew("Jonathan's Holiday Card Planning Sheet"),
	  FALSE, FALSE, 0)

sw <- gtkScrolledWindowNew(NULL, NULL)
sw$setShadowType("etched-in")
sw$setPolicy("automatic", "automatic")
vbox$packStart(sw, TRUE, TRUE, 0)

# create model
model <- create.model()

# create tree view
treeview <- gtkTreeViewNewWithModel(model)
treeview$setRulesHint(TRUE)
treeview$getSelection()$setMode("multiple")

add.columns(treeview)

sw$add(treeview)

# expand all rows after the treeview widget has been realized
gSignalConnect(treeview, "realize", gtkTreeViewExpandAll)
window$setDefaultSize(650, 400)

window$showAll()
