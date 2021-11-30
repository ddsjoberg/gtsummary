renderer <- gtkCellRendererText()
column <- gtkTreeViewColumn("Title", renderer, "text" = TEXT_COLUMN,
                            "foreground" = COLOR_COLUMN)
