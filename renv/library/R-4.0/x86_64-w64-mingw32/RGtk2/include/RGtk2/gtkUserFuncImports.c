void
S_GtkAboutDialogActivateLinkFunc(GtkAboutDialog* about, const gchar* link, gpointer data)
{
  static void (*fun)(GtkAboutDialog*, const gchar*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkAboutDialog*, const gchar*, gpointer))R_GetCCallable("RGtk2", "S_GtkAboutDialogActivateLinkFunc"));
  return(fun(about, link, data));
} 

void
S_GtkCellLayoutDataFunc(GtkCellLayout* cell_layout, GtkCellRenderer* cell, GtkTreeModel* tree_model, GtkTreeIter* iter, gpointer data)
{
  static void (*fun)(GtkCellLayout*, GtkCellRenderer*, GtkTreeModel*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkCellLayout*, GtkCellRenderer*, GtkTreeModel*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkCellLayoutDataFunc"));
  return(fun(cell_layout, cell, tree_model, iter, data));
} 

void
S_GtkClipboardGetFunc(GtkClipboard* clipboard, GtkSelectionData* selection_data, guint info, gpointer user_data_or_owner)
{
  static void (*fun)(GtkClipboard*, GtkSelectionData*, guint, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, GtkSelectionData*, guint, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardGetFunc"));
  return(fun(clipboard, selection_data, info, user_data_or_owner));
} 

void
S_GtkClipboardReceivedFunc(GtkClipboard* clipboard, GtkSelectionData* selection_data, gpointer data)
{
  static void (*fun)(GtkClipboard*, GtkSelectionData*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, GtkSelectionData*, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardReceivedFunc"));
  return(fun(clipboard, selection_data, data));
} 

void
S_GtkClipboardImageReceivedFunc(GtkClipboard* clipboard, GdkPixbuf* image, gpointer data)
{
  static void (*fun)(GtkClipboard*, GdkPixbuf*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, GdkPixbuf*, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardImageReceivedFunc"));
  return(fun(clipboard, image, data));
} 

void
S_GtkClipboardTextReceivedFunc(GtkClipboard* clipboard, const gchar* text, gpointer data)
{
  static void (*fun)(GtkClipboard*, const gchar*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, const gchar*, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardTextReceivedFunc"));
  return(fun(clipboard, text, data));
} 

void
S_GtkClipboardTargetsReceivedFunc(GtkClipboard* clipboard, GdkAtom* atoms, gint n_atoms, gpointer data)
{
  static void (*fun)(GtkClipboard*, GdkAtom[], gint, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, GdkAtom[], gint, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardTargetsReceivedFunc"));
  return(fun(clipboard, atoms, n_atoms, data));
} 

void
S_GtkColorSelectionChangePaletteFunc(const GdkColor* colors, gint n_colors)
{
  static void (*fun)(const GdkColor[], gint) = NULL;
  if(!fun) fun = ((void (*)(const GdkColor[], gint))R_GetCCallable("RGtk2", "S_GtkColorSelectionChangePaletteFunc"));
  return(fun(colors, n_colors));
} 

void
S_GtkColorSelectionChangePaletteWithScreenFunc(GdkScreen* screen, const GdkColor* colors, gint n_colors)
{
  static void (*fun)(GdkScreen*, const GdkColor[], gint) = NULL;
  if(!fun) fun = ((void (*)(GdkScreen*, const GdkColor[], gint))R_GetCCallable("RGtk2", "S_GtkColorSelectionChangePaletteWithScreenFunc"));
  return(fun(screen, colors, n_colors));
} 

gboolean
S_GtkCTreeGNodeFunc(GtkCTree* ctree, guint depth, GNode* gnode, GtkCTreeNode* cnode, gpointer data)
{
  static gboolean (*fun)(GtkCTree*, guint, GNode*, GtkCTreeNode*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkCTree*, guint, GNode*, GtkCTreeNode*, gpointer))R_GetCCallable("RGtk2", "S_GtkCTreeGNodeFunc"));
  return(fun(ctree, depth, gnode, cnode, data));
} 

void
S_GtkCTreeFunc(GtkCTree* ctree, GtkCTreeNode* node, gpointer data)
{
  static void (*fun)(GtkCTree*, GtkCTreeNode*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkCTree*, GtkCTreeNode*, gpointer))R_GetCCallable("RGtk2", "S_GtkCTreeFunc"));
  return(fun(ctree, node, data));
} 

gboolean
S_GtkEntryCompletionMatchFunc(GtkEntryCompletion* completion, const gchar* key, GtkTreeIter* iter, gpointer user_data)
{
  static gboolean (*fun)(GtkEntryCompletion*, const gchar*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkEntryCompletion*, const gchar*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkEntryCompletionMatchFunc"));
  return(fun(completion, key, iter, user_data));
} 

gboolean
S_GtkFileFilterFunc(const GtkFileFilterInfo* filter_info, gpointer data)
{
  static gboolean (*fun)(const GtkFileFilterInfo*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(const GtkFileFilterInfo*, gpointer))R_GetCCallable("RGtk2", "S_GtkFileFilterFunc"));
  return(fun(filter_info, data));
} 

void
S_GtkIconViewForeachFunc(GtkIconView* icon_view, GtkTreePath* path, gpointer data)
{
  static void (*fun)(GtkIconView*, GtkTreePath*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkIconView*, GtkTreePath*, gpointer))R_GetCCallable("RGtk2", "S_GtkIconViewForeachFunc"));
  return(fun(icon_view, path, data));
} 

void
S_GtkTranslateFunc(const gchar* path, gpointer func_data)
{
  static void (*fun)(const gchar*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(const gchar*, gpointer))R_GetCCallable("RGtk2", "S_GtkTranslateFunc"));
  return(fun(path, func_data));
} 

gboolean
S_GtkFunction(gpointer data)
{
  static gboolean (*fun)(gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(gpointer))R_GetCCallable("RGtk2", "S_GtkFunction"));
  return(fun(data));
} 

gint
S_GtkKeySnoopFunc(GtkWidget* grab_widget, GdkEventKey* event, gpointer func_data)
{
  static gint (*fun)(GtkWidget*, GdkEventKey*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkWidget*, GdkEventKey*, gpointer))R_GetCCallable("RGtk2", "S_GtkKeySnoopFunc"));
  return(fun(grab_widget, event, func_data));
} 

gint
S_GtkMenuPositionFunc(GtkMenu* menu, gint* x, gint* y, gboolean* push_in, gpointer user_data)
{
  static gint (*fun)(GtkMenu*, gint*, gint*, gboolean*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkMenu*, gint*, gint*, gboolean*, gpointer))R_GetCCallable("RGtk2", "S_GtkMenuPositionFunc"));
  return(fun(menu, x, y, push_in, user_data));
} 

gint
S_GtkTreeModelForeachFunc(GtkTreeModel* model, GtkTreePath* path, GtkTreeIter* iter, gpointer data)
{
  static gint (*fun)(GtkTreeModel*, GtkTreePath*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkTreeModel*, GtkTreePath*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeModelForeachFunc"));
  return(fun(model, path, iter, data));
} 

gint
S_GtkTreeModelFilterVisibleFunc(GtkTreeModel* model, GtkTreeIter* iter, gpointer data)
{
  static gint (*fun)(GtkTreeModel*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkTreeModel*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeModelFilterVisibleFunc"));
  return(fun(model, iter, data));
} 

gint
S_GtkTreeModelFilterModifyFunc(GtkTreeModel* model, GtkTreeIter* iter, GValue* value, gint column, gpointer data)
{
  static gint (*fun)(GtkTreeModel*, GtkTreeIter*, GValue*, gint, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkTreeModel*, GtkTreeIter*, GValue*, gint, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeModelFilterModifyFunc"));
  return(fun(model, iter, value, column, data));
} 

gboolean
S_GtkTreeSelectionFunc(GtkTreeSelection* selection, GtkTreeModel* model, GtkTreePath* path, gboolean path_currently_selected, gpointer data)
{
  static gboolean (*fun)(GtkTreeSelection*, GtkTreeModel*, GtkTreePath*, gboolean, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkTreeSelection*, GtkTreeModel*, GtkTreePath*, gboolean, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeSelectionFunc"));
  return(fun(selection, model, path, path_currently_selected, data));
} 

void
S_GtkTreeSelectionForeachFunc(GtkTreeModel* model, GtkTreePath* path, GtkTreeIter* iter, gpointer data)
{
  static void (*fun)(GtkTreeModel*, GtkTreePath*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkTreeModel*, GtkTreePath*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeSelectionForeachFunc"));
  return(fun(model, path, iter, data));
} 

gint
S_GtkTreeIterCompareFunc(GtkTreeModel* model, GtkTreeIter* a, GtkTreeIter* b, gpointer user_data)
{
  static gint (*fun)(GtkTreeModel*, GtkTreeIter*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkTreeModel*, GtkTreeIter*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeIterCompareFunc"));
  return(fun(model, a, b, user_data));
} 

void
S_GtkTreeCellDataFunc(GtkTreeViewColumn* tree_column, GtkCellRenderer* cell, GtkTreeModel* tree_model, GtkTreeIter* iter, gpointer data)
{
  static void (*fun)(GtkTreeViewColumn*, GtkCellRenderer*, GtkTreeModel*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkTreeViewColumn*, GtkCellRenderer*, GtkTreeModel*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeCellDataFunc"));
  return(fun(tree_column, cell, tree_model, iter, data));
} 

gboolean
S_GtkTreeViewColumnDropFunc(GtkTreeView* tree_view, GtkTreeViewColumn* column, GtkTreeViewColumn* prev_column, GtkTreeViewColumn* next_column, gpointer data)
{
  static gboolean (*fun)(GtkTreeView*, GtkTreeViewColumn*, GtkTreeViewColumn*, GtkTreeViewColumn*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkTreeView*, GtkTreeViewColumn*, GtkTreeViewColumn*, GtkTreeViewColumn*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeViewColumnDropFunc"));
  return(fun(tree_view, column, prev_column, next_column, data));
} 

void
S_GtkTreeViewMappingFunc(GtkTreeView* tree_view, GtkTreePath* path, gpointer user_data)
{
  static void (*fun)(GtkTreeView*, GtkTreePath*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkTreeView*, GtkTreePath*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeViewMappingFunc"));
  return(fun(tree_view, path, user_data));
} 

gboolean
S_GtkTreeViewSearchEqualFunc(GtkTreeModel* model, gint column, const gchar* key, GtkTreeIter* iter, gpointer search_data)
{
  static gboolean (*fun)(GtkTreeModel*, gint, const gchar*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkTreeModel*, gint, const gchar*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeViewSearchEqualFunc"));
  return(fun(model, column, key, iter, search_data));
} 

void
S_GtkTreeDestroyCountFunc(GtkTreeView* tree_view, GtkTreePath* path, gint children, gpointer user_data)
{
  static void (*fun)(GtkTreeView*, GtkTreePath*, gint, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkTreeView*, GtkTreePath*, gint, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeDestroyCountFunc"));
  return(fun(tree_view, path, children, user_data));
} 

gboolean
S_GtkTreeViewRowSeparatorFunc(GtkTreeModel* model, GtkTreeIter* iter, gpointer data)
{
  static gboolean (*fun)(GtkTreeModel*, GtkTreeIter*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkTreeModel*, GtkTreeIter*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeViewRowSeparatorFunc"));
  return(fun(model, iter, data));
} 

void
S_GtkCallback(GtkWidget* child, gpointer data)
{
  static void (*fun)(GtkWidget*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkWidget*, gpointer))R_GetCCallable("RGtk2", "S_GtkCallback"));
  return(fun(child, data));
} 

void
S_GtkAccelMapForeach(gpointer data, const gchar* accel_path, guint accel_key, GdkModifierType accel_mods, gboolean changed)
{
  static void (*fun)(gpointer, const gchar*, guint, GdkModifierType, gboolean) = NULL;
  if(!fun) fun = ((void (*)(gpointer, const gchar*, guint, GdkModifierType, gboolean))R_GetCCallable("RGtk2", "S_GtkAccelMapForeach"));
  return(fun(data, accel_path, accel_key, accel_mods, changed));
} 

gboolean
S_GtkAccelGroupFindFunc(GtkAccelKey* key, GClosure* closure, gpointer data)
{
  static gboolean (*fun)(GtkAccelKey*, GClosure*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkAccelKey*, GClosure*, gpointer))R_GetCCallable("RGtk2", "S_GtkAccelGroupFindFunc"));
  return(fun(key, closure, data));
} 

gboolean
S_GtkAccelGroupActivate(GtkAccelGroup* accel_group, GObject* acceleratable, guint keyval, GdkModifierType modifier)
{
  static gboolean (*fun)(GtkAccelGroup*, GObject*, guint, GdkModifierType) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkAccelGroup*, GObject*, guint, GdkModifierType))R_GetCCallable("RGtk2", "S_GtkAccelGroupActivate"));
  return(fun(accel_group, acceleratable, keyval, modifier));
} 

void
S_GtkTextTagTableForeach(GtkTextTag* tag, gpointer data)
{
  static void (*fun)(GtkTextTag*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkTextTag*, gpointer))R_GetCCallable("RGtk2", "S_GtkTextTagTableForeach"));
  return(fun(tag, data));
} 

gboolean
S_GtkTextCharPredicate(gunichar ch, gpointer user_data)
{
  static gboolean (*fun)(gunichar, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(gunichar, gpointer))R_GetCCallable("RGtk2", "S_GtkTextCharPredicate"));
  return(fun(ch, user_data));
} 

void
S_GtkItemFactoryCallback1(gpointer callback_data, guint callback_action, GtkWidget* widget)
{
  static void (*fun)(gpointer, guint, GtkWidget*) = NULL;
  if(!fun) fun = ((void (*)(gpointer, guint, GtkWidget*))R_GetCCallable("RGtk2", "S_GtkItemFactoryCallback1"));
  return(fun(callback_data, callback_action, widget));
} 

void
S_GtkItemFactoryCallback2(GtkWidget* widget, gpointer callback_data, guint callback_action)
{
  static void (*fun)(GtkWidget*, gpointer, guint) = NULL;
  if(!fun) fun = ((void (*)(GtkWidget*, gpointer, guint))R_GetCCallable("RGtk2", "S_GtkItemFactoryCallback2"));
  return(fun(widget, callback_data, callback_action));
} 

#if GTK_CHECK_VERSION(2, 10, 0)
gint
S_GtkAssistantPageFunc(gint current_page, gpointer data)
{
  static gint (*fun)(gint, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(gint, gpointer))R_GetCCallable("RGtk2", "S_GtkAssistantPageFunc"));
  return(fun(current_page, data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
void
S_GtkClipboardRichTextReceivedFunc(GtkClipboard* clipboard, GdkAtom format, const guint8* text, gsize length, gpointer data)
{
  static void (*fun)(GtkClipboard*, GdkAtom, const guint8*, gsize, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, GdkAtom, const guint8*, gsize, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardRichTextReceivedFunc"));
  return(fun(clipboard, format, text, length, data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
void
S_GtkLinkButtonUriFunc(GtkLinkButton* button, const gchar* link, gpointer user_data)
{
  static void (*fun)(GtkLinkButton*, const gchar*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkLinkButton*, const gchar*, gpointer))R_GetCCallable("RGtk2", "S_GtkLinkButtonUriFunc"));
  return(fun(button, link, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
GtkNotebook*
S_GtkNotebookWindowCreationFunc(GtkNotebook* source, GtkWidget* page, gint x, gint y, gpointer data)
{
  static GtkNotebook* (*fun)(GtkNotebook*, GtkWidget*, gint, gint, gpointer) = NULL;
  if(!fun) fun = ((GtkNotebook* (*)(GtkNotebook*, GtkWidget*, gint, gint, gpointer))R_GetCCallable("RGtk2", "S_GtkNotebookWindowCreationFunc"));
  return(fun(source, page, x, y, data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
void
S_GtkPageSetupDoneFunc(GtkPageSetup* page_setup, gpointer data)
{
  static void (*fun)(GtkPageSetup*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkPageSetup*, gpointer))R_GetCCallable("RGtk2", "S_GtkPageSetupDoneFunc"));
  return(fun(page_setup, data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
void
S_GtkPrintSettingsFunc(const gchar* key, const gchar* value, gpointer user_data)
{
  static void (*fun)(const gchar*, const gchar*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(const gchar*, const gchar*, gpointer))R_GetCCallable("RGtk2", "S_GtkPrintSettingsFunc"));
  return(fun(key, value, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
gint
S_GtkRecentSortFunc(GtkRecentInfo* a, GtkRecentInfo* b, gpointer user_data)
{
  static gint (*fun)(GtkRecentInfo*, GtkRecentInfo*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(GtkRecentInfo*, GtkRecentInfo*, gpointer))R_GetCCallable("RGtk2", "S_GtkRecentSortFunc"));
  return(fun(a, b, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
gboolean
S_GtkRecentFilterFunc(const GtkRecentFilterInfo* filter_info, gpointer user_data)
{
  static gboolean (*fun)(const GtkRecentFilterInfo*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(const GtkRecentFilterInfo*, gpointer))R_GetCCallable("RGtk2", "S_GtkRecentFilterFunc"));
  return(fun(filter_info, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
gboolean
S_GtkTextBufferDeserializeFunc(GtkTextBuffer* register_buffer, GtkTextBuffer* content_buffer, GtkTextIter* iter, const guint8* data, gsize length, gboolean create_tags, gpointer user_data, GError** error)
{
  static gboolean (*fun)(GtkTextBuffer*, GtkTextBuffer*, GtkTextIter*, const guint8*, gsize, gboolean, gpointer, GError**) = NULL;
  if(!fun) fun = ((gboolean (*)(GtkTextBuffer*, GtkTextBuffer*, GtkTextIter*, const guint8*, gsize, gboolean, gpointer, GError**))R_GetCCallable("RGtk2", "S_GtkTextBufferDeserializeFunc"));
  return(fun(register_buffer, content_buffer, iter, data, length, create_tags, user_data, error));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
void
S_GtkTreeViewSearchPositionFunc(GtkTreeView* tree_view, GtkWidget* search_dialog, gpointer user_data)
{
  static void (*fun)(GtkTreeView*, GtkWidget*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkTreeView*, GtkWidget*, gpointer))R_GetCCallable("RGtk2", "S_GtkTreeViewSearchPositionFunc"));
  return(fun(tree_view, search_dialog, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 12, 0)
void
S_GtkBuilderConnectFunc(GtkBuilder* builder, GObject* object, const gchar* signal_name, const gchar* handler_name, GObject* connect_object, guint flags, gpointer user_data)
{
  static void (*fun)(GtkBuilder*, GObject*, const gchar*, const gchar*, GObject*, guint, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkBuilder*, GObject*, const gchar*, const gchar*, GObject*, guint, gpointer))R_GetCCallable("RGtk2", "S_GtkBuilderConnectFunc"));
  return(fun(builder, object, signal_name, handler_name, connect_object, flags, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 14, 0)
gchar*
S_GtkCalendarDetailFunc(GtkCalendar* calendar, guint year, guint month, guint day, gpointer user_data)
{
  static gchar* (*fun)(GtkCalendar*, guint, guint, guint, gpointer) = NULL;
  if(!fun) fun = ((gchar* (*)(GtkCalendar*, guint, guint, guint, gpointer))R_GetCCallable("RGtk2", "S_GtkCalendarDetailFunc"));
  return(fun(calendar, year, month, day, user_data));
}
#endif 

#if GTK_CHECK_VERSION(2, 14, 0)
void
S_GtkClipboardURIReceivedFunc(GtkClipboard* clipboard, gchar** uris, gpointer user_data)
{
  static void (*fun)(GtkClipboard*, gchar**, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, gchar**, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardURIReceivedFunc"));
  return(fun(clipboard, uris, user_data));
}
#endif 

