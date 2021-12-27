#ifndef S_GTK_USERFUNCS_H
#define S_GTK_USERFUNCS_H
#include <RGtk2/gobject.h>
#include <RGtk2/gtk.h>


  void
S_GtkAboutDialogActivateLinkFunc(GtkAboutDialog* s_about, const gchar* s_link, gpointer s_data); 

  void
S_GtkCellLayoutDataFunc(GtkCellLayout* s_cell_layout, GtkCellRenderer* s_cell, GtkTreeModel* s_tree_model, GtkTreeIter* s_iter, gpointer s_data); 

  void
S_GtkClipboardGetFunc(GtkClipboard* s_clipboard, GtkSelectionData* s_selection_data, guint s_info, gpointer s_user_data_or_owner); 

  void
S_GtkClipboardReceivedFunc(GtkClipboard* s_clipboard, GtkSelectionData* s_selection_data, gpointer s_data); 

  void
S_GtkClipboardImageReceivedFunc(GtkClipboard* s_clipboard, GdkPixbuf* s_image, gpointer s_data); 

  void
S_GtkClipboardTextReceivedFunc(GtkClipboard* s_clipboard, const gchar* s_text, gpointer s_data); 

  void
S_GtkClipboardTargetsReceivedFunc(GtkClipboard* s_clipboard, GdkAtom* s_atoms, gint s_n_atoms, gpointer s_data); 

  void
S_GtkColorSelectionChangePaletteFunc(const GdkColor* s_colors, gint s_n_colors); 

  void
S_GtkColorSelectionChangePaletteWithScreenFunc(GdkScreen* s_screen, const GdkColor* s_colors, gint s_n_colors); 

  gboolean
S_GtkCTreeGNodeFunc(GtkCTree* s_ctree, guint s_depth, GNode* s_gnode, GtkCTreeNode* s_cnode, gpointer s_data); 

  void
S_GtkCTreeFunc(GtkCTree* s_ctree, GtkCTreeNode* s_node, gpointer s_data); 

  gboolean
S_GtkEntryCompletionMatchFunc(GtkEntryCompletion* s_completion, const gchar* s_key, GtkTreeIter* s_iter, gpointer s_user_data); 

  gboolean
S_GtkFileFilterFunc(const GtkFileFilterInfo* s_filter_info, gpointer s_data); 

  void
S_GtkIconViewForeachFunc(GtkIconView* s_icon_view, GtkTreePath* s_path, gpointer s_data); 

  void
S_GtkTranslateFunc(const gchar* s_path, gpointer s_func_data); 

  gboolean
S_GtkFunction(gpointer s_data); 

  gint
S_GtkKeySnoopFunc(GtkWidget* s_grab_widget, GdkEventKey* s_event, gpointer s_func_data); 

  gint
S_GtkMenuPositionFunc(GtkMenu* s_menu, gint* s_x, gint* s_y, gboolean* s_push_in, gpointer s_user_data); 

  gint
S_GtkTreeModelForeachFunc(GtkTreeModel* s_model, GtkTreePath* s_path, GtkTreeIter* s_iter, gpointer s_data); 

  gint
S_GtkTreeModelFilterVisibleFunc(GtkTreeModel* s_model, GtkTreeIter* s_iter, gpointer s_data); 

  gint
S_GtkTreeModelFilterModifyFunc(GtkTreeModel* s_model, GtkTreeIter* s_iter, GValue* s_value, gint s_column, gpointer s_data); 

  gboolean
S_GtkTreeSelectionFunc(GtkTreeSelection* s_selection, GtkTreeModel* s_model, GtkTreePath* s_path, gboolean s_path_currently_selected, gpointer s_data); 

  void
S_GtkTreeSelectionForeachFunc(GtkTreeModel* s_model, GtkTreePath* s_path, GtkTreeIter* s_iter, gpointer s_data); 

  gint
S_GtkTreeIterCompareFunc(GtkTreeModel* s_model, GtkTreeIter* s_a, GtkTreeIter* s_b, gpointer s_user_data); 

  void
S_GtkTreeCellDataFunc(GtkTreeViewColumn* s_tree_column, GtkCellRenderer* s_cell, GtkTreeModel* s_tree_model, GtkTreeIter* s_iter, gpointer s_data); 

  gboolean
S_GtkTreeViewColumnDropFunc(GtkTreeView* s_tree_view, GtkTreeViewColumn* s_column, GtkTreeViewColumn* s_prev_column, GtkTreeViewColumn* s_next_column, gpointer s_data); 

  void
S_GtkTreeViewMappingFunc(GtkTreeView* s_tree_view, GtkTreePath* s_path, gpointer s_user_data); 

  gboolean
S_GtkTreeViewSearchEqualFunc(GtkTreeModel* s_model, gint s_column, const gchar* s_key, GtkTreeIter* s_iter, gpointer s_search_data); 

  void
S_GtkTreeDestroyCountFunc(GtkTreeView* s_tree_view, GtkTreePath* s_path, gint s_children, gpointer s_user_data); 

  gboolean
S_GtkTreeViewRowSeparatorFunc(GtkTreeModel* s_model, GtkTreeIter* s_iter, gpointer s_data); 

  void
S_GtkCallback(GtkWidget* s_child, gpointer s_data); 

  void
S_GtkAccelMapForeach(gpointer s_data, const gchar* s_accel_path, guint s_accel_key, GdkModifierType s_accel_mods, gboolean s_changed); 

  gboolean
S_GtkAccelGroupFindFunc(GtkAccelKey* s_key, GClosure* s_closure, gpointer s_data); 

  gboolean
S_GtkAccelGroupActivate(GtkAccelGroup* s_accel_group, GObject* s_acceleratable, guint s_keyval, GdkModifierType s_modifier); 

  void
S_GtkTextTagTableForeach(GtkTextTag* s_tag, gpointer s_data); 

  gboolean
S_GtkTextCharPredicate(gunichar s_ch, gpointer s_user_data); 

  void
S_GtkItemFactoryCallback1(gpointer s_callback_data, guint s_callback_action, GtkWidget* s_widget); 

  void
S_GtkItemFactoryCallback2(GtkWidget* s_widget, gpointer s_callback_data, guint s_callback_action); 

#if GTK_CHECK_VERSION(2, 10, 0)
  gint
S_GtkAssistantPageFunc(gint s_current_page, gpointer s_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  void
S_GtkClipboardRichTextReceivedFunc(GtkClipboard* s_clipboard, GdkAtom s_format, const guint8* s_text, gsize s_length, gpointer s_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  void
S_GtkLinkButtonUriFunc(GtkLinkButton* s_button, const gchar* s_link, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  GtkNotebook*
S_GtkNotebookWindowCreationFunc(GtkNotebook* s_source, GtkWidget* s_page, gint s_x, gint s_y, gpointer s_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  void
S_GtkPageSetupDoneFunc(GtkPageSetup* s_page_setup, gpointer s_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  void
S_GtkPrintSettingsFunc(const gchar* s_key, const gchar* s_value, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  gint
S_GtkRecentSortFunc(GtkRecentInfo* s_a, GtkRecentInfo* s_b, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  gboolean
S_GtkRecentFilterFunc(const GtkRecentFilterInfo* s_filter_info, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  gboolean
S_GtkTextBufferDeserializeFunc(GtkTextBuffer* s_register_buffer, GtkTextBuffer* s_content_buffer, GtkTextIter* s_iter, const guint8* s_data, gsize s_length, gboolean s_create_tags, gpointer s_user_data, GError** s_error);
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
  void
S_GtkTreeViewSearchPositionFunc(GtkTreeView* s_tree_view, GtkWidget* s_search_dialog, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 12, 0)
  void
S_GtkBuilderConnectFunc(GtkBuilder* s_builder, GObject* s_object, const gchar* s_signal_name, const gchar* s_handler_name, GObject* s_connect_object, guint s_flags, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 14, 0)
  gchar*
S_GtkCalendarDetailFunc(GtkCalendar* s_calendar, guint s_year, guint s_month, guint s_day, gpointer s_user_data);
#endif 

#if GTK_CHECK_VERSION(2, 14, 0)
  void
S_GtkClipboardURIReceivedFunc(GtkClipboard* s_clipboard, gchar** s_uris, gpointer s_user_data);
#endif 

#endif
