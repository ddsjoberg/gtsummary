#ifndef RGTK2_GTK_H
#define RGTK2_GTK_H

#include <RGtk2/atk.h>
#include <RGtk2/pango.h>
#include <RGtk2/cairo.h>
#include <RGtk2/gio.h>

/* GdkPixbuf is inside Gdk, and Gdk is inside GTK+ (wrt distribution),
   so we group them all into this header.
*/
#define GDK_PIXBUF_ENABLE_BACKEND
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtk.h>

#define GDK_CHECK_VERSION GTK_CHECK_VERSION

#include <RGtk2/gdkClasses.h>
#include <RGtk2/gdkUserFuncs.h>
#include <RGtk2/gtkClasses.h>
#include <RGtk2/gtkUserFuncs.h>

/******* Manual UserFuncs *********/

/* GTK */
void S_GtkClipboardClearFunc(GtkClipboard *clipboard, gpointer user_data_or_owner);
void S_GtkSignalFunc(GtkWidget* s_child, gpointer s_data);
guint8* S_GtkTextBufferSerializeFunc(GtkTextBuffer* s_register_buffer, 
  GtkTextBuffer* s_content_buffer, GtkTextIter* s_start, GtkTextIter* s_end, 
  gsize* s_length, gpointer s_user_data);
gint S_GtkMenuPositionFunc(GtkMenu* s_menu, gint* s_x, gint* s_y, gboolean* s_push_in, gpointer s_user_data);

/******* Conversion ********/

/* GDK */
USER_OBJECT_ asRGdkAtom(GdkAtom val);
GdkAtom asCGdkAtom(USER_OBJECT_ s_atom);
GdkAtom* asCGdkAtomArray(USER_OBJECT_ s_atoms);
GdkGeometry* asCGdkGeometry(USER_OBJECT_ s_geom, GdkWindowHints *hints);
GdkGCValues* asCGdkGCValues(USER_OBJECT_ s_values);
GdkGCValues* asCGdkGCValuesWithMask(USER_OBJECT_ s_values, GdkGCValuesMask *mask);
GdkWindowAttr* asCGdkWindowAttr(USER_OBJECT_ s_attr, GdkWindowAttributesType *mask);
USER_OBJECT_ asRGdkTimeCoord(GdkTimeCoord *coord, int num_axes);
GdkRectangle* asCGdkRectangle(USER_OBJECT_ s_rect);
USER_OBJECT_ asRGdkRectangle(GdkRectangle *rect);
GdkRgbCmap* asCGdkRgbCmap(USER_OBJECT_ s_cmap);
USER_OBJECT_ asRGdkRgbCmap(GdkRgbCmap *map);
GdkKeymapKey* asCGdkKeymapKey(USER_OBJECT_ s_key);
USER_OBJECT_ asRGdkKeymapKey(GdkKeymapKey* key);
GdkPoint* asCGdkPoint(USER_OBJECT_ s_point);
USER_OBJECT_ asRGdkPoint(GdkPoint *point);
GdkSegment* asCGdkSegment(USER_OBJECT_ s_segment);
USER_OBJECT_ asRGdkSegment(GdkSegment * obj);
GdkColor* asCGdkColor(USER_OBJECT_ s_color);
USER_OBJECT_ asRGdkColor(const GdkColor* color);
USER_OBJECT_ asRGdkNativeWindow(GdkNativeWindow window);
GdkNativeWindow asCGdkNativeWindow(USER_OBJECT_ s_window);
USER_OBJECT_ asRGdkEvent(GdkEvent *event);
USER_OBJECT_ toRGdkEvent(GdkEvent *event, gboolean finalize);
USER_OBJECT_ toRGdkFont(GdkFont *font);
GdkTrapezoid * asCGdkTrapezoid(USER_OBJECT_ s_trapezoid);
USER_OBJECT_ asRGdkTrapezoid(GdkTrapezoid * obj);
USER_OBJECT_ asRGdkGCValues(GdkGCValues *values);
GdkSpan* asCGdkSpan(USER_OBJECT_ s_span);
USER_OBJECT_ asRGdkSpan(GdkSpan * obj);

/* GTK */

GtkTargetEntry* asCGtkTargetEntry(USER_OBJECT_ s_entry);
USER_OBJECT_ asRGtkTargetEntry(const GtkTargetEntry * obj);
GtkFileFilterInfo* asCGtkFileFilterInfo(USER_OBJECT_ s_info);
USER_OBJECT_ asRGtkFileFilterInfo(const GtkFileFilterInfo * obj);
GtkSettingsValue* asCGtkSettingsValue(USER_OBJECT_ s_value);
GtkStockItem* asCGtkStockItem(USER_OBJECT_ s_item);
USER_OBJECT_ asRGtkStockItem(GtkStockItem *item);
GtkItemFactoryEntry* asCGtkItemFactoryEntry(USER_OBJECT_ s_entry);
GtkItemFactoryEntry* asCGtkItemFactoryEntry2(USER_OBJECT_ s_entry);
GtkItemFactoryEntry* R_createGtkItemFactoryEntry(USER_OBJECT_ s_entry, guint cbtype);
GtkAllocation* asCGtkAllocation(USER_OBJECT_ s_alloc);
USER_OBJECT_ asRGtkAllocation(GtkAllocation* alloc);
#if GTK_CHECK_VERSION(2,10,0)
GtkRecentFilterInfo * asCGtkRecentFilterInfo(USER_OBJECT_ s_obj);
USER_OBJECT_ asRGtkRecentFilterInfo(const GtkRecentFilterInfo * obj);
GtkRecentData * asCGtkRecentData(USER_OBJECT_ s_obj);
USER_OBJECT_ asRGtkPageRange(GtkPageRange * obj);
GtkPageRange * asCGtkPageRange(USER_OBJECT_ s_obj);
#endif
USER_OBJECT_ asRGtkAccelKey(GtkAccelKey * obj);

/* For some systems, e.g. Irix, gtkFuncs has a time_t that is not defined
   unless we include this. Seems harmless on other systems! */
#include <time.h>

#endif
