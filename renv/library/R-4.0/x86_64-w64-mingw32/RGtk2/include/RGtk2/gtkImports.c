#ifndef S_GTK_IMPORTS_C
#define S_GTK_IMPORTS_C
#include <RGtk2/gtk.h>

#include <RGtk2/gdkImports.c>

#include <RGtk2/atkImports.c>

#include <RGtk2/gtkUserFuncImports.c>

#include <RGtk2/gtkClassImports.c>

void
S_GtkClipboardClearFunc(GtkClipboard* clipboard, gpointer user_data_or_owner)
{
  static void (*fun)(GtkClipboard*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkClipboard*, gpointer))R_GetCCallable("RGtk2", "S_GtkClipboardClearFunc"));
  return(fun(clipboard, user_data_or_owner));
} 

void
S_GtkSignalFunc(GtkWidget* s_child, gpointer s_data)
{
  static void (*fun)(GtkWidget*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GtkWidget*, gpointer))R_GetCCallable("RGtk2", "S_GtkSignalFunc"));
  return(fun(s_child, s_data));
} 

guint8*
S_GtkTextBufferSerializeFunc(GtkTextBuffer* s_register_buffer, GtkTextBuffer* s_content_buffer, GtkTextIter* s_start, GtkTextIter* s_end, gsize* s_length, gpointer s_user_data)
{
  static guint8* (*fun)(GtkTextBuffer*, GtkTextBuffer*, GtkTextIter*, GtkTextIter*, gsize*, gpointer) = NULL;
  if(!fun) fun = ((guint8* (*)(GtkTextBuffer*, GtkTextBuffer*, GtkTextIter*, GtkTextIter*, gsize*, gpointer))R_GetCCallable("RGtk2", "S_GtkTextBufferSerializeFunc"));
  return(fun(s_register_buffer, s_content_buffer, s_start, s_end, s_length, s_user_data));
} 

USER_OBJECT_
asRGdkAtom(GdkAtom val)
{
  static USER_OBJECT_ (*fun)(GdkAtom) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkAtom))R_GetCCallable("RGtk2", "asRGdkAtom"));
  return(fun(val));
} 

GdkAtom
asCGdkAtom(USER_OBJECT_ s_atom)
{
  static GdkAtom (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkAtom (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkAtom"));
  return(fun(s_atom));
} 

GdkAtom*
asCGdkAtomArray(USER_OBJECT_ s_atoms)
{
  static GdkAtom* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkAtom* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkAtomArray"));
  return(fun(s_atoms));
} 

GdkGeometry*
asCGdkGeometry(USER_OBJECT_ s_geom, GdkWindowHints* hints)
{
  static GdkGeometry* (*fun)(USER_OBJECT_, GdkWindowHints*) = NULL;
  if(!fun) fun = ((GdkGeometry* (*)(USER_OBJECT_, GdkWindowHints*))R_GetCCallable("RGtk2", "asCGdkGeometry"));
  return(fun(s_geom, hints));
} 

GdkGCValues*
asCGdkGCValues(USER_OBJECT_ s_values)
{
  static GdkGCValues* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkGCValues* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkGCValues"));
  return(fun(s_values));
} 

GdkGCValues*
asCGdkGCValuesWithMask(USER_OBJECT_ s_values, GdkGCValuesMask* mask)
{
  static GdkGCValues* (*fun)(USER_OBJECT_, GdkGCValuesMask*) = NULL;
  if(!fun) fun = ((GdkGCValues* (*)(USER_OBJECT_, GdkGCValuesMask*))R_GetCCallable("RGtk2", "asCGdkGCValuesWithMask"));
  return(fun(s_values, mask));
} 

GdkWindowAttr*
asCGdkWindowAttr(USER_OBJECT_ s_attr, GdkWindowAttributesType* mask)
{
  static GdkWindowAttr* (*fun)(USER_OBJECT_, GdkWindowAttributesType*) = NULL;
  if(!fun) fun = ((GdkWindowAttr* (*)(USER_OBJECT_, GdkWindowAttributesType*))R_GetCCallable("RGtk2", "asCGdkWindowAttr"));
  return(fun(s_attr, mask));
} 

USER_OBJECT_
asRGdkTimeCoord(GdkTimeCoord* coord, int num_axes)
{
  static USER_OBJECT_ (*fun)(GdkTimeCoord*, int) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkTimeCoord*, int))R_GetCCallable("RGtk2", "asRGdkTimeCoord"));
  return(fun(coord, num_axes));
} 

GdkRectangle*
asCGdkRectangle(USER_OBJECT_ s_rect)
{
  static GdkRectangle* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkRectangle* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkRectangle"));
  return(fun(s_rect));
} 

USER_OBJECT_
asRGdkRectangle(GdkRectangle* rect)
{
  static USER_OBJECT_ (*fun)(GdkRectangle*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkRectangle*))R_GetCCallable("RGtk2", "asRGdkRectangle"));
  return(fun(rect));
} 

GdkRgbCmap*
asCGdkRgbCmap(USER_OBJECT_ s_cmap)
{
  static GdkRgbCmap* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkRgbCmap* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkRgbCmap"));
  return(fun(s_cmap));
} 

USER_OBJECT_
asRGdkRgbCmap(GdkRgbCmap* map)
{
  static USER_OBJECT_ (*fun)(GdkRgbCmap*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkRgbCmap*))R_GetCCallable("RGtk2", "asRGdkRgbCmap"));
  return(fun(map));
} 

GdkKeymapKey*
asCGdkKeymapKey(USER_OBJECT_ s_key)
{
  static GdkKeymapKey* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkKeymapKey* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkKeymapKey"));
  return(fun(s_key));
} 

USER_OBJECT_
asRGdkKeymapKey(GdkKeymapKey* key)
{
  static USER_OBJECT_ (*fun)(GdkKeymapKey*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkKeymapKey*))R_GetCCallable("RGtk2", "asRGdkKeymapKey"));
  return(fun(key));
} 

GdkPoint*
asCGdkPoint(USER_OBJECT_ s_point)
{
  static GdkPoint* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkPoint* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkPoint"));
  return(fun(s_point));
} 

USER_OBJECT_
asRGdkPoint(GdkPoint* point)
{
  static USER_OBJECT_ (*fun)(GdkPoint*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkPoint*))R_GetCCallable("RGtk2", "asRGdkPoint"));
  return(fun(point));
} 

GdkSegment*
asCGdkSegment(USER_OBJECT_ s_segment)
{
  static GdkSegment* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkSegment* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkSegment"));
  return(fun(s_segment));
} 

USER_OBJECT_
asRGdkSegment(GdkSegment* obj)
{
  static USER_OBJECT_ (*fun)(GdkSegment*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkSegment*))R_GetCCallable("RGtk2", "asRGdkSegment"));
  return(fun(obj));
} 

GdkColor*
asCGdkColor(USER_OBJECT_ s_color)
{
  static GdkColor* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkColor* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkColor"));
  return(fun(s_color));
} 

USER_OBJECT_
asRGdkColor(const GdkColor* color)
{
  static USER_OBJECT_ (*fun)(const GdkColor*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(const GdkColor*))R_GetCCallable("RGtk2", "asRGdkColor"));
  return(fun(color));
} 

USER_OBJECT_
asRGdkNativeWindow(GdkNativeWindow window)
{
  static USER_OBJECT_ (*fun)(GdkNativeWindow) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkNativeWindow))R_GetCCallable("RGtk2", "asRGdkNativeWindow"));
  return(fun(window));
} 

GdkNativeWindow
asCGdkNativeWindow(USER_OBJECT_ s_window)
{
  static GdkNativeWindow (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkNativeWindow (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkNativeWindow"));
  return(fun(s_window));
} 

USER_OBJECT_
asRGdkEvent(GdkEvent* event)
{
  static USER_OBJECT_ (*fun)(GdkEvent*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkEvent*))R_GetCCallable("RGtk2", "asRGdkEvent"));
  return(fun(event));
} 

USER_OBJECT_
toRGdkEvent(GdkEvent* event, gboolean finalize)
{
  static USER_OBJECT_ (*fun)(GdkEvent*, gboolean) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkEvent*, gboolean))R_GetCCallable("RGtk2", "toRGdkEvent"));
  return(fun(event, finalize));
} 

USER_OBJECT_
toRGdkFont(GdkFont* font)
{
  static USER_OBJECT_ (*fun)(GdkFont*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkFont*))R_GetCCallable("RGtk2", "toRGdkFont"));
  return(fun(font));
} 

GdkTrapezoid*
asCGdkTrapezoid(USER_OBJECT_ s_trapezoid)
{
  static GdkTrapezoid* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkTrapezoid* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkTrapezoid"));
  return(fun(s_trapezoid));
} 

USER_OBJECT_
asRGdkTrapezoid(GdkTrapezoid* obj)
{
  static USER_OBJECT_ (*fun)(GdkTrapezoid*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkTrapezoid*))R_GetCCallable("RGtk2", "asRGdkTrapezoid"));
  return(fun(obj));
} 

USER_OBJECT_
asRGdkGCValues(GdkGCValues* values)
{
  static USER_OBJECT_ (*fun)(GdkGCValues*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkGCValues*))R_GetCCallable("RGtk2", "asRGdkGCValues"));
  return(fun(values));
} 

GdkSpan*
asCGdkSpan(USER_OBJECT_ s_span)
{
  static GdkSpan* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GdkSpan* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGdkSpan"));
  return(fun(s_span));
} 

USER_OBJECT_
asRGdkSpan(GdkSpan* obj)
{
  static USER_OBJECT_ (*fun)(GdkSpan*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GdkSpan*))R_GetCCallable("RGtk2", "asRGdkSpan"));
  return(fun(obj));
} 

GtkTargetEntry*
asCGtkTargetEntry(USER_OBJECT_ s_entry)
{
  static GtkTargetEntry* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkTargetEntry* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkTargetEntry"));
  return(fun(s_entry));
} 

USER_OBJECT_
asRGtkTargetEntry(GtkTargetEntry* obj)
{
  static USER_OBJECT_ (*fun)(GtkTargetEntry*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GtkTargetEntry*))R_GetCCallable("RGtk2", "asRGtkTargetEntry"));
  return(fun(obj));
} 

GtkFileFilterInfo*
asCGtkFileFilterInfo(USER_OBJECT_ s_info)
{
  static GtkFileFilterInfo* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkFileFilterInfo* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkFileFilterInfo"));
  return(fun(s_info));
} 

USER_OBJECT_
asRGtkFileFilterInfo(const GtkFileFilterInfo* obj)
{
  static USER_OBJECT_ (*fun)(const GtkFileFilterInfo*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(const GtkFileFilterInfo*))R_GetCCallable("RGtk2", "asRGtkFileFilterInfo"));
  return(fun(obj));
} 

GtkSettingsValue*
asCGtkSettingsValue(USER_OBJECT_ s_value)
{
  static GtkSettingsValue* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkSettingsValue* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkSettingsValue"));
  return(fun(s_value));
} 

GtkStockItem*
asCGtkStockItem(USER_OBJECT_ s_item)
{
  static GtkStockItem* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkStockItem* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkStockItem"));
  return(fun(s_item));
} 

USER_OBJECT_
asRGtkStockItem(GtkStockItem* item)
{
  static USER_OBJECT_ (*fun)(GtkStockItem*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GtkStockItem*))R_GetCCallable("RGtk2", "asRGtkStockItem"));
  return(fun(item));
} 

GtkItemFactoryEntry*
asCGtkItemFactoryEntry(USER_OBJECT_ s_entry)
{
  static GtkItemFactoryEntry* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkItemFactoryEntry* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkItemFactoryEntry"));
  return(fun(s_entry));
} 

GtkItemFactoryEntry*
asCGtkItemFactoryEntry2(USER_OBJECT_ s_entry)
{
  static GtkItemFactoryEntry* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkItemFactoryEntry* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkItemFactoryEntry2"));
  return(fun(s_entry));
} 

GtkItemFactoryEntry*
R_createGtkItemFactoryEntry(USER_OBJECT_ s_entry, guint cbtype)
{
  static GtkItemFactoryEntry* (*fun)(USER_OBJECT_, guint) = NULL;
  if(!fun) fun = ((GtkItemFactoryEntry* (*)(USER_OBJECT_, guint))R_GetCCallable("RGtk2", "R_createGtkItemFactoryEntry"));
  return(fun(s_entry, cbtype));
} 

GtkAllocation*
asCGtkAllocation(USER_OBJECT_ s_alloc)
{
  static GtkAllocation* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkAllocation* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkAllocation"));
  return(fun(s_alloc));
} 

USER_OBJECT_
asRGtkAllocation(GtkAllocation* alloc)
{
  static USER_OBJECT_ (*fun)(GtkAllocation*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GtkAllocation*))R_GetCCallable("RGtk2", "asRGtkAllocation"));
  return(fun(alloc));
} 

#if GTK_CHECK_VERSION(2, 10, 0)
GtkRecentFilterInfo*
asCGtkRecentFilterInfo(USER_OBJECT_ s_obj)
{
  static GtkRecentFilterInfo* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkRecentFilterInfo* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkRecentFilterInfo"));
  return(fun(s_obj));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
USER_OBJECT_
asRGtkRecentFilterInfo(const GtkRecentFilterInfo* obj)
{
  static USER_OBJECT_ (*fun)(const GtkRecentFilterInfo*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(const GtkRecentFilterInfo*))R_GetCCallable("RGtk2", "asRGtkRecentFilterInfo"));
  return(fun(obj));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
GtkRecentData*
asCGtkRecentData(USER_OBJECT_ s_obj)
{
  static GtkRecentData* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkRecentData* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkRecentData"));
  return(fun(s_obj));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
USER_OBJECT_
asRGtkPageRange(GtkPageRange* obj)
{
  static USER_OBJECT_ (*fun)(GtkPageRange*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GtkPageRange*))R_GetCCallable("RGtk2", "asRGtkPageRange"));
  return(fun(obj));
}
#endif 

#if GTK_CHECK_VERSION(2, 10, 0)
GtkPageRange*
asCGtkPageRange(USER_OBJECT_ s_obj)
{
  static GtkPageRange* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GtkPageRange* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGtkPageRange"));
  return(fun(s_obj));
}
#endif 

USER_OBJECT_
asRGtkAccelKey(GtkAccelKey* obj)
{
  static USER_OBJECT_ (*fun)(GtkAccelKey*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GtkAccelKey*))R_GetCCallable("RGtk2", "asRGtkAccelKey"));
  return(fun(obj));
} 

#endif
