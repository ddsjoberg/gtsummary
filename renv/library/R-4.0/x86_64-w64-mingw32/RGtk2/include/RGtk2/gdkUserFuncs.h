#ifndef S_GDK_USERFUNCS_H
#define S_GDK_USERFUNCS_H
#include <RGtk2/gobject.h>
#include <RGtk2/gdk.h>


  void
S_GdkFilterFunc(GdkXEvent* s_xevent, GdkEvent* s_event, gpointer s_data); 

  void
S_GdkEventFunc(GdkEvent* s_event, gpointer s_data); 

  gboolean
S_GdkPixbufSaveFunc(const guchar* s_buf, gsize s_count, GError** s_error, gpointer s_data); 

  void
S_GdkSpanFunc(GdkSpan* s_span, gpointer s_data); 

#endif
