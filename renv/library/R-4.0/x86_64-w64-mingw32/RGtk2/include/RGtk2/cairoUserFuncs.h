#ifndef S_CAIRO_USERFUNCS_H
#define S_CAIRO_USERFUNCS_H
#include <RGtk2/gobject.h>
#include <RGtk2/cairo.h>


  cairo_status_t
S_cairo_write_func_t(gpointer s_closure, const guchar* s_data, guint s_length); 

#endif
