#ifndef S_CAIRO_IMPORTS_C
#define S_CAIRO_IMPORTS_C
#include <RGtk2/cairo.h>

#include <RGtk2/gobjectImports.c>

#include <RGtk2/cairoUserFuncImports.c>

cairo_status_t
S_cairo_read_func_t(gpointer s_closure, guchar* s_data, guint s_length)
{
  static cairo_status_t (*fun)(gpointer, guchar*, guint) = NULL;
  if(!fun) fun = ((cairo_status_t (*)(gpointer, guchar*, guint))R_GetCCallable("RGtk2", "S_cairo_read_func_t"));
  return(fun(s_closure, s_data, s_length));
} 

USER_OBJECT_
asRCairoPath(cairo_path_t* path)
{
  static USER_OBJECT_ (*fun)(cairo_path_t*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(cairo_path_t*))R_GetCCallable("RGtk2", "asRCairoPath"));
  return(fun(path));
} 

cairo_path_t*
asCCairoPath(USER_OBJECT_ s_path)
{
  static cairo_path_t* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((cairo_path_t* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCCairoPath"));
  return(fun(s_path));
} 

cairo_glyph_t*
asCCairoGlyph(USER_OBJECT_ s_glyph)
{
  static cairo_glyph_t* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((cairo_glyph_t* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCCairoGlyph"));
  return(fun(s_glyph));
} 

#if CAIRO_CHECK_VERSION(1, 4, 0)
USER_OBJECT_
asRCairoRectangle(cairo_rectangle_t* path)
{
  static USER_OBJECT_ (*fun)(cairo_rectangle_t*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(cairo_rectangle_t*))R_GetCCallable("RGtk2", "asRCairoRectangle"));
  return(fun(path));
}
#endif 

#if CAIRO_CHECK_VERSION(1, 4, 0)
USER_OBJECT_
asRCairoRectangleList(cairo_rectangle_list_t* list)
{
  static USER_OBJECT_ (*fun)(cairo_rectangle_list_t*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(cairo_rectangle_list_t*))R_GetCCallable("RGtk2", "asRCairoRectangleList"));
  return(fun(list));
}
#endif 

#endif
