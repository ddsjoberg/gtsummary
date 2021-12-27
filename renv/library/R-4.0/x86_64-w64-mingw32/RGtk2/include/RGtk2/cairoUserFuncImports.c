cairo_status_t
S_cairo_write_func_t(gpointer closure, const guchar* data, guint length)
{
  static cairo_status_t (*fun)(gpointer, const guchar*, guint) = NULL;
  if(!fun) fun = ((cairo_status_t (*)(gpointer, const guchar*, guint))R_GetCCallable("RGtk2", "S_cairo_write_func_t"));
  return(fun(closure, data, length));
} 

