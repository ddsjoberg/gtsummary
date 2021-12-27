void
S_GdkFilterFunc(GdkXEvent* xevent, GdkEvent* event, gpointer data)
{
  static void (*fun)(GdkXEvent*, GdkEvent*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GdkXEvent*, GdkEvent*, gpointer))R_GetCCallable("RGtk2", "S_GdkFilterFunc"));
  return(fun(xevent, event, data));
} 

void
S_GdkEventFunc(GdkEvent* event, gpointer data)
{
  static void (*fun)(GdkEvent*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GdkEvent*, gpointer))R_GetCCallable("RGtk2", "S_GdkEventFunc"));
  return(fun(event, data));
} 

gboolean
S_GdkPixbufSaveFunc(const guchar* buf, gsize count, GError** error, gpointer data)
{
  static gboolean (*fun)(const guchar*, gsize, GError**, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(const guchar*, gsize, GError**, gpointer))R_GetCCallable("RGtk2", "S_GdkPixbufSaveFunc"));
  return(fun(buf, count, error, data));
} 

void
S_GdkSpanFunc(GdkSpan* span, gpointer data)
{
  static void (*fun)(GdkSpan*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GdkSpan*, gpointer))R_GetCCallable("RGtk2", "S_GdkSpanFunc"));
  return(fun(span, data));
} 

