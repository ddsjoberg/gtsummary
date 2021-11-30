#if GIO_CHECK_VERSION(2, 16, 0)
gboolean
S_GIOSchedulerJobFunc(GIOSchedulerJob* job, GCancellable* cancellable, gpointer user_data)
{
  static gboolean (*fun)(GIOSchedulerJob*, GCancellable*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(GIOSchedulerJob*, GCancellable*, gpointer))R_GetCCallable("RGtk2", "S_GIOSchedulerJobFunc"));
  return(fun(job, cancellable, user_data));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
gboolean
S_GSimpleAsyncThreadFunc(GSimpleAsyncResult* res, GObject* object, GCancellable* cancellable)
{
  static gboolean (*fun)(GSimpleAsyncResult*, GObject*, GCancellable*) = NULL;
  if(!fun) fun = ((gboolean (*)(GSimpleAsyncResult*, GObject*, GCancellable*))R_GetCCallable("RGtk2", "S_GSimpleAsyncThreadFunc"));
  return(fun(res, object, cancellable));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_GAsyncReadyCallback(GObject* source_object, GSimpleAsyncResult* res, gpointer user_data)
{
  static void (*fun)(GObject*, GSimpleAsyncResult*, gpointer) = NULL;
  if(!fun) fun = ((void (*)(GObject*, GSimpleAsyncResult*, gpointer))R_GetCCallable("RGtk2", "S_GAsyncReadyCallback"));
  return(fun(source_object, res, user_data));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_GFileProgressCallback(goffset current_num_bytes, goffset total_num_bytes, gpointer user_data)
{
  static void (*fun)(goffset, goffset, gpointer) = NULL;
  if(!fun) fun = ((void (*)(goffset, goffset, gpointer))R_GetCCallable("RGtk2", "S_GFileProgressCallback"));
  return(fun(current_num_bytes, total_num_bytes, user_data));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_GFileReadMoreCallback(const char* file_contents, goffset file_size, gpointer callback_data)
{
  static void (*fun)(const char*, goffset, gpointer) = NULL;
  if(!fun) fun = ((void (*)(const char*, goffset, gpointer))R_GetCCallable("RGtk2", "S_GFileReadMoreCallback"));
  return(fun(file_contents, file_size, callback_data));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
gpointer
S_GReallocFunc(gpointer data, gsize size)
{
  static gpointer (*fun)(gpointer, gsize) = NULL;
  if(!fun) fun = ((gpointer (*)(gpointer, gsize))R_GetCCallable("RGtk2", "S_GReallocFunc"));
  return(fun(data, size));
}
#endif 

