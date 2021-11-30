#ifndef S_GIO_USERFUNCS_H
#define S_GIO_USERFUNCS_H
#include <RGtk2/gobject.h>
#include <RGtk2/gio.h>


#if GIO_CHECK_VERSION(2, 16, 0)
  gboolean
S_GIOSchedulerJobFunc(GIOSchedulerJob* s_job, GCancellable* s_cancellable, gpointer s_user_data);
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
  gboolean
S_GSimpleAsyncThreadFunc(GSimpleAsyncResult* s_res, GObject* s_object, GCancellable* s_cancellable);
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
  void
S_GAsyncReadyCallback(GObject* s_source_object, GSimpleAsyncResult* s_res, gpointer s_user_data);
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
  void
S_GFileProgressCallback(goffset s_current_num_bytes, goffset s_total_num_bytes, gpointer s_user_data);
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
  void
S_GFileReadMoreCallback(const char* s_file_contents, goffset s_file_size, gpointer s_callback_data);
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
  gpointer
S_GReallocFunc(gpointer s_data, gsize s_size);
#endif 

#endif
