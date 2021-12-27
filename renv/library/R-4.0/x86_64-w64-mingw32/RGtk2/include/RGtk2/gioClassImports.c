#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gapp_launch_context_class_init(GAppLaunchContextClass * c, SEXP e)
{
  static void (*fun)(GAppLaunchContextClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GAppLaunchContextClass *, SEXP))R_GetCCallable("RGtk2", "S_gapp_launch_context_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gcancellable_class_init(GCancellableClass * c, SEXP e)
{
  static void (*fun)(GCancellableClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GCancellableClass *, SEXP))R_GetCCallable("RGtk2", "S_gcancellable_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfilename_completer_class_init(GFilenameCompleterClass * c, SEXP e)
{
  static void (*fun)(GFilenameCompleterClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFilenameCompleterClass *, SEXP))R_GetCCallable("RGtk2", "S_gfilename_completer_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_enumerator_class_init(GFileEnumeratorClass * c, SEXP e)
{
  static void (*fun)(GFileEnumeratorClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFileEnumeratorClass *, SEXP))R_GetCCallable("RGtk2", "S_gfile_enumerator_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_monitor_class_init(GFileMonitorClass * c, SEXP e)
{
  static void (*fun)(GFileMonitorClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFileMonitorClass *, SEXP))R_GetCCallable("RGtk2", "S_gfile_monitor_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_ginput_stream_class_init(GInputStreamClass * c, SEXP e)
{
  static void (*fun)(GInputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GInputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_ginput_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_input_stream_class_init(GFileInputStreamClass * c, SEXP e)
{
  static void (*fun)(GFileInputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFileInputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gfile_input_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfilter_input_stream_class_init(GFilterInputStreamClass * c, SEXP e)
{
  static void (*fun)(GFilterInputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFilterInputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gfilter_input_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gbuffered_input_stream_class_init(GBufferedInputStreamClass * c, SEXP e)
{
  static void (*fun)(GBufferedInputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GBufferedInputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gbuffered_input_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gdata_input_stream_class_init(GDataInputStreamClass * c, SEXP e)
{
  static void (*fun)(GDataInputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GDataInputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gdata_input_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmemory_input_stream_class_init(GMemoryInputStreamClass * c, SEXP e)
{
  static void (*fun)(GMemoryInputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GMemoryInputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gmemory_input_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmount_operation_class_init(GMountOperationClass * c, SEXP e)
{
  static void (*fun)(GMountOperationClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GMountOperationClass *, SEXP))R_GetCCallable("RGtk2", "S_gmount_operation_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_goutput_stream_class_init(GOutputStreamClass * c, SEXP e)
{
  static void (*fun)(GOutputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GOutputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_goutput_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmemory_output_stream_class_init(GMemoryOutputStreamClass * c, SEXP e)
{
  static void (*fun)(GMemoryOutputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GMemoryOutputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gmemory_output_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfilter_output_stream_class_init(GFilterOutputStreamClass * c, SEXP e)
{
  static void (*fun)(GFilterOutputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFilterOutputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gfilter_output_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gbuffered_output_stream_class_init(GBufferedOutputStreamClass * c, SEXP e)
{
  static void (*fun)(GBufferedOutputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GBufferedOutputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gbuffered_output_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gdata_output_stream_class_init(GDataOutputStreamClass * c, SEXP e)
{
  static void (*fun)(GDataOutputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GDataOutputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gdata_output_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_output_stream_class_init(GFileOutputStreamClass * c, SEXP e)
{
  static void (*fun)(GFileOutputStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFileOutputStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gfile_output_stream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gvfs_class_init(GVfsClass * c, SEXP e)
{
  static void (*fun)(GVfsClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GVfsClass *, SEXP))R_GetCCallable("RGtk2", "S_gvfs_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gvolume_monitor_class_init(GVolumeMonitorClass * c, SEXP e)
{
  static void (*fun)(GVolumeMonitorClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GVolumeMonitorClass *, SEXP))R_GetCCallable("RGtk2", "S_gvolume_monitor_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gnative_volume_monitor_class_init(GNativeVolumeMonitorClass * c, SEXP e)
{
  static void (*fun)(GNativeVolumeMonitorClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GNativeVolumeMonitorClass *, SEXP))R_GetCCallable("RGtk2", "S_gnative_volume_monitor_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gfile_iostream_class_init(GFileIOStreamClass * c, SEXP e)
{
  static void (*fun)(GFileIOStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFileIOStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_gfile_iostream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_ginet_address_class_init(GInetAddressClass * c, SEXP e)
{
  static void (*fun)(GInetAddressClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GInetAddressClass *, SEXP))R_GetCCallable("RGtk2", "S_ginet_address_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gnetwork_address_class_init(GNetworkAddressClass * c, SEXP e)
{
  static void (*fun)(GNetworkAddressClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GNetworkAddressClass *, SEXP))R_GetCCallable("RGtk2", "S_gnetwork_address_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gnetwork_service_class_init(GNetworkServiceClass * c, SEXP e)
{
  static void (*fun)(GNetworkServiceClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GNetworkServiceClass *, SEXP))R_GetCCallable("RGtk2", "S_gnetwork_service_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gresolver_class_init(GResolverClass * c, SEXP e)
{
  static void (*fun)(GResolverClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GResolverClass *, SEXP))R_GetCCallable("RGtk2", "S_gresolver_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_class_init(GSocketClass * c, SEXP e)
{
  static void (*fun)(GSocketClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_address_class_init(GSocketAddressClass * c, SEXP e)
{
  static void (*fun)(GSocketAddressClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketAddressClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_address_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_address_enumerator_class_init(GSocketAddressEnumeratorClass * c, SEXP e)
{
  static void (*fun)(GSocketAddressEnumeratorClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketAddressEnumeratorClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_address_enumerator_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_client_class_init(GSocketClientClass * c, SEXP e)
{
  static void (*fun)(GSocketClientClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketClientClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_client_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_connection_class_init(GSocketConnectionClass * c, SEXP e)
{
  static void (*fun)(GSocketConnectionClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketConnectionClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_connection_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_control_message_class_init(GSocketControlMessageClass * c, SEXP e)
{
  static void (*fun)(GSocketControlMessageClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketControlMessageClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_control_message_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_listener_class_init(GSocketListenerClass * c, SEXP e)
{
  static void (*fun)(GSocketListenerClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketListenerClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_listener_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_service_class_init(GSocketServiceClass * c, SEXP e)
{
  static void (*fun)(GSocketServiceClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketServiceClass *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_service_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gtcp_connection_class_init(GTcpConnectionClass * c, SEXP e)
{
  static void (*fun)(GTcpConnectionClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GTcpConnectionClass *, SEXP))R_GetCCallable("RGtk2", "S_gtcp_connection_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gthreaded_socket_service_class_init(GThreadedSocketServiceClass * c, SEXP e)
{
  static void (*fun)(GThreadedSocketServiceClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GThreadedSocketServiceClass *, SEXP))R_GetCCallable("RGtk2", "S_gthreaded_socket_service_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_giostream_class_init(GIOStreamClass * c, SEXP e)
{
  static void (*fun)(GIOStreamClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GIOStreamClass *, SEXP))R_GetCCallable("RGtk2", "S_giostream_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_ginet_socket_address_class_init(GInetSocketAddressClass * c, SEXP e)
{
  static void (*fun)(GInetSocketAddressClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GInetSocketAddressClass *, SEXP))R_GetCCallable("RGtk2", "S_ginet_socket_address_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gapp_info_class_init(GAppInfoIface * c, SEXP e)
{
  static void (*fun)(GAppInfoIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GAppInfoIface *, SEXP))R_GetCCallable("RGtk2", "S_gapp_info_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gasync_result_class_init(GAsyncResultIface * c, SEXP e)
{
  static void (*fun)(GAsyncResultIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GAsyncResultIface *, SEXP))R_GetCCallable("RGtk2", "S_gasync_result_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gdrive_class_init(GDriveIface * c, SEXP e)
{
  static void (*fun)(GDriveIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GDriveIface *, SEXP))R_GetCCallable("RGtk2", "S_gdrive_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_class_init(GFileIface * c, SEXP e)
{
  static void (*fun)(GFileIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GFileIface *, SEXP))R_GetCCallable("RGtk2", "S_gfile_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gicon_class_init(GIconIface * c, SEXP e)
{
  static void (*fun)(GIconIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GIconIface *, SEXP))R_GetCCallable("RGtk2", "S_gicon_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gloadable_icon_class_init(GLoadableIconIface * c, SEXP e)
{
  static void (*fun)(GLoadableIconIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GLoadableIconIface *, SEXP))R_GetCCallable("RGtk2", "S_gloadable_icon_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmount_class_init(GMountIface * c, SEXP e)
{
  static void (*fun)(GMountIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GMountIface *, SEXP))R_GetCCallable("RGtk2", "S_gmount_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gseekable_class_init(GSeekableIface * c, SEXP e)
{
  static void (*fun)(GSeekableIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSeekableIface *, SEXP))R_GetCCallable("RGtk2", "S_gseekable_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gvolume_class_init(GVolumeIface * c, SEXP e)
{
  static void (*fun)(GVolumeIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GVolumeIface *, SEXP))R_GetCCallable("RGtk2", "S_gvolume_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gasync_initable_class_init(GAsyncInitableIface * c, SEXP e)
{
  static void (*fun)(GAsyncInitableIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GAsyncInitableIface *, SEXP))R_GetCCallable("RGtk2", "S_gasync_initable_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_ginitable_class_init(GInitableIface * c, SEXP e)
{
  static void (*fun)(GInitableIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GInitableIface *, SEXP))R_GetCCallable("RGtk2", "S_ginitable_class_init"));
  return(fun(c, e));
}
#endif 

#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_connectable_class_init(GSocketConnectableIface * c, SEXP e)
{
  static void (*fun)(GSocketConnectableIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GSocketConnectableIface *, SEXP))R_GetCCallable("RGtk2", "S_gsocket_connectable_class_init"));
  return(fun(c, e));
}
#endif 

