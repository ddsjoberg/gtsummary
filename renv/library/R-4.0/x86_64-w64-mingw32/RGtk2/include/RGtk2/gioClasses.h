#ifndef S_GIO_CLASSES_H
#define S_GIO_CLASSES_H
#include <RGtk2/gobject.h>
#include <RGtk2/gio.h>

#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gapp_launch_context_class_init(GAppLaunchContextClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gcancellable_class_init(GCancellableClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfilename_completer_class_init(GFilenameCompleterClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_enumerator_class_init(GFileEnumeratorClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_monitor_class_init(GFileMonitorClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_ginput_stream_class_init(GInputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_input_stream_class_init(GFileInputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfilter_input_stream_class_init(GFilterInputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gbuffered_input_stream_class_init(GBufferedInputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gdata_input_stream_class_init(GDataInputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmemory_input_stream_class_init(GMemoryInputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmount_operation_class_init(GMountOperationClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_goutput_stream_class_init(GOutputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmemory_output_stream_class_init(GMemoryOutputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfilter_output_stream_class_init(GFilterOutputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gbuffered_output_stream_class_init(GBufferedOutputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gdata_output_stream_class_init(GDataOutputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_output_stream_class_init(GFileOutputStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gvfs_class_init(GVfsClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gvolume_monitor_class_init(GVolumeMonitorClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gnative_volume_monitor_class_init(GNativeVolumeMonitorClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gfile_iostream_class_init(GFileIOStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_ginet_address_class_init(GInetAddressClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gnetwork_address_class_init(GNetworkAddressClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gnetwork_service_class_init(GNetworkServiceClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gresolver_class_init(GResolverClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_class_init(GSocketClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_address_class_init(GSocketAddressClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_address_enumerator_class_init(GSocketAddressEnumeratorClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_client_class_init(GSocketClientClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_connection_class_init(GSocketConnectionClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_control_message_class_init(GSocketControlMessageClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_listener_class_init(GSocketListenerClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_service_class_init(GSocketServiceClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gtcp_connection_class_init(GTcpConnectionClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gthreaded_socket_service_class_init(GThreadedSocketServiceClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_giostream_class_init(GIOStreamClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_ginet_socket_address_class_init(GInetSocketAddressClass * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gapp_info_class_init(GAppInfoIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gasync_result_class_init(GAsyncResultIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gdrive_class_init(GDriveIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gfile_class_init(GFileIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gicon_class_init(GIconIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gloadable_icon_class_init(GLoadableIconIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gmount_class_init(GMountIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gseekable_class_init(GSeekableIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 16, 0)
void
S_gvolume_class_init(GVolumeIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gasync_initable_class_init(GAsyncInitableIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_ginitable_class_init(GInitableIface * c, SEXP e);
#endif 
#if GIO_CHECK_VERSION(2, 22, 0)
void
S_gsocket_connectable_class_init(GSocketConnectableIface * c, SEXP e);
#endif 
#endif
