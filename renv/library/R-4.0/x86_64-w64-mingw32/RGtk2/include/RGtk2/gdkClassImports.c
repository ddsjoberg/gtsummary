void
S_gdk_bitmap_class_init(GdkDrawableClass * c, SEXP e)
{
  static void (*fun)(GdkDrawableClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkDrawableClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_bitmap_class_init"));
  return(fun(c, e));
} 

void
S_gdk_colormap_class_init(GdkColormapClass * c, SEXP e)
{
  static void (*fun)(GdkColormapClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkColormapClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_colormap_class_init"));
  return(fun(c, e));
} 

void
S_gdk_display_class_init(GdkDisplayClass * c, SEXP e)
{
  static void (*fun)(GdkDisplayClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkDisplayClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_display_class_init"));
  return(fun(c, e));
} 

void
S_gdk_display_manager_class_init(GdkDisplayManagerClass * c, SEXP e)
{
  static void (*fun)(GdkDisplayManagerClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkDisplayManagerClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_display_manager_class_init"));
  return(fun(c, e));
} 

void
S_gdk_drag_context_class_init(GdkDragContextClass * c, SEXP e)
{
  static void (*fun)(GdkDragContextClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkDragContextClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_drag_context_class_init"));
  return(fun(c, e));
} 

void
S_gdk_drawable_class_init(GdkDrawableClass * c, SEXP e)
{
  static void (*fun)(GdkDrawableClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkDrawableClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_drawable_class_init"));
  return(fun(c, e));
} 

void
S_gdk_window_class_init(GdkWindowClass * c, SEXP e)
{
  static void (*fun)(GdkWindowClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkWindowClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_window_class_init"));
  return(fun(c, e));
} 

void
S_gdk_pixmap_class_init(GdkPixmapObjectClass * c, SEXP e)
{
  static void (*fun)(GdkPixmapObjectClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkPixmapObjectClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_pixmap_class_init"));
  return(fun(c, e));
} 

void
S_gdk_gc_class_init(GdkGCClass * c, SEXP e)
{
  static void (*fun)(GdkGCClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkGCClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_gc_class_init"));
  return(fun(c, e));
} 

void
S_gdk_image_class_init(GdkImageClass * c, SEXP e)
{
  static void (*fun)(GdkImageClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkImageClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_image_class_init"));
  return(fun(c, e));
} 

void
S_gdk_keymap_class_init(GdkKeymapClass * c, SEXP e)
{
  static void (*fun)(GdkKeymapClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkKeymapClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_keymap_class_init"));
  return(fun(c, e));
} 

void
S_gdk_pixbuf_animation_class_init(GdkPixbufAnimationClass * c, SEXP e)
{
  static void (*fun)(GdkPixbufAnimationClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkPixbufAnimationClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_pixbuf_animation_class_init"));
  return(fun(c, e));
} 

void
S_gdk_pixbuf_animation_iter_class_init(GdkPixbufAnimationIterClass * c, SEXP e)
{
  static void (*fun)(GdkPixbufAnimationIterClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkPixbufAnimationIterClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_pixbuf_animation_iter_class_init"));
  return(fun(c, e));
} 

void
S_gdk_pixbuf_loader_class_init(GdkPixbufLoaderClass * c, SEXP e)
{
  static void (*fun)(GdkPixbufLoaderClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkPixbufLoaderClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_pixbuf_loader_class_init"));
  return(fun(c, e));
} 

void
S_gdk_pango_renderer_class_init(GdkPangoRendererClass * c, SEXP e)
{
  static void (*fun)(GdkPangoRendererClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkPangoRendererClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_pango_renderer_class_init"));
  return(fun(c, e));
} 

void
S_gdk_screen_class_init(GdkScreenClass * c, SEXP e)
{
  static void (*fun)(GdkScreenClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkScreenClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_screen_class_init"));
  return(fun(c, e));
} 

#if GDK_CHECK_VERSION(2, 14, 0)
void
S_gdk_app_launch_context_class_init(GdkAppLaunchContextClass * c, SEXP e)
{
  static void (*fun)(GdkAppLaunchContextClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(GdkAppLaunchContextClass *, SEXP))R_GetCCallable("RGtk2", "S_gdk_app_launch_context_class_init"));
  return(fun(c, e));
}
#endif 

