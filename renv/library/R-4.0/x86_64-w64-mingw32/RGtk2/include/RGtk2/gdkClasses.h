#ifndef S_GDK_CLASSES_H
#define S_GDK_CLASSES_H
#include <RGtk2/gobject.h>
#include <RGtk2/gdk.h>

void
S_gdk_bitmap_class_init(GdkDrawableClass * c, SEXP e); 
void
S_gdk_colormap_class_init(GdkColormapClass * c, SEXP e); 
void
S_gdk_display_class_init(GdkDisplayClass * c, SEXP e); 
void
S_gdk_display_manager_class_init(GdkDisplayManagerClass * c, SEXP e); 
void
S_gdk_drag_context_class_init(GdkDragContextClass * c, SEXP e); 
void
S_gdk_drawable_class_init(GdkDrawableClass * c, SEXP e); 
void
S_gdk_window_class_init(GdkWindowClass * c, SEXP e); 
void
S_gdk_pixmap_class_init(GdkPixmapObjectClass * c, SEXP e); 
void
S_gdk_gc_class_init(GdkGCClass * c, SEXP e); 
void
S_gdk_image_class_init(GdkImageClass * c, SEXP e); 
void
S_gdk_keymap_class_init(GdkKeymapClass * c, SEXP e); 
void
S_gdk_pixbuf_animation_class_init(GdkPixbufAnimationClass * c, SEXP e); 
void
S_gdk_pixbuf_animation_iter_class_init(GdkPixbufAnimationIterClass * c, SEXP e); 
void
S_gdk_pixbuf_loader_class_init(GdkPixbufLoaderClass * c, SEXP e); 
void
S_gdk_pango_renderer_class_init(GdkPangoRendererClass * c, SEXP e); 
void
S_gdk_screen_class_init(GdkScreenClass * c, SEXP e); 
#if GDK_CHECK_VERSION(2, 14, 0)
void
S_gdk_app_launch_context_class_init(GdkAppLaunchContextClass * c, SEXP e);
#endif 
#endif
