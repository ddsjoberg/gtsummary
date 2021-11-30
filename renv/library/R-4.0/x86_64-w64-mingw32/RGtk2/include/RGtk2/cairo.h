#ifndef RGTK2_CAIRO_H
#define RGTK2_CAIRO_H

/* Note: cairo itself is not based on GObject, but we use our own GEnum-based
   wrappers for the cairo enums/flags plus other things linked to GObject
*/
#include <RGtk2/gobject.h>

#include <cairo.h>

#define CAIRO_CHECK_VERSION(major, minor, micro) \
  (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(major, minor, micro))

/* for cairo 1.2, ps/pdf backend is required for GTK, svg comes 'free',
   so we can safely depend on these */
#if CAIRO_CHECK_VERSION(1,2,0)
#include <cairo-ps.h>
#include <cairo-pdf.h>
#include <cairo-svg.h>
#endif

/* custom GEnum wrappers so that we can use the same routines for cairo */
#include <RGtk2/cairo-enums.h>

#include <RGtk2/cairoUserFuncs.h>

/****** Manual UserFuncs *****/

cairo_status_t S_cairo_read_func_t(gpointer s_closure, guchar* s_data, guint s_length);

/****** Conversion ******/

#define toRPointerWithCairoRef(ptr, name, type) \
__extension__ \
({ \
	type ## _reference(ptr); \
	toRPointerWithFinalizer(ptr, name, (RPointerFinalizer) type ## _destroy); \
})

USER_OBJECT_ asRCairoPath(cairo_path_t *path);
cairo_path_t * asCCairoPath(USER_OBJECT_ s_path);
cairo_glyph_t * asCCairoGlyph(USER_OBJECT_ s_glyph);
USER_OBJECT_ asRCairoGlyph(cairo_glyph_t * obj);
#if CAIRO_CHECK_VERSION(1,4,0)
USER_OBJECT_ asRCairoRectangle(cairo_rectangle_t *rect);
USER_OBJECT_ asRCairoRectangleList(cairo_rectangle_list_t *list);
#endif
#if CAIRO_CHECK_VERSION(1,8,0)
cairo_text_cluster_t *asCCairoTextCluster(USER_OBJECT_ s_obj);
USER_OBJECT_ asRCairoTextCluster(cairo_text_cluster_t * obj);
cairo_font_extents_t *asCCairoFontExtents(USER_OBJECT_ s_obj);
USER_OBJECT_ asRCairoFontExtents(cairo_font_extents_t * obj);
#endif
#endif
