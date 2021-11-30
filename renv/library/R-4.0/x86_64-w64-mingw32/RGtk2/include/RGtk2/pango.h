#ifndef RGTK2_PANGO_H
#define RGTK2_PANGO_H

#include <RGtk2/gobject.h>
#include <RGtk2/cairo.h>

#define PANGO_ENABLE_BACKEND
#include <pango/pango.h>
#include <pango/pangocairo.h>

/* Pango got version macros in 1.16.0; before that, assume 1.10.0 */
#ifndef PANGO_VERSION_CHECK
#define PANGO_CHECK_VERSION(major,minor,micro)    \
    (1 > (major) || \
     (1 == (major) && 10 > (minor)) || \
     (1 == (major) && 10 == (minor) && \
      0 >= (micro)))
#else
#define PANGO_CHECK_VERSION PANGO_VERSION_CHECK
#endif

#include <RGtk2/pangoClasses.h>
#include <RGtk2/pangoUserFuncs.h>

/**** Conversion *****/

PangoRectangle* asCPangoRectangle(USER_OBJECT_ s_rect);
USER_OBJECT_ asRPangoRectangle(PangoRectangle *rect);
USER_OBJECT_ asRPangoAttribute(PangoAttribute *attr);
USER_OBJECT_ asRPangoAttributeCopy(PangoAttribute *attr);
USER_OBJECT_ toRPangoAttribute(PangoAttribute *attr, gboolean finalize);

#endif
