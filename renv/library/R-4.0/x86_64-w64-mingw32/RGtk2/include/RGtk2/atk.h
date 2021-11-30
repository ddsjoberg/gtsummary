#ifndef RGTK2_ATK_H
#define RGTK2_ATK_H

#include <RGtk2/gobject.h>

#include <atk/atk.h>
#include <atk/atk-enum-types.h>

/* Unlike the other libraries, ATK did not always provide version information,
   so we assume 1.10.0 when the macro is not present.
*/
#ifndef ATK_CHECK_VERSION
#define ATK_CHECK_VERSION(major,minor,micro)    \
    (1 > (major) || \
     (1 == (major) && 10 > (minor)) || \
     (1 == (major) && 10 == (minor) && \
      0 >= (micro)))
#endif

#include <RGtk2/atkUserFuncs.h>
#include <RGtk2/atkClasses.h>

/**** Conversion ****/

AtkAttributeSet* asCAtkAttributeSet(USER_OBJECT_ s_set);
AtkAttribute* asCAtkAttribute(USER_OBJECT_ s_attr);
USER_OBJECT_ asRAtkAttributeSet(AtkAttributeSet* set);
USER_OBJECT_ asRAtkAttribute(AtkAttribute* attr);
AtkTextRectangle* asCAtkTextRectangle(USER_OBJECT_ s_rect);
USER_OBJECT_ asRAtkTextRectangle(AtkTextRectangle *rect);
USER_OBJECT_ asRAtkTextRange(AtkTextRange *range);
AtkTextRange *asCAtkTextRange(USER_OBJECT_ s_obj);
USER_OBJECT_ asRAtkKeyEventStruct(AtkKeyEventStruct * obj);
AtkRectangle* asCAtkRectangle(USER_OBJECT_ s_rect);
USER_OBJECT_ asRAtkRectangle(AtkRectangle *rect);

#endif
