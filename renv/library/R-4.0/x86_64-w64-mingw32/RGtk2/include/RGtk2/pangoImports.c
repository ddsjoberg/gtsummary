#ifndef S_PANGO_IMPORTS_C
#define S_PANGO_IMPORTS_C
#include <RGtk2/pango.h>

#include <RGtk2/cairoImports.c>

#include <RGtk2/pangoUserFuncImports.c>

#include <RGtk2/pangoClassImports.c>

PangoRectangle*
asCPangoRectangle(USER_OBJECT_ s_rect)
{
  static PangoRectangle* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((PangoRectangle* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCPangoRectangle"));
  return(fun(s_rect));
} 

USER_OBJECT_
asRPangoRectangle(PangoRectangle* rect)
{
  static USER_OBJECT_ (*fun)(PangoRectangle*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(PangoRectangle*))R_GetCCallable("RGtk2", "asRPangoRectangle"));
  return(fun(rect));
} 

USER_OBJECT_
asRPangoAttribute(PangoAttribute* attr)
{
  static USER_OBJECT_ (*fun)(PangoAttribute*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(PangoAttribute*))R_GetCCallable("RGtk2", "asRPangoAttribute"));
  return(fun(attr));
} 

USER_OBJECT_
asRPangoAttributeCopy(PangoAttribute* attr)
{
  static USER_OBJECT_ (*fun)(PangoAttribute*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(PangoAttribute*))R_GetCCallable("RGtk2", "asRPangoAttributeCopy"));
  return(fun(attr));
} 

USER_OBJECT_
toRPangoAttribute(PangoAttribute* attr, gboolean finalize)
{
  static USER_OBJECT_ (*fun)(PangoAttribute*, gboolean) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(PangoAttribute*, gboolean))R_GetCCallable("RGtk2", "toRPangoAttribute"));
  return(fun(attr, finalize));
} 

#endif
