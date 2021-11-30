#ifndef S_ATK_IMPORTS_C
#define S_ATK_IMPORTS_C
#include <RGtk2/atk.h>

#include <RGtk2/gobjectImports.c>

#include <RGtk2/atkUserFuncImports.c>

#include <RGtk2/atkClassImports.c>

AtkAttributeSet*
asCAtkAttributeSet(USER_OBJECT_ s_set)
{
  static AtkAttributeSet* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((AtkAttributeSet* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCAtkAttributeSet"));
  return(fun(s_set));
} 

AtkAttribute*
asCAtkAttribute(USER_OBJECT_ s_attr)
{
  static AtkAttribute* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((AtkAttribute* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCAtkAttribute"));
  return(fun(s_attr));
} 

USER_OBJECT_
asRAtkAttributeSet(AtkAttributeSet* set)
{
  static USER_OBJECT_ (*fun)(AtkAttributeSet*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(AtkAttributeSet*))R_GetCCallable("RGtk2", "asRAtkAttributeSet"));
  return(fun(set));
} 

USER_OBJECT_
asRAtkAttribute(AtkAttribute* attr)
{
  static USER_OBJECT_ (*fun)(AtkAttribute*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(AtkAttribute*))R_GetCCallable("RGtk2", "asRAtkAttribute"));
  return(fun(attr));
} 

AtkTextRectangle*
asCAtkTextRectangle(USER_OBJECT_ s_rect)
{
  static AtkTextRectangle* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((AtkTextRectangle* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCAtkTextRectangle"));
  return(fun(s_rect));
} 

USER_OBJECT_
asRAtkTextRectangle(AtkTextRectangle* rect)
{
  static USER_OBJECT_ (*fun)(AtkTextRectangle*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(AtkTextRectangle*))R_GetCCallable("RGtk2", "asRAtkTextRectangle"));
  return(fun(rect));
} 

USER_OBJECT_
asRAtkTextRange(AtkTextRange* range)
{
  static USER_OBJECT_ (*fun)(AtkTextRange*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(AtkTextRange*))R_GetCCallable("RGtk2", "asRAtkTextRange"));
  return(fun(range));
} 

AtkTextRange*
asCAtkTextRange(USER_OBJECT_ s_obj)
{
  static AtkTextRange* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((AtkTextRange* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCAtkTextRange"));
  return(fun(s_obj));
} 

USER_OBJECT_
asRAtkKeyEventStruct(AtkKeyEventStruct* obj)
{
  static USER_OBJECT_ (*fun)(AtkKeyEventStruct*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(AtkKeyEventStruct*))R_GetCCallable("RGtk2", "asRAtkKeyEventStruct"));
  return(fun(obj));
} 

AtkRectangle*
asCAtkRectangle(USER_OBJECT_ s_rect)
{
  static AtkRectangle* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((AtkRectangle* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCAtkRectangle"));
  return(fun(s_rect));
} 

USER_OBJECT_
asRAtkRectangle(AtkRectangle* rect)
{
  static USER_OBJECT_ (*fun)(AtkRectangle*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(AtkRectangle*))R_GetCCallable("RGtk2", "asRAtkRectangle"));
  return(fun(rect));
} 

#endif
