#ifndef S_GOBJECT_IMPORTS_C
#define S_GOBJECT_IMPORTS_C
#include <R_ext/Rdynload.h>

#include <RGtk2/gobject.h>

gchar**
asCStringArray(USER_OBJECT_ svec)
{
  static gchar** (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((gchar** (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCStringArray"));
  return(fun(svec));
} 

const gchar*
asCString(USER_OBJECT_ s_str)
{
  static const gchar* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((const gchar* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCString"));
  return(fun(s_str));
} 

gchar
asCCharacter(USER_OBJECT_ s_char)
{
  static gchar (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((gchar (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCCharacter"));
  return(fun(s_char));
} 

USER_OBJECT_
asRUnsigned(guint num)
{
  static USER_OBJECT_ (*fun)(guint) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(guint))R_GetCCallable("RGtk2", "asRUnsigned"));
  return(fun(num));
} 

USER_OBJECT_
asRCharacter(gchar c)
{
  static USER_OBJECT_ (*fun)(gchar) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(gchar))R_GetCCallable("RGtk2", "asRCharacter"));
  return(fun(c));
} 

USER_OBJECT_
asRString(const gchar* str)
{
  static USER_OBJECT_ (*fun)(const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(const gchar*))R_GetCCallable("RGtk2", "asRString"));
  return(fun(str));
} 

USER_OBJECT_
asREnum(int value, GType etype)
{
  static USER_OBJECT_ (*fun)(int, GType) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(int, GType))R_GetCCallable("RGtk2", "asREnum"));
  return(fun(value, etype));
} 

USER_OBJECT_
asRFlag(guint value, GType ftype)
{
  static USER_OBJECT_ (*fun)(guint, GType) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(guint, GType))R_GetCCallable("RGtk2", "asRFlag"));
  return(fun(value, ftype));
} 

guint
asCFlag(USER_OBJECT_ s_flag, GType ftype)
{
  static guint (*fun)(USER_OBJECT_, GType) = NULL;
  if(!fun) fun = ((guint (*)(USER_OBJECT_, GType))R_GetCCallable("RGtk2", "asCFlag"));
  return(fun(s_flag, ftype));
} 

gint
asCEnum(USER_OBJECT_ s_enum, GType etype)
{
  static gint (*fun)(USER_OBJECT_, GType) = NULL;
  if(!fun) fun = ((gint (*)(USER_OBJECT_, GType))R_GetCCallable("RGtk2", "asCEnum"));
  return(fun(s_enum, etype));
} 

USER_OBJECT_
toRPointerWithFinalizer(gconstpointer val, const gchar* typeName, RPointerFinalizer finalizer)
{
  static USER_OBJECT_ (*fun)(gconstpointer, const gchar*, RPointerFinalizer) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(gconstpointer, const gchar*, RPointerFinalizer))R_GetCCallable("RGtk2", "toRPointerWithFinalizer"));
  return(fun(val, typeName, finalizer));
} 

USER_OBJECT_
toRPointerWithRef(gconstpointer val, const gchar* type)
{
  static USER_OBJECT_ (*fun)(gconstpointer, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(gconstpointer, const gchar*))R_GetCCallable("RGtk2", "toRPointerWithRef"));
  return(fun(val, type));
} 

gpointer
getPtrValueWithRef(USER_OBJECT_ sval)
{
  static gpointer (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((gpointer (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "getPtrValueWithRef"));
  return(fun(sval));
} 

USER_OBJECT_
asRGQuark(GQuark val)
{
  static USER_OBJECT_ (*fun)(GQuark) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GQuark))R_GetCCallable("RGtk2", "asRGQuark"));
  return(fun(val));
} 

GTimeVal*
asCGTimeVal(USER_OBJECT_ s_timeval)
{
  static GTimeVal* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GTimeVal* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGTimeVal"));
  return(fun(s_timeval));
} 

USER_OBJECT_
asRGTimeVal(const GTimeVal* timeval)
{
  static USER_OBJECT_ (*fun)(const GTimeVal*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(const GTimeVal*))R_GetCCallable("RGtk2", "asRGTimeVal"));
  return(fun(timeval));
} 

GString*
asCGString(USER_OBJECT_ s_string)
{
  static GString* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GString* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGString"));
  return(fun(s_string));
} 

GList*
toCGList(USER_OBJECT_ s_list, gboolean dup)
{
  static GList* (*fun)(USER_OBJECT_, gboolean) = NULL;
  if(!fun) fun = ((GList* (*)(USER_OBJECT_, gboolean))R_GetCCallable("RGtk2", "toCGList"));
  return(fun(s_list, dup));
} 

USER_OBJECT_
asRGList(GList* glist, const gchar* type)
{
  static USER_OBJECT_ (*fun)(GList*, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GList*, const gchar*))R_GetCCallable("RGtk2", "asRGList"));
  return(fun(glist, type));
} 

USER_OBJECT_
asRGListWithRef(GList* gslist, const gchar* type)
{
  static USER_OBJECT_ (*fun)(GList*, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GList*, const gchar*))R_GetCCallable("RGtk2", "asRGListWithRef"));
  return(fun(gslist, type));
} 

USER_OBJECT_
asRGListWithFinalizer(GList* glist, const gchar* type, RPointerFinalizer finalizer)
{
  static USER_OBJECT_ (*fun)(GList*, const gchar*, RPointerFinalizer) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GList*, const gchar*, RPointerFinalizer))R_GetCCallable("RGtk2", "asRGListWithFinalizer"));
  return(fun(glist, type, finalizer));
} 

USER_OBJECT_
asRGListConv(GList* glist, ElementConverter converter)
{
  static USER_OBJECT_ (*fun)(GList*, ElementConverter) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GList*, ElementConverter))R_GetCCallable("RGtk2", "asRGListConv"));
  return(fun(glist, converter));
} 

GSList*
toCGSList(USER_OBJECT_ s_list, gboolean dup)
{
  static GSList* (*fun)(USER_OBJECT_, gboolean) = NULL;
  if(!fun) fun = ((GSList* (*)(USER_OBJECT_, gboolean))R_GetCCallable("RGtk2", "toCGSList"));
  return(fun(s_list, dup));
} 

USER_OBJECT_
asRGSList(GSList* gslist, const gchar* type)
{
  static USER_OBJECT_ (*fun)(GSList*, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GSList*, const gchar*))R_GetCCallable("RGtk2", "asRGSList"));
  return(fun(gslist, type));
} 

USER_OBJECT_
asRGSListWithRef(GSList* gslist, const gchar* type)
{
  static USER_OBJECT_ (*fun)(GSList*, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GSList*, const gchar*))R_GetCCallable("RGtk2", "asRGSListWithRef"));
  return(fun(gslist, type));
} 

USER_OBJECT_
asRGSListWithFinalizer(GSList* gslist, const gchar* type, RPointerFinalizer finalizer)
{
  static USER_OBJECT_ (*fun)(GSList*, const gchar*, RPointerFinalizer) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GSList*, const gchar*, RPointerFinalizer))R_GetCCallable("RGtk2", "asRGSListWithFinalizer"));
  return(fun(gslist, type, finalizer));
} 

USER_OBJECT_
asRGSListConv(GSList* gslist, ElementConverter converter)
{
  static USER_OBJECT_ (*fun)(GSList*, ElementConverter) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GSList*, ElementConverter))R_GetCCallable("RGtk2", "asRGSListConv"));
  return(fun(gslist, converter));
} 

USER_OBJECT_
asRGError(GError* error)
{
  static USER_OBJECT_ (*fun)(GError*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GError*))R_GetCCallable("RGtk2", "asRGError"));
  return(fun(error));
} 

GError*
asCGError(USER_OBJECT_ s_error)
{
  static GError* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GError* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGError"));
  return(fun(s_error));
} 

int
R_setGValueFromSValue(GValue* val, USER_OBJECT_ sval)
{
  static int (*fun)(GValue*, USER_OBJECT_) = NULL;
  if(!fun) fun = ((int (*)(GValue*, USER_OBJECT_))R_GetCCallable("RGtk2", "R_setGValueFromSValue"));
  return(fun(val, sval));
} 

GValue*
createGValueFromSValue(USER_OBJECT_ sval)
{
  static GValue* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GValue* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "createGValueFromSValue"));
  return(fun(sval));
} 

gboolean
initGValueFromSValue(USER_OBJECT_ sval, GValue* raw)
{
  static gboolean (*fun)(USER_OBJECT_, GValue*) = NULL;
  if(!fun) fun = ((gboolean (*)(USER_OBJECT_, GValue*))R_GetCCallable("RGtk2", "initGValueFromSValue"));
  return(fun(sval, raw));
} 

gboolean
initGValueFromVector(USER_OBJECT_ sval, gint n, GValue* raw)
{
  static gboolean (*fun)(USER_OBJECT_, gint, GValue*) = NULL;
  if(!fun) fun = ((gboolean (*)(USER_OBJECT_, gint, GValue*))R_GetCCallable("RGtk2", "initGValueFromVector"));
  return(fun(sval, n, raw));
} 

USER_OBJECT_
asRGValue(const GValue* val)
{
  static USER_OBJECT_ (*fun)(const GValue*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(const GValue*))R_GetCCallable("RGtk2", "asRGValue"));
  return(fun(val));
} 

GValue*
asCGValue(USER_OBJECT_ sval)
{
  static GValue* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GValue* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGValue"));
  return(fun(sval));
} 

USER_OBJECT_
asRGType(GType type)
{
  static USER_OBJECT_ (*fun)(GType) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GType))R_GetCCallable("RGtk2", "asRGType"));
  return(fun(type));
} 

GParamSpec*
asCGParamSpec(USER_OBJECT_ s_spec)
{
  static GParamSpec* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GParamSpec* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGParamSpec"));
  return(fun(s_spec));
} 

USER_OBJECT_
asRGParamSpec(GParamSpec* spec)
{
  static USER_OBJECT_ (*fun)(GParamSpec*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GParamSpec*))R_GetCCallable("RGtk2", "asRGParamSpec"));
  return(fun(spec));
} 

GClosure*
asCGClosure(USER_OBJECT_ s_closure)
{
  static GClosure* (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GClosure* (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "asCGClosure"));
  return(fun(s_closure));
} 

USER_OBJECT_
asRGClosure(GClosure* closure)
{
  static USER_OBJECT_ (*fun)(GClosure*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GClosure*))R_GetCCallable("RGtk2", "asRGClosure"));
  return(fun(closure));
} 

USER_OBJECT_
toRPointerWithSink(void* val, const char* type)
{
  static USER_OBJECT_ (*fun)(void*, const char*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(void*, const char*))R_GetCCallable("RGtk2", "toRPointerWithSink"));
  return(fun(val, type));
} 

USER_OBJECT_
asRGListWithSink(GList* glist, const gchar* type)
{
  static USER_OBJECT_ (*fun)(GList*, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GList*, const gchar*))R_GetCCallable("RGtk2", "asRGListWithSink"));
  return(fun(glist, type));
} 

USER_OBJECT_
asRGSListWithSink(GSList* gslist, const gchar* type)
{
  static USER_OBJECT_ (*fun)(GSList*, const gchar*) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GSList*, const gchar*))R_GetCCallable("RGtk2", "asRGSListWithSink"));
  return(fun(gslist, type));
} 

void
S_GCompareFunc(gconstpointer s_a, gconstpointer s_b)
{
  static void (*fun)(gconstpointer, gconstpointer) = NULL;
  if(!fun) fun = ((void (*)(gconstpointer, gconstpointer))R_GetCCallable("RGtk2", "S_GCompareFunc"));
  return(fun(s_a, s_b));
} 

gboolean
S_GSourceFunc(gpointer data)
{
  static gboolean (*fun)(gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(gpointer))R_GetCCallable("RGtk2", "S_GSourceFunc"));
  return(fun(data));
} 

GClosure*
R_createGClosure(USER_OBJECT_ s_func, USER_OBJECT_ s_data)
{
  static GClosure* (*fun)(USER_OBJECT_, USER_OBJECT_) = NULL;
  if(!fun) fun = ((GClosure* (*)(USER_OBJECT_, USER_OBJECT_))R_GetCCallable("RGtk2", "R_createGClosure"));
  return(fun(s_func, s_data));
} 

GType
r_gtk_sexp_get_type(void)
{
  static GType (*fun)() = NULL;
  if(!fun) fun = ((GType (*)())R_GetCCallable("RGtk2", "r_gtk_sexp_get_type"));
  return(fun());
} 

GType
r_gtk_param_spec_sexp_get_type(void)
{
  static GType (*fun)() = NULL;
  if(!fun) fun = ((GType (*)())R_GetCCallable("RGtk2", "r_gtk_param_spec_sexp_get_type"));
  return(fun());
} 

void
S_gobject_class_init(GObjectClass* c, USER_OBJECT_ e)
{
  static void (*fun)(GObjectClass*, USER_OBJECT_) = NULL;
  if(!fun) fun = ((void (*)(GObjectClass*, USER_OBJECT_))R_GetCCallable("RGtk2", "S_gobject_class_init"));
  return(fun(c, e));
} 

USER_OBJECT_
retByVal(USER_OBJECT_ retval, ...) {
    va_list va;
    int n = 0, i;
    USER_OBJECT_ list, names;
    va_start(va, retval);
    while(va_arg(va, void *)) n++;
    n = n / 2 + 1;
    va_end(va);

    PROTECT(list = NEW_LIST(n));
    PROTECT(names = NEW_CHARACTER(n));
    SET_VECTOR_ELT(list, 0, retval);
    SET_STRING_ELT(names, 0, COPY_TO_USER_STRING("retval"));
    va_start(va, retval);
    for (i = 1; i < n; i++) {
        SET_STRING_ELT(names, i, COPY_TO_USER_STRING(va_arg(va, char *)));
        SET_VECTOR_ELT(list, i, va_arg(va, USER_OBJECT_));
    }
    va_end(va);
    SET_NAMES(list, names);
    UNPROTECT(2);
    return list;
}

R_CallbackData*
R_createCBData(USER_OBJECT_ s_func, USER_OBJECT_ s_data)
{
  static R_CallbackData* (*fun)(USER_OBJECT_, USER_OBJECT_) = NULL;
  if(!fun) fun = ((R_CallbackData* (*)(USER_OBJECT_, USER_OBJECT_))R_GetCCallable("RGtk2", "R_createCBData"));
  return(fun(s_func, s_data));
} 

void
R_freeCBData(R_CallbackData* cbdata)
{
  static void (*fun)(R_CallbackData*) = NULL;
  if(!fun) fun = ((void (*)(R_CallbackData*))R_GetCCallable("RGtk2", "R_freeCBData"));
  return(fun(cbdata));
} 

GType
getSValueGType(USER_OBJECT_ sval)
{
  static GType (*fun)(USER_OBJECT_) = NULL;
  if(!fun) fun = ((GType (*)(USER_OBJECT_))R_GetCCallable("RGtk2", "getSValueGType"));
  return(fun(sval));
} 

USER_OBJECT_
R_internal_getInterfaces(GType type)
{
  static USER_OBJECT_ (*fun)(GType) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GType))R_GetCCallable("RGtk2", "R_internal_getInterfaces"));
  return(fun(type));
} 

USER_OBJECT_
R_internal_getGTypeAncestors(GType type)
{
  static USER_OBJECT_ (*fun)(GType) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(GType))R_GetCCallable("RGtk2", "R_internal_getGTypeAncestors"));
  return(fun(type));
} 

gpointer
propertyConstructor(GType obj_type, char** prop_names, USER_OBJECT_* args, int nargs)
{
  static gpointer (*fun)(GType, char**, USER_OBJECT_*, int) = NULL;
  if(!fun) fun = ((gpointer (*)(GType, char**, USER_OBJECT_*, int))R_GetCCallable("RGtk2", "propertyConstructor"));
  return(fun(obj_type, prop_names, args, nargs));
} 

USER_OBJECT_
R_setGObjectProps(USER_OBJECT_ sobj, USER_OBJECT_ svals)
{
  static USER_OBJECT_ (*fun)(USER_OBJECT_, USER_OBJECT_) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(USER_OBJECT_, USER_OBJECT_))R_GetCCallable("RGtk2", "R_setGObjectProps"));
  return(fun(sobj, svals));
} 

USER_OBJECT_
R_getGObjectProps(USER_OBJECT_ sobj, USER_OBJECT_ argNames)
{
  static USER_OBJECT_ (*fun)(USER_OBJECT_, USER_OBJECT_) = NULL;
  if(!fun) fun = ((USER_OBJECT_ (*)(USER_OBJECT_, USER_OBJECT_))R_GetCCallable("RGtk2", "R_getGObjectProps"));
  return(fun(sobj, argNames));
} 

void
GSListFreeStrings(GSList* gslist)
{
  static void (*fun)(GSList*) = NULL;
  if(!fun) fun = ((void (*)(GSList*))R_GetCCallable("RGtk2", "GSListFreeStrings"));
  return(fun(gslist));
} 

void
GListFreeStrings(GList* glist)
{
  static void (*fun)(GList*) = NULL;
  if(!fun) fun = ((void (*)(GList*))R_GetCCallable("RGtk2", "GListFreeStrings"));
  return(fun(glist));
} 

#endif
