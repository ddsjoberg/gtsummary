/* Base bindings between R/S and GObject */

#ifndef RGTK2_GOBJECT_H
#define RGTK2_GOBJECT_H

#include "RSCommon.h"
#include <glib.h>
#include <glib-object.h>

typedef void (*RPointerFinalizer)(void *ptr);
typedef void* (*ElementConverter)(void *element);

/***** ARRAY CONVERSION MACROS ******/

/* converts an array, taking the reference of each element, so that conversion
	functions taking a pointer parameter will work (array elements are values) */
#define asRArrayRef(array, converter) \
__extension__ \
({ \
    asRArray(&array, converter); \
})

#define asRArrayRefWithSize(array, converter, n) \
__extension__ \
({ \
    asRArrayWithSize(&array, converter, n); \
})
/* converts an array directly using the conversion function to an R list */
#define asRArray(array, converter) \
__extension__ \
({ \
    _asRArray(array, converter, LIST, VECTOR); \
})

#define asRArrayWithSize(array, converter, n) \
__extension__ \
({ \
    _asRArrayWithSize(array, converter, n, LIST, VECTOR); \
})

/* converts primitive (numeric, integer, logical) arrays to R vectors */
#define _asRPrimArray(array, TYPE) \
__extension__ \
({ \
    guint n = 0; \
    USER_OBJECT_ s = R_NilValue; \
    if (array) { \
      while(array[n++]); \
      s = _asRPrimArrayWithSize(array, n-1, TYPE); \
    } \
    s; \
})

#define _asRPrimArrayWithSize(array, n, TYPE) \
__extension__ \
({ \
    guint i; \
    USER_OBJECT_ s_obj; \
    PROTECT(s_obj = NEW_ ## TYPE(n)); \
\
    for (i = 0; i < n; i++) { \
        TYPE ## _POINTER(s_obj)[i] = array[i]; \
	} \
\
    UNPROTECT(1); \
    s_obj; \
})

/* core converter, for converting string arrays and other arrays of pointer types */
#define _asRArray(array, converter, TYPE, SETTER_TYPE) \
__extension__ \
({ \
    guint n = 0; \
    USER_OBJECT_ s = R_NilValue; \
    if (array) { \
      while(array[n++]); \
      s = _asRArrayWithSize(array, converter, n-1, TYPE, SETTER_TYPE); \
    } \
    s; \
})

#define _asRArrayWithSize(array, converter, n, TYPE, SETTER_TYPE) \
__extension__ \
({ \
    guint i; \
    USER_OBJECT_ s_obj; \
    PROTECT(s_obj = NEW_ ## TYPE(n)); \
\
    for (i = 0; i < n; i++) { \
        SET_ ## SETTER_TYPE ## _ELT(s_obj, i, converter(array[i])); \
	} \
\
    UNPROTECT(1); \
    s_obj; \
})

/* Below are primitive array -> R vector converters */

#define asRStringArray(array) \
__extension__ \
({ \
    _asRArray(array, COPY_TO_USER_STRING, CHARACTER, STRING); \
})

#define asRStringArrayWithSize(array, n) \
__extension__ \
({ \
    _asRArrayWithSize(array, COPY_TO_USER_STRING, n, CHARACTER, STRING); \
})

#define asRIntegerArray(array) \
__extension__ \
({ \
    _asRPrimArray(array, INTEGER); \
})

#define asRIntegerArrayWithSize(array, size) \
__extension__ \
({ \
    _asRPrimArrayWithSize(array, size, INTEGER); \
})

#define RAW_POINTER(x)	RAW(x)

#define asRRawArray(array) \
__extension__ \
({ \
    _asRPrimArray(array, RAW); \
})

#define asRRawArrayWithSize(array, size) \
__extension__ \
({ \
    _asRPrimArrayWithSize(array, size, RAW); \
})


#define asRNumericArray(array) \
__extension__ \
({ \
    _asRPrimArray(array, NUMERIC); \
})

#define asRNumericArrayWithSize(array, size) \
__extension__ \
({ \
    _asRPrimArrayWithSize(array, size, NUMERIC); \
})

#define asRGTypeArrayWithSize(array, size) \
  asRArrayWithSize(array, asRGType, size)

#define asRLogicalArray(array) \
__extension__ \
({ \
    _asRPrimArray(array, LOGICAL); \
})

#define asRLogicalArrayWithSize(array, size) \
__extension__ \
({ \
    _asRPrimArrayWithSize(array, size, LOGICAL); \
})

/* for converting each element to an R pointer of a specified class 
	-- I don't think this is ever used */ 
#define toRPointerArray(array, type) \
__extension__ \
({ \
    toRPointerWithFinalizerArray(array, type); \
})
#define toRPointerArrayWithSize(array, type, n) \
__extension__ \
({ \
    toRPointerWithFinalizerArrayWithSize(array, type, NULL, n); \
})

/* for converting elements to R pointers of a specified class with a special finalizer 
	- only used like once */
#define toRPointerWithFinalizerArray(array, type, finalizer) \
__extension__ \
({ \
    guint n = 0; \
    USER_OBJECT_ s = R_NilValue; \
    if (array) { \
      while(array[n++]); \
      s = asRPointerWithFinalizerArrayWithSize(array, type, finalizer, n-1); \
    } \
    s; \
})
#define toRPointerWithFinalizerArrayWithSize(array, type, finalizer, n) \
__extension__ \
({ \
    guint i; \
    USER_OBJECT_ s_array; \
    PROTECT(s_array = NEW_LIST(n)); \
\
    for (i = 0; i < n; i++) { \
        SET_VECTOR_ELT(s_array, i, toRPointerWithFinalizer(array[i], type, finalizer)); \
    } \
\
    UNPROTECT(1); \
    s_array; \
})

/* converts each element to a ref'd R pointer (ie, they're GObjects) 
	- used only a couple of times */
#define toRPointerWithRefArray(array, type) \
__extension__ \
({ \
    int n = 0; \
    USER_OBJECT_ s = R_NilValue; \
    if (array) { \
      while(array[n++]); \
      s = asRPointerWithRefArrayWithSize(array, type, n-1); \
    } \
    s; \
})
#define toRPointerWithRefArrayWithSize(array, type, n) \
__extension__ \
({ \
    int i; \
    USER_OBJECT_ s_array; \
    PROTECT(s_array = NEW_LIST(n)); \
\
    for (i = 0; i < n; i++) { \
        SET_VECTOR_ELT(s_array, i, toRPointerWithRef(array[i], type)); \
    } \
\
    UNPROTECT(1); \
    s_array; \
})

/* this is used when there is an array of struct values that need to be copied
	into separate areas in memory so that they can be individually finalized 
	- used maybe once */
#define asRStructArray(array, type) \
__extension__ \
({ \
    guint n = 0; \
    USER_OBJECT_ s = R_NilValue; \
    if (array) { \
      while(array[n++]); \
      s = asRStructArrayWithSize(array, type, n-1); \
    } \
    s; \
})
#define asRStructArrayWithSize(array, type, n) \
__extension__ \
({ \
    guint i; \
    USER_OBJECT_ s_array; \
    PROTECT(s_array = NEW_LIST(n)); \
\
    for (i = 0; i < n; i++) { \
        typeof(array) ptr = g_malloc(sizeof(typeof(array[i]))); \
        memcpy(ptr, array+i, sizeof(typeof(array[i]))); \
        SET_VECTOR_ELT(s_array, i, toRPointerWithFinalizer(ptr, type, g_free)); \
    } \
\
    UNPROTECT(1); \
    s_array; \
})
/* for converting enum elements of a given type */
#define asREnumArray(array, type) \
__extension__ \
({ \
    int n = 0; \
    USER_OBJECT_ s = R_NilValue; \
    if (array) { \
      while(array[n++]); \
      s = asREnumArrayWithSize(array, type, n-1); \
    } \
    s; \
})
#define asREnumArrayWithSize(array, type, n) \
__extension__ \
({ \
    int i; \
    USER_OBJECT_ s_array; \
    PROTECT(s_array = NEW_LIST(n)); \
\
    for (i = 0; i < n; i++) { \
        SET_VECTOR_ELT(s_array, i, asREnum(array[i], type)); \
    } \
\
    UNPROTECT(1); \
    s_array; \
})

/* now from R to C */

#define asCArrayRef(s, type, converter) \
__extension__ \
({ \
    asCArray(s, type, * (type *)converter); \
})
#define asCArray(s_array, type, converter) \
__extension__ \
({ \
    guint i; \
\
    type* array = (type*)R_alloc(GET_LENGTH(s_array), sizeof(type)); \
\
    for (i = 0; i < GET_LENGTH(s_array); i++) { \
        array[i] = (type)converter(VECTOR_ELT(s_array, i)); \
    } \
\
    array; \
})
#define asCArrayDup(s, type, converter) \
__extension__ \
({ \
    type* array = asCArray(s, type, converter); \
    g_memdup(array, sizeof(type) * GET_LENGTH(s)); \
})

#define asCEnumArray(s_array, type, code) \
__extension__ \
({ \
    int i; \
\
    type* array = (type*)R_alloc(GET_LENGTH(s_array), sizeof(type)); \
\
    for (i = 0; i < GET_LENGTH(s_array); i++) { \
        array[i] = asCEnum(VECTOR_ELT(s_array, i), code); \
    } \
\
    array; \
})
gchar ** asCStringArray(USER_OBJECT_ svec);

/****** Primitive Type Conversion *********/

#define asCLogical(s_log) (GET_LENGTH(s_log) ? LOGICAL(s_log)[0] : FALSE)
#define asCInteger(s_int) (GET_LENGTH(s_int) ? INTEGER(s_int)[0] : 0)
#define asCRaw(s_raw) (GET_LENGTH(s_raw) ? RAW(s_raw)[0] : 0)
#define asCNumeric(s_num) (GET_LENGTH(s_num) ? REAL(s_num)[0] : 0)

const gchar * asCString(USER_OBJECT_ s_str);
gchar asCCharacter(USER_OBJECT_ s_char);

#define asRLogical ScalarLogical
#define asRInteger ScalarInteger
#define asRNumeric ScalarReal
#define asRRaw ScalarRaw
USER_OBJECT_ asRUnsigned(guint num);

USER_OBJECT_ asRCharacter(gchar c);
USER_OBJECT_ asRString(const gchar *str);

#define asCGenericData(sval) __extension__ ({ R_PreserveObject(sval); sval; })

USER_OBJECT_ asREnum(int value, GType etype);
USER_OBJECT_ asRFlag(guint value, GType ftype);
guint asCFlag(USER_OBJECT_ s_flag, GType ftype);
gint asCEnum(USER_OBJECT_ s_enum, GType etype);

/******* Pointer-type conversion ********/

USER_OBJECT_ toRPointerWithFinalizer(gconstpointer val, const gchar *typeName, 
  RPointerFinalizer finalizer);
#define toRPointer(val, name) toRPointerWithFinalizer(val, name, NULL)
USER_OBJECT_ toRPointerWithRef(gconstpointer val, const gchar *type);
USER_OBJECT_ toRPointerFn(DL_FUNC val, const gchar *typeName);

#define getPtrValue(sval) (sval == NULL_USER_OBJECT ? NULL : R_ExternalPtrAddr(sval))
#define getPtrValueFn(sval) (sval == NULL_USER_OBJECT ? NULL : R_ExternalPtrAddrFn(sval))
gpointer getPtrValueWithRef(USER_OBJECT_ sval);


/********* GLib structure conversion *********/
GQuark asCGQuark(USER_OBJECT_ sval);
USER_OBJECT_ asRGQuark(GQuark val);
GTimeVal* asCGTimeVal(USER_OBJECT_ s_timeval);
USER_OBJECT_ asRGTimeVal(const GTimeVal *timeval);
GString* asCGString(USER_OBJECT_ s_string);
GList* toCGList(USER_OBJECT_ s_list, gboolean dup);
#define asCGList(s_list) toCGList(s_list, FALSE)
#define asCGListDup(s_list) toCGList(s_list, TRUE)
USER_OBJECT_ asRGList(GList *glist, const gchar* type);
USER_OBJECT_ asRGListWithRef(GList *gslist, const gchar* type);
USER_OBJECT_ asRGListWithFinalizer(GList *glist, const gchar* type, RPointerFinalizer finalizer);
USER_OBJECT_ asRGListConv(GList *glist, ElementConverter converter);
GSList* toCGSList(USER_OBJECT_ s_list, gboolean dup);
#define asCGSList(s_list) toCGSList(s_list, FALSE)
#define asCGSListDup(s_list) toCGSList(s_list, TRUE)
USER_OBJECT_ asRGSList(GSList *gslist, const gchar* type);
USER_OBJECT_ asRGSListWithRef(GSList *gslist, const gchar* type);
USER_OBJECT_ asRGSListWithFinalizer(GSList *gslist, const gchar* type, RPointerFinalizer finalizer);
USER_OBJECT_ asRGSListConv(GSList *gslist, ElementConverter converter);
USER_OBJECT_ asRGError(GError *error);
GError *asCGError(USER_OBJECT_ s_error); 

/******* GObject structure conversion *********/
int R_setGValueFromSValue(GValue *val, USER_OBJECT_ sval);
USER_OBJECT_ R_setGValueForProperty(GValue *value, GObjectClass *class, const gchar *property_name, USER_OBJECT_ s_value);
GValue* createGValueFromSValue(USER_OBJECT_ sval);
gboolean initGValueFromSValue(USER_OBJECT_ sval, GValue *raw);
gboolean initGValueFromVector(USER_OBJECT_ sval, gint n, GValue *raw);
USER_OBJECT_ asRGValue(const GValue *val);
GValue* asCGValue(USER_OBJECT_ sval);
GType asCGType(USER_OBJECT_ sval);
USER_OBJECT_ asRGType(GType type);
GParamSpec* asCGParamSpec(USER_OBJECT_ s_spec);
USER_OBJECT_ asRGParamSpec(GParamSpec* spec);
GClosure* asCGClosure(USER_OBJECT_ s_closure);
USER_OBJECT_ asRGClosure(GClosure *closure);
USER_OBJECT_ toRPointerWithSink(void *val, const char *type);
USER_OBJECT_ asRGListWithSink(GList *glist, const gchar* type);
USER_OBJECT_ asRGSListWithSink(GSList *gslist, const gchar* type);

/********** User Function wrappers ********/

void S_GCompareFunc(gconstpointer s_a, gconstpointer s_b);
gboolean S_GSourceFunc(gpointer data);

/********** GClosure callbacks ***********/

GClosure* R_createGClosure(USER_OBJECT_ s_func, USER_OBJECT_ s_data);

/****** Custom RGtk2 types supporting R objects ********/

GType r_gtk_sexp_get_type(void);
#define R_GTK_TYPE_SEXP r_gtk_sexp_get_type()

GType r_gtk_param_spec_sexp_get_type(void);
#define R_GTK_TYPE_PARAM_SEXP r_gtk_param_spec_sexp_get_type()

typedef struct _RGtkParamSpecSexp {
  GParamSpec parent_instance;
  SEXPTYPE s_type;
  USER_OBJECT_ default_value;
} RGtkParamSpecSexp;

/* Marker interface for SGObjects */

typedef struct _SGObject SGObject; /* Dummy typedef */
typedef struct _SGObjectIface {
  GTypeInterface parent;
  USER_OBJECT_ (*get_environment)(SGObject *);
} SGObjectIface;

GType s_g_object_get_type(void);
#define S_TYPE_G_OBJECT s_g_object_get_type()

/******* GObject extension ********/

void S_gobject_class_init(GObjectClass *c, USER_OBJECT_ e);
/* getting the static environment out of the class of a SGObject */
#define S_GOBJECT_GET_ENV(s_object) \
__extension__ \
({ \
    GTypeQuery query; \
    g_type_query(G_OBJECT_TYPE(s_object), &query); \
    G_STRUCT_MEMBER(SEXP, G_OBJECT_GET_CLASS(s_object), query.class_size - sizeof(SEXP)); \
})
/* getting the instance-level environment out of a SGObject */
#define S_G_OBJECT_GET_INSTANCE_ENV(s_object) \
__extension__ \
({ \
    GTypeQuery query; \
    g_type_query(G_OBJECT_TYPE(s_object), &query); \
    G_STRUCT_MEMBER(SEXP, s_object, query.instance_size - sizeof(SEXP)); \
})
/* add the instance environment as an attribute of an R user object */
#define S_G_OBJECT_ADD_ENV(s_object, user_object) \
__extension__ \
({ \
    USER_OBJECT_ user_obj = PROTECT(user_object);			\
    setAttrib(user_obj, install(".private"), S_G_OBJECT_GET_INSTANCE_ENV(s_object)); \
    UNPROTECT(1); \
    user_obj; \
})

/******* Utilities *******/

/* puts return by reference parameters into a list */
USER_OBJECT_ retByVal(USER_OBJECT_ retval, ...);

/* generic callback stuff */

typedef struct {
  USER_OBJECT_ function;
  USER_OBJECT_ data;
  Rboolean     useData;
  Rboolean     userDataFirst;
  gpointer     extra; /* some C-level user data */
} R_CallbackData;

R_CallbackData *R_createCBData(USER_OBJECT_ s_func, USER_OBJECT_ s_data);
void R_freeCBData(R_CallbackData *cbdata);

/* introspection support */
GType getSValueGType(USER_OBJECT_ sval);
USER_OBJECT_ R_internal_getInterfaces(GType type);
USER_OBJECT_ R_internal_getGTypeAncestors(GType type);

/* property stuff */
gpointer propertyConstructor(GType obj_type, char **prop_names, USER_OBJECT_ *args, int nargs);
USER_OBJECT_ R_setGObjectProps(USER_OBJECT_ sobj, USER_OBJECT_ svals);
USER_OBJECT_ R_getGObjectProps(USER_OBJECT_ sobj, USER_OBJECT_ argNames);

/* make sure something is not NULL before we try to free it */
#define CLEANUP(cleaner, ptr) if (ptr) cleaner(ptr)
/* always free the actual string inside a GString */
#define free_g_string(str) g_string_free(str, TRUE)
/* easy way to free strings (or anything on the GLib stack) in G(S)Lists */
void GSListFreeStrings(GSList *gslist);
void GListFreeStrings(GList *glist);

void transformDoubleString(const GValue *src, GValue *dst);
void transformIntString(const GValue *src, GValue *dst);
void transformBooleanString(const GValue *src, GValue *dst);

/* GSeekType enum runtime type info support (needed by GIO) */

#define G_TYPE_SEEK_TYPE (g_seek_type_get_type ())
GType g_seek_type_get_type (void) G_GNUC_CONST;
#define G_TYPE_IO_CONDITION (g_io_condition_get_type ())
GType g_io_condition_get_type (void) G_GNUC_CONST;

/* do this by name, so that it is resolved at runtime, simplifying header dependencies */
#if GLIB_CHECK_VERSION(2,10,0)
#define UNOWNED_TYPE_NAME "GInitiallyUnowned"
#else
#define UNOWNED_TYPE_NAME "GtkObject"
#endif
  
#endif
