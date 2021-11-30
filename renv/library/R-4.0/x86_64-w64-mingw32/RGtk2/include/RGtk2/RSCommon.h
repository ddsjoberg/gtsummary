
#ifndef RSCOMMON_H
#define RSCOMMON_H

#ifdef __cplusplus 
extern "C" {
#endif

  #if defined(_S_)
  /* || defined(_R_) */

  #ifdef _SPLUS5_
  #ifdef ARGS
  #undef ARGS
  #endif
  #endif


  #include "S.h"

  #ifdef _SPLUS5_

  #include "S_tokens.h"
  typedef boolean s_boolean;

  #endif /* End of _SPLUS5_ */


  #endif

  #if defined(_S4_)
  #define vector s_object
  typedef s_object* USER_OBJECT_;

  typedef long RSInt;

  typedef s_boolean Rboolean;

  #endif

  #if defined _SPLUS6_
  typedef s_boolean boolean;
  #define TRUE S_TRUE
  #define FALSE S_FALSE
  #endif


  #if defined(_R_)
  #include <Rinternals.h>
  #include <Rdefines.h>
  #ifdef length
  #undef length
  #endif

  #ifdef GET_LENGTH
  #undef GET_LENGTH
  #define GET_LENGTH(x) Rf_length(x)
  #endif

  #ifdef append
  #undef append
  #endif


  typedef SEXP USER_OBJECT_;
  typedef int RSInt;
  #include "Rversion.h"
  #if R_VERSION < R_Version(1, 2, 0)
  #define STRING_ELT(x,i)   STRING(x)[i]
  #define VECTOR_ELT(x,i)   VECTOR(x)[i]
  #define SET_STRING_ELT(x,i,v)     (STRING(x)[i]=(v))
  #define SET_VECTOR_ELT(x,i,v)     (VECTOR(x)[i]=(v))

  #define SETCAR(x,v)  (CAR(x) = v)
  #else
  #include "R_ext/Boolean.h"
  #endif



  #endif


  
  #if defined(_S4_) /* redefine vector and declare routines with  S_evaluator */

  #ifdef vector
  #undef vector
  #endif

  #define COPY_TO_USER_STRING(a) c_s_cpy(a, S_evaluator)
  #define LOCAL_EVALUATOR  S_EVALUATOR
  #define CREATE_FUNCTION_CALL(name, argList) alcf(name, argList, S_evaluator)

  #define CREATE_STRING_VECTOR(a) STRING_VECTOR(a, S_evaluator)

  #define NULL_USER_OBJECT S_void
  /* This is to keep R happy until it moves to char ** rather than
     SEXP * for character vectors.
  */
  #define CHAR_DEREF(x)   (x)


  #ifdef PROTECT
  #undef PROTECT
  #endif

  #define PROTECT(x)   (x) /**/
  #define UNPROTECT(x) /**/

   /* Note that this will override the one in S.h which is for S4, not S3, style classes. */
  #if defined(SET_CLASS)
  #undef SET_CLASS
  #endif

  #define SET_CLASS(obj,classname)  set_attr((obj), "class", (classname), S_evaluator)

  #if defined(GET_CLASS)
  #undef GET_CLASS
  #endif
  #define GET_CLASS(x) GET_ATTR((x), "class")

  #define STRING_ELT(x,i)   CHARACTER_DATA(x)[i]
  #define VECTOR_ELT(x,i)   LIST_POINTER(x)[i]


  #define SET_VECTOR_ELT(v, pos, val) LIST_POINTER((v))[(pos)]=(val)
  #define SET_STRING_ELT(v, pos, val) CHARACTER_DATA((v))[(pos)]=(val)   


  #endif /* end of this S4 */


  #if defined(_R_)
  #define CHAR_DEREF(x)   CHAR((x))


  #define IS_FUNCTION(x)   isFunction((x))

   /* SET_CLASS and SET_NAMES have been moved to Rdefines.h in the R distribution.*/
  #endif  /* of defined(_R_) */



  #if defined(_Octave_)

  #include <octave/oct.h>
  extern char error_buf[];
  #define PROBLEM sprintf(error_buf,
  #define ERROR  ); error(error_buf)

  #define STRING_VALUE(a)  a.all_strings()[0].c_str()

  #define GET_LENGTH(a)  getLength(a)

  #define LOCAL_EVALUATOR /**/
  #define COPY_TO_USER_STRING(a) strdup(a) /*XXX*/

  #endif /* end of defined(_Octave_)*/



#ifdef __cplusplus 
}
#endif

#endif /* end of RSCOMMON_H*/


