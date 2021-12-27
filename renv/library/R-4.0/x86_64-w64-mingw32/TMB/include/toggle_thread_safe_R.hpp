/** \file
    \brief Override subset of R-API with thread safe versions.

    This file is included by `TMB.hpp` if compiled with `_OPENMP`.  It
    overrides a selected subset of the R-API with thread safe versions
    (FIXME: Still some missing?).

    If, for some reason, you want to undo the re-defines:

    ```
    #ifdef TMB_HAVE_THREAD_SAFE_R
    #include <toggle_thread_safe_R.hpp>
    #endif
    ```

    However, note that this is **not** recommended (the R macros are
    often used from the user template through at least `DATA_STRUCT()`
    and `DATA_UPDATE()`).

    \note To minimize overhead one should use as few R-API calls as
    possible, i.e. avoid doing REAL(x)[i] in a loop.
*/

#ifdef _OPENMP

#ifndef TMB_HAVE_THREAD_SAFE_R

inline SEXP Ts_getAttrib(SEXP x, SEXP y) {
  SEXP ans;
#pragma omp critical
  {
    ans = Rf_getAttrib(x, y);
  }
  return ans;
}

inline SEXP Ts_STRING_ELT(SEXP x, size_t i) {
  SEXP ans;
#pragma omp critical
  {
    ans = STRING_ELT(x, i);
  }
  return ans;
}

inline const char* Ts_CHAR(SEXP x) {
  const char* ans;
#pragma omp critical
  {
    ans = R_CHAR(x);
  }
  return ans;
}

inline SEXP Ts_VECTOR_ELT(SEXP x, size_t i) {
  SEXP ans;
#pragma omp critical
  {
    ans = VECTOR_ELT(x, i);
  }
  return ans;
}

inline R_len_t Ts_length(SEXP x) {
  R_len_t ans;
#pragma omp critical
  {
    ans = Rf_length(x);
  }
  return ans;
}

inline int* Ts_INTEGER(SEXP x) {
  int* ans;
#pragma omp critical
  {
    ans = INTEGER(x);
  }
  return ans;
}

inline double* Ts_REAL(SEXP x) {
  double* ans;
#pragma omp critical
  {
    ans = REAL(x);
  }
  return ans;
}

extern "C"
inline void Ts_GetRNGstate() {
#pragma omp critical
  {
    GetRNGstate();
  }
  // Wait for all threads to get to this point
#pragma omp barrier
}

inline Rboolean Ts_isNumeric(SEXP x) {
  Rboolean ans;
#pragma omp critical
  {
    ans = Rf_isNumeric(x);
  }
  return ans;
}

inline int Ts_LENGTH(SEXP x) {
  int ans;
#pragma omp critical
  {
    ans = LENGTH(x);
  }
  return ans;
}

inline R_xlen_t Ts_XLENGTH(SEXP x) {
  R_xlen_t ans;
#pragma omp critical
  {
    ans = XLENGTH(x);
  }
  return ans;
}

inline SEXP Ts_install(const char *x) {
  SEXP ans;
#pragma omp critical
  {
    ans = Rf_install(x);
  }
  return ans;
}

inline SEXP Ts_findVar(SEXP x, SEXP y) {
  SEXP ans;
#pragma omp critical
  {
    ans = Rf_findVar(x, y);
  }
  return ans;
}

inline SEXP Ts_ENCLOS(SEXP x) {
  SEXP ans;
#pragma omp critical
  {
    ans = ENCLOS(x);
  }
  return ans;
}

inline void Ts_warning(const char *x, ...) {
  if (omp_get_thread_num() == 0) {
    Rf_warning(x);
  }
}

/* --- Re-define ---------------------------------------------------------- */
#define TMB_HAVE_THREAD_SAFE_R
#define Rf_getAttrib   Ts_getAttrib
#define STRING_ELT     Ts_STRING_ELT
#undef  CHAR
#define CHAR(x)        Ts_CHAR(x)
#define VECTOR_ELT     Ts_VECTOR_ELT
#define Rf_length      Ts_length
#define INTEGER        Ts_INTEGER
#define REAL           Ts_REAL
#define GetRNGstate    Ts_GetRNGstate
#define Rf_isNumeric   Ts_isNumeric
#define LENGTH         Ts_LENGTH
#define XLENGTH        Ts_XLENGTH
#define Rf_install     Ts_install
#define Rf_findVar     Ts_findVar
#define ENCLOS         Ts_ENCLOS
#define Rf_warning     Ts_warning

#else

/* --- Un-define ---------------------------------------------------------- */
#undef TMB_HAVE_THREAD_SAFE_R
#undef Rf_getAttrib
#undef STRING_ELT
#undef  CHAR
#define CHAR(x)        R_CHAR(x)
#undef VECTOR_ELT
#undef Rf_length
#undef INTEGER
#undef REAL
#undef GetRNGstate
#undef Rf_isNumeric
#undef LENGTH
#undef XLENGTH
#undef Rf_install
#undef Rf_findVar
#undef ENCLOS
#undef Rf_warning

#endif // TMB_HAVE_THREAD_SAFE_R

#endif // _OPENMP
