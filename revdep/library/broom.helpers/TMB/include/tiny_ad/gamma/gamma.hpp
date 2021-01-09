#ifndef TINY_AD_GAMMA_H
#define TINY_AD_GAMMA_H

/* Standalone ? */
#ifndef R_RCONFIG_H
#include <cmath>
#include <iostream>
#include <float.h>  // INFINITY etc
#include <stdlib.h> // calloc (bessel)
#undef R_PosInf
#undef R_NegInf
#undef F77_NAME
#undef FALSE
#undef TRUE
#undef R_NaN
#undef R_FINITE
#define R_PosInf INFINITY
#define R_NegInf -INFINITY
#define F77_NAME(x) x
#define FALSE false
#define TRUE  true
#define R_NaN NAN
#define R_FINITE(x) R_finite(x)
#endif

namespace gamma_utils {

/* Selected functions may be called ignoring derivatives */
template<class T> int R_finite(T x) { return std::isfinite(asDouble(x)); }
template<class T> int isnan(T x) { return std::isnan(asDouble(x)); }

/* Common defines for Rmath routines */
#undef ML_ERROR
#undef MATHLIB_ERROR
#undef MATHLIB_WARNING
#undef MATHLIB_WARNING2
#undef MATHLIB_WARNING3
#undef MATHLIB_WARNING4
#undef MATHLIB_WARNING5
#undef ML_POSINF
#undef ML_NEGINF
#undef ML_NAN
#undef M_SQRT_2dPI
#undef ISNAN
# define ML_ERROR(x, s) /* nothing */
# define MATHLIB_ERROR(fmt,x) /* nothing */
# define MATHLIB_WARNING(fmt,x) /* nothing */
# define MATHLIB_WARNING2(fmt,x,x2) /* nothing */
# define MATHLIB_WARNING3(fmt,x,x2,x3) /* nothing */
# define MATHLIB_WARNING4(fmt,x,x2,x3,x4) /* nothing */
# define MATHLIB_WARNING5(fmt,x,x2,x3,x4,x5) /* nothing */
#define ML_POSINF	R_PosInf
#define ML_NEGINF	R_NegInf
#define ML_NAN		R_NaN
#define M_SQRT_2dPI	0.797884560802865355879892119869	/* sqrt(2/pi) */
#define ISNAN(x) (isnan(x)!=0)

// Re-defines
// #undef Rboolean
// #define Rboolean bool

// Fake that Rmath.h is included - and take explicitly what we need
#ifndef RMATH_H
#define RMATH_H
#endif
#ifndef M_LOG10_2
#define M_LOG10_2	0.301029995663981195213738894724	/* log10(2) */
#endif
#ifndef M_LN_SQRT_PI
#define M_LN_SQRT_PI	0.572364942924700087071713675677	/* log(sqrt(pi))
								   == log(pi)/2 */
#endif
#ifndef M_LN_SQRT_2PI
#define M_LN_SQRT_2PI	0.918938533204672741780329736406	/* log(sqrt(2*pi))
								 == log(2*pi)/2 */
#endif
#ifndef M_SQRT_PI
#define M_SQRT_PI	1.772453850905516027298167483341	/* sqrt(pi) */
#endif

#ifndef M_LN_SQRT_PId2
#define M_LN_SQRT_PId2	0.225791352644727432363097614947	/* log(sqrt(pi/2))
								   == log(pi/2)/2 */
#endif

// Fake that nmath.h is included - and take explicitly what we need
#ifndef MATHLIB_PRIVATE_H
#define MATHLIB_PRIVATE_H
#endif
#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif
#define ML_ERR_return_NAN { ML_ERROR(ME_DOMAIN, ""); return ML_NAN; }


// Forward declare for stirlerr:
template<class Float> Float lgammafn(Float x);
// For gamma.cpp:
template <class Float> Float sinpi(Float x) { return sin(x * M_PI); }

#include "chebyshev.cpp"
#include "lgammacor.cpp"
#undef nalgm
#undef xbig
#undef xmax
#include "stirlerr.cpp"
#undef S0
#undef S1
#undef S2
#undef S3
#undef S4
#include "gamma.cpp"
#undef ngam
#undef xmin
#undef xmax
#undef xsml
#undef dxrel
#include "lgamma.cpp"
#undef xmax
#undef dxrel
#include "gamma_cody.cpp"
#include "undefs.h"

} // End namespace gamma_utils

// using gamma_utils::lgammafn;
// using gamma_utils::gammafn;
// using gamma_utils::Rf_gamma_cody;

#endif
