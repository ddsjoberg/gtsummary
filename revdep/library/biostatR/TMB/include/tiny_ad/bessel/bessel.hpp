#ifndef TINY_AD_BESSEL_H
#define TINY_AD_BESSEL_H

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

#include "../gamma/gamma.hpp"

namespace bessel_utils {
using gamma_utils::Rf_gamma_cody;

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

/* I got crashes with Eigen if not setting this: */
#define MATHLIB_STANDALONE 1
#include "bessel_k.cpp"
#undef min0
#undef max0

template <class Float> Float sinpi(Float x) { return sin(x * M_PI); }
template <class Float> Float cospi(Float x) { return cos(x * M_PI); }
template<class S, class T>
T fmax2(S x, T y) {return (x < y) ? y : x;}
template<class Float> Float bessel_y(Float x, Float alpha);
#include "bessel_j.cpp"
#include "bessel_y.cpp"

/* FIXME: */
template<class Float>
Float R_pow(Float x, Float n) {return pow(x, n);}
template<class Float>
Float R_pow_di(Float x, int n)
{
    Float pow = 1.0;

    if (ISNAN(x)) return x;
    if (n != 0) {
	if (!R_FINITE(x)) return R_pow(x, (Float)n);
	if (n < 0) { n = -n; x = 1/x; }
	for(;;) {
	    if(n & 01) pow *= x;
	    if(n >>= 1) x *= x; else break;
	}
    }
    return pow;
}
template<class Float>
Float ldexp (Float x, int expo) {
  return exp( log(x) + expo * log(2.0) );
}

#include "bessel_i.cpp"
#undef MATHLIB_STANDALONE
#include "undefs.h"

} // End namespace bessel_utils

// using bessel_utils::bessel_k;
// using bessel_utils::bessel_j;
// using bessel_utils::bessel_y;
// using bessel_utils::bessel_i;

#endif
