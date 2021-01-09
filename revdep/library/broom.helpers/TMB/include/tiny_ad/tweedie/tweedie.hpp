#ifndef TINY_AD_TWEEDIE_H
#define TINY_AD_TWEEDIE_H

#include "../gamma/gamma.hpp"

namespace tweedie_utils {
using gamma_utils::lgammafn;

template<class Float>
Float dmax (Float *x, int n){
  Float s = x[0] ;
  for (int i = 1; i < n; i++)
      if (x[i] > s) s = x[i] ; 
  return s ;
}
#define imax dmax

template<class S, class T>
T fmax2(S x, T y) {return (x < y) ? y : x;}

template<class S>
int imax2(S x, int y) {return (x < y) ? y : x;}
template<class S>
int imin2(S x, int y) {return (x < y) ? x : y;}

#ifndef Calloc
#define Calloc(n,type) (type*)calloc(n, sizeof(type))
#endif
#ifndef Free
#define Free free
#endif
#include "tweedie.cpp"
#undef TWEEDIE_DROP
#undef TWEEDIE_INCRE
#undef TWEEDIE_NTERM
#undef imax

} // End namespace tweedie_utils

#endif
