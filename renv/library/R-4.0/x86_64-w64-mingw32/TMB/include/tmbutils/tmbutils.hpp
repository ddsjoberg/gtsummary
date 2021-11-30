// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file 
   \brief Namespace of utility functions for TMB
*/

/** \brief Utility functions for TMB (automatically included)

Contains basic data types such as vectors, matrices and arrays.
Included automatically so you \b should \b NOT include it via
   \code
   using tmbutils
   \endcode

*/
namespace tmbutils{
// Utilities used by the core
using namespace Eigen;
#include "vector.hpp"
#include "array.hpp"

template <class Type, class From>
vector<Type> asVector(From *px, int n){
  vector<Type> x(n);
  for(int i=0;i<n;i++)x[i]=Type(px[i]);
  return x;
}

#if defined(R_R_H) 
template <class Type>
array<Type> asArray(SEXP x)
{
  if(!Rf_isArray(x))Rf_error("NOT AN ARRAY!");
  SEXP dim=Rf_getAttrib(x,R_DimSymbol);
  vector<int> d=asVector<int,int>(INTEGER(dim), LENGTH(dim));
  vector<Type> y=asVector<Type,double>(REAL(x), LENGTH(x));
  return array<Type>(y,d);
}

#endif

} // End namespace

