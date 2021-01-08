#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  /* Minimal example */
  DATA_VECTOR(x);
  PARAMETER(mu);
  PARAMETER(logSigma);

  Type f = 0;
  f -= dnorm(x, mu, exp(logSigma), true).sum();

  return f;

  /* Quick Reference
     ===============

     ** Macros to read data and declare parameters:

     _Template_Syntax_              _C++_type_                     _R_type_
     DATA_VECTOR(name)              vector<Type>                   vector
     DATA_MATRIX(name)              matrix<Type>                   matrix
     DATA_SCALAR(name)              Type                           numeric(1)
     DATA_INTEGER(name)             int                            integer(1)
     DATA_FACTOR(name)              vector<int>                    factor
     DATA_SPARSE_MATRIX(name)       Eigen::SparseMatrix<Type>      dgTMatrix
     DATA_ARRAY(name)               array<Type>                    array
     PARAMETER_MATRIX(name)         matrix<Type>                   matrix
     PARAMETER_VECTOR(name)         vector<Type>                   vector
     PARAMETER_ARRAY(name)          array<Type>                    array
     PARAMETER(name)                Type                           numeric(1)

     ** Macro to report intermediate expressions back to R:

     REPORT(x)
     ADREPORT(x)

     ** Basic constructors:

     vector<Type> v(n1);
     matrix<Type> m(n1,n2);
     array<Type> a(n1,n2,n3)

     ** Basic operations:

     v+v,v-v,v*v,v/v                Pointwise binary operations
     m*v                            Matrix-vector multiply
     a.col(i)                       R equivalent of a[,,i]
     a.col(i).col(j)                R equivalent of a[,j,i]
     a(i,j,k)                       R equivalent of a[i,j,k]
     exp(v)                         Pointwise math
     m(i,j)                         R equivalent of m[i,j]
     v.sum()                        R equivalent of sum(v)
     m.transpose()                  R equivalent of t(m)

     ** Distributions:

     Type dnbinom2(const Type &x, const Type &mu, const Type &var, int give_log=0)
     Type dpois(const Type &x, const Type &lambda, int give_log=0)
     Type dlgamma(Type y, Type shape, Type scale, int give_log=0)
     Type dnorm(Type x, Type mean, Type sd, int give_log=0)

     ** Parallel accumulator declaration (only methods "+=" and "-="):
     
     parallel_accumulator<Type> res(this);

  */

}
