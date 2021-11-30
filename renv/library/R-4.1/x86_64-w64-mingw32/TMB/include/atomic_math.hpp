// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/**
   \brief Namespace with special functions and derivatives

   This namespace extends the 'derivatives table' of CppAD.
   - R's special math library is extended with derivatives in cases 
   where symbolic derivatives are available. These special functions
   are often iterative and therefore difficult to implement with AD
   types. Instead, we code the derivatives based on the double versions 
   available from R. This approach requires fewer code lines, and has the
   benefit of obtaining the same high accuracy as R's math functions.
   - Some matrix operations are extended with derivatives. This greatly 
   reduces the AD memory usage. Furthermore, these atomic operations
   can be linked to a performance library by setting preprocesor flag 
   EIGEN_USE_BLAS.
   - New symbols can be added by advanced users. First option is to
   code the reverse mode derivatives by hand using the
   TMB_ATOMIC_VECTOR_FUNCTION macro, see source code for examples.
   Second option is to generate reverse mode derivatives automatically
   using the macro REGISTER_ATOMIC.
*/
namespace atomic {
/**
   \internal \brief Namespace with double versions of some R special math
   functions
*/
namespace Rmath {
  /*
    Was:

    #include <Rmath.h>

    However, Rmath defines a large number of macros that do not
    respect the namespace limits. Some of them will conflict with
    other TMB functions or mess up symbols in the users template
    (e.g. change 'dt' to 'Rf_dt').

    Therefore, explicitly select the few function headers (from
    /usr/share/R/include/Rmath.h) rather than including them all.
  */

  extern "C" {
    /* See 'R-API: entry points to C-code' (Writing R-extensions) */
    double	Rf_pnorm5(double, double, double, int, int);
    double	Rf_qnorm5(double, double, double, int, int);
    double	Rf_ppois(double, double, int, int);
    double	Rf_bessel_k(double, double, double);
    double	Rf_bessel_i(double, double, double);
    double	Rf_pgamma(double, double, double, int, int);
    double	Rf_qgamma(double, double, double, int, int);
    double	Rf_lgammafn(double);
    double	Rf_psigamma(double, double);
    double	Rf_fmin2(double, double);
    /* Selected headers from <R_ext/Applic.h> */
    typedef void integr_fn(double *x, int n, void *ex);
    void Rdqags(integr_fn f, void *ex, double *a, double *b,
                double *epsabs, double *epsrel,
                double *result, double *abserr, int *neval, int *ier,
                int *limit, int *lenw, int *last, int *iwork, double *work);
    void Rdqagi(integr_fn f, void *ex, double *bound, int *inf,
                double *epsabs, double *epsrel,
                double *result, double *abserr, int *neval, int *ier,
                int *limit, int *lenw, int *last,
                int *iwork, double *work);
  }

  /* Non-standard TMB special functions based on numerical
     integration: */
#ifdef WITH_LIBTMB
  void integrand_D_incpl_gamma_shape(double *x, int nx, void *ex);
  double D_incpl_gamma_shape(double x, double shape, double n, double logc);
  double inv_incpl_gamma(double y, double shape, double logc);
  double D_lgamma(double x, double n);
#else
  void integrand_D_incpl_gamma_shape(double *x, int nx, void *ex){
    double* parms=(double*)ex;
    double shape = parms[0];
    double n     = parms[1];
    double logc  = parms[2];
    for(int i=0; i<nx; i++){
      x[i] = exp( -exp(x[i]) + shape * x[i] + logc ) * pow(x[i], n);
    }
  }
  /* n'th order derivative of (scaled) incomplete gamma wrt. shape parameter */
  double D_incpl_gamma_shape(double x, double shape, double n, double logc){
    if(n<.5){
      return exp(logc + Rf_lgammafn(shape)) * Rf_pgamma(x, shape, 1.0, 1, 0);
    }
    double epsabs=1e-10;
    double epsrel=1e-10;
    double result1=0;
    double result2=0;
    double abserr=10000;
    int neval=10000;
    int ier=0;
    int limit=100;
    int lenw = 4 * limit;
    int last=0;
    int* iwork = (int*)malloc(limit * sizeof(int));
    double* work = (double*)malloc(lenw * sizeof(double));
    double ex[3];
    ex[0] = shape;
    ex[1] = n;
    ex[2] = logc; /* Scale integrand with exp(logc) */
    double bound; /* For indefinite integration */
    int inf=-1;   /* corresponds to (-Inf, bound) */
    bound = log(Rf_fmin2(x,shape));
    /* integrate -Inf...min(log(x),log(shape)) */
    Rdqagi(integrand_D_incpl_gamma_shape, ex, &bound, &inf,
	   &epsabs, &epsrel,
	   &result1, &abserr, &neval, &ier,
	   &limit, &lenw, &last, iwork, work);
    if(ier!=0){
#ifndef _OPENMP
      Rf_warning("incpl_gamma (indef) integrate unreliable: x=%f shape=%f n=%f ier=%i", x, shape, n, ier);
#endif
    }
    /* integrate min(log(x),log(shape))...log(x) */
    if(x>shape){
      ier = 0;
      double a = bound;
      double b = log(x);
      Rdqags(integrand_D_incpl_gamma_shape, ex, &a, &b,
	     &epsabs, &epsrel,
	     &result2, &abserr, &neval, &ier,
	     &limit, &lenw, &last, iwork, work);
      if(ier!=0){
#ifndef _OPENMP
	Rf_warning("incpl_gamma (def) integrate unreliable: x=%f shape=%f n=%f ier=%i", x, shape, n, ier);
#endif
      }
    }
    free(iwork);
    free(work);
    return result1 + result2;
  }
  double inv_incpl_gamma(double y, double shape, double logc){
    double logp = log(y) - Rf_lgammafn(shape) - logc;
    double scale = 1.0;
    return Rf_qgamma(exp(logp), shape, scale, 1, 0);
  }
  /* n'th order derivative of log gamma function */
  double D_lgamma(double x, double n){
    if(n<.5)return Rf_lgammafn(x);
    else return Rf_psigamma(x,n-1.0);
  }
#endif // #ifdef WITH_LIBTMB

}

#include "atomic_macro.hpp"

template<class Type>
struct TypeDefs{
  /* The following typedefs allow us to construct matrices based on
     already allocated memory (pointers). 'MapMatrix' gives write access
     to the pointers while 'ConstMapMatrix' is read-only.
  */
  typedef Eigen::Map<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > MapMatrix;
  typedef Eigen::Map<const Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > ConstMapMatrix;
  typedef Eigen::LDLT<Eigen::Matrix<Type, Eigen::Dynamic, Eigen::Dynamic> > LDLT;
};

/** \internal \brief Convert segment of CppAD::vector to Eigen::Matrix
    \param x Input vector.
    \param m Number of rows in result.
    \param n Number of columns in result.
    \param offset Segment offset.
*/
template<class Type>
typename TypeDefs<Type>::ConstMapMatrix vec2mat(const CppAD::vector<Type> &x, int m, int n, int offset=0){
  typedef typename TypeDefs<Type>::ConstMapMatrix ConstMapMatrix_t;
  ConstMapMatrix_t res(&x[offset], m, n);
  return res;
}

/** \internal \brief Convert Eigen::Matrix to CppAD::vector by stacking the matrix columns.
    \param x Input matrix.
*/
template<class Type>
CppAD::vector<Type> mat2vec(matrix<Type> x){
  int n=x.size();
  CppAD::vector<Type> res(n);
  for(int i=0;i<n;i++)res[i]=x(i);
  return res;
}

/** \internal \brief Standard normal density function 'dnorm1'.
    Needed to define derivative of 'pnorm1'.
*/
template<class Type>
Type dnorm1(Type x){
  return Type(1.0/sqrt(2.0*M_PI)) * exp(-Type(.5)*x*x);
}

/** \brief Atomic version of standard normal distribution function. 
    Derivative is known to be 'dnorm1'.
    \param x Input vector of length 1.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   pnorm1
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0] = Rmath::Rf_pnorm5(tx[0],0,1,1,0);
			   ,
			   // ATOMIC_REVERSE
			   px[0] = dnorm1(tx[0]) * py[0];
			   )

/** \brief Atomic version of standard normal quantile function. 
    Derivative is expressed through 'dnorm1'.
    \param x Input vector of length 1.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   qnorm1
			   ,
			   // OUTPUT_DIM
			   1,
			   // ATOMIC_DOUBLE
			   ty[0] = Rmath::Rf_qnorm5(tx[0],0,1,1,0);
			   ,
			   // ATOMIC_REVERSE
			   px[0] = Type(1) / dnorm1(ty[0]) * py[0];
			   )

/** \brief Atomic version of scaled incomplete gamma function differentiated to any order wrt. shape parameter
    \f[ \exp(c) \int_0^{y} \exp(-t) t^{\lambda-1} \log(t)^n \:dt \f]
    where the 4 input parameters are passed as a vector \f$x=(y,\lambda,n,c)\f$.
    Note that the normalized incomplete gamma function is obtained as the special case 
    \f$n=0\f$ and \f$c=-\log \Gamma(\lambda)\f$.
    Valid parameter range: \f$x \in \mathbb{R}_+\times\mathbb{R}_+\times\mathbb{N}_0\times\mathbb{R}\f$.
    \warning No check is performed on parameters
    \param x Input vector of length 4.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   D_incpl_gamma_shape
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0]=Rmath::D_incpl_gamma_shape(tx[0],tx[1],tx[2],tx[3]);
			   ,
			   // ATOMIC_REVERSE
			   px[0] = exp( -tx[0] + (tx[1]-Type(1.0)) * log(tx[0]) + tx[3] ) * pow(log(tx[0]),tx[2]) * py[0];
			   CppAD::vector<Type> tx_(tx);
			   tx_[2] = tx_[2] + Type(1.0);  // Add one to get partial wrt. tx[1]
			   px[1] = D_incpl_gamma_shape(tx_)[0] * py[0];
			   px[2] = Type(0);
			   px[3] = ty[0] * py[0];
			   )

/** \brief Atomic version of inverse of scaled incomplete gamma function.
    Given \f$z\f$ find \f$y\f$ such that
    \f[ z = \exp(c) \int_0^{y} \exp(-t) t^{\lambda-1} \:dt \f]
    where the 3 input parameters are passed as a vector \f$x=(z,\lambda,c)\f$.
    The special case \f$c=-\log \Gamma(\lambda)\f$ gives the inverse normalized
    incomplete gamma function.
    Valid parameter range: \f$x \in \mathbb{R}_+\times\mathbb{R}_+\times\mathbb{R}\f$.
    \warning No check is performed on parameters
    \param x Input vector of length 3.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   inv_incpl_gamma
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0]=Rmath::inv_incpl_gamma(tx[0],tx[1],tx[2]);
			   ,
			   // ATOMIC_REVERSE
			   Type value = ty[0];
			   Type shape = tx[1];
			   Type logc = tx[2];
			   Type tmp = exp(-value+logc)*pow(value,shape-Type(1));
			   px[0] = 1.0 / tmp * py[0];
			   CppAD::vector<Type> arg(4);
			   arg[0] = value;
			   arg[1] = shape;
			   arg[2] = Type(1); // 1st order partial wrt. shape
			   arg[3] = logc;
			   px[1] = -D_incpl_gamma_shape(arg)[0] / tmp * py[0];
			   arg[2] = Type(0); // 0 order partial wrt. shape
			   px[2] = -D_incpl_gamma_shape(arg)[0] / tmp * py[0];
			   )

/** \brief Atomic version of the n'th order derivative of the log gamma function.
    \f[ \frac{d^n}{d\lambda^n}\log \Gamma(\lambda) \f]
    where the 2 input parameters are passed as a vector \f$x=(\lambda,n)\f$.
    The special case \f$n=0\f$ gives the log gamma function.
    \param x Input vector of length 2.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   D_lgamma
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0]=Rmath::D_lgamma(tx[0],tx[1]);
			   ,
			   // ATOMIC_REVERSE
			   CppAD::vector<Type> tx_(2);
			   tx_[0]=tx[0];
			   tx_[1]=tx[1]+Type(1.0);
			   px[0] = D_lgamma(tx_)[0] * py[0];
			   px[1] = Type(0);
			   )

/** \brief Atomic version of poisson cdf \f$ppois(n,\lambda)\f$.
    Valid parameter range: \f$x =(n,\lambda) \in \mathbb{N}_0\times\mathbb{R}_+\f$.
    \warning No check is performed on parameters
    \param x Input vector of length 2.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   ppois
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0]=Rmath::Rf_ppois(tx[0],tx[1],1,0);
			   ,
			   // ATOMIC_REVERSE
			   Type value = ty[0];
			   Type n = tx[0];
			   Type lambda = tx[1];
			   CppAD::vector<Type> arg(2);
			   arg[0] = n - Type(1);
			   arg[1] = lambda;
			   px[0] = Type(0);
			   px[1] = (-value + ppois(arg)[0]) * py[0];
			   )

/** \brief Atomic version of \f$besselK(x,\nu)\f$.
    Valid parameter range: \f$x =(x,\nu) \in \mathbb{R}_+\times\mathbb{R}\f$.
    \note This atomic function does not handle the derivative wrt. \f$\nu\f$.
    \param x Input vector of length 2.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   bessel_k_10
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0] = Rmath::Rf_bessel_k(tx[0], tx[1], 1.0 /* Not scaled */);
			   ,
			   // ATOMIC_REVERSE
			   Type value = ty[0];
			   Type x = tx[0];
			   Type nu = tx[1];
			   CppAD::vector<Type> arg(2);
			   arg[0] = x;
			   arg[1] = nu + Type(1);
			   px[0] = ( -bessel_k_10(arg)[0] + value * (nu / x) ) * py[0];
			   px[1] = Type(0); /* Not implemented (!) */
			   )

/** \brief Atomic version of \f$besselI(x,\nu)\f$.
    Valid parameter range: \f$x =(x,\nu) \in \mathbb{R}_+\times\mathbb{R}\f$.
    \note This atomic function does not handle the derivative wrt. \f$\nu\f$.
    \param x Input vector of length 2.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   bessel_i_10
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0] = Rmath::Rf_bessel_i(tx[0], tx[1], 1.0 /* Not scaled */);
			   ,
			   // ATOMIC_REVERSE
			   Type x =  tx[0];
			   Type nu = tx[1];
			   CppAD::vector<Type> arg(2);
			   arg[0] = x;
			   arg[1] = nu + Type(1);
			   Type B_right = bessel_i_10(arg)[0];
			   arg[1] = nu - Type(1);
			   Type B_left  = bessel_i_10(arg)[0];
			   px[0] = Type(0.5) * ( B_left + B_right ) * py[0];
			   px[1] = Type(0); /* Not implemented (!) */
)

/** \cond */
template<class Type> /* Header of matmul interface */
matrix<Type> matmul(matrix<Type> x, matrix<Type> y);
template<>
matrix<double> matmul(matrix<double> x, matrix<double> y)CSKIP({
    return x*y;
})
/** \endcond */

/** \brief Atomic version of matrix multiply.
    Multiplies n1-by-n2 matrix with n2-by-n3 matrix.
    \param x Input vector of length 2+n1*n2+n2*n3 containing the
    output dimension (length=2), the first matrix (length=n1*n2) and
    the second matrix (length=n2*n3).
    \return Vector of length n1*n3 containing result of matrix
    multiplication.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   matmul
			   ,
			   // OUTPUT_DIM
			   CppAD::Integer(tx[0]) * CppAD::Integer(tx[1])
			   ,
			   // ATOMIC_DOUBLE
			   typedef TypeDefs<double>::MapMatrix MapMatrix_t;
			   typedef TypeDefs<double>::ConstMapMatrix ConstMapMatrix_t;
			   int n1 = CppAD::Integer(tx[0]);
			   int n3 = CppAD::Integer(tx[1]);
			   int n2 = (tx.size() - 2) / (n1 + n3);
			   ConstMapMatrix_t X(&tx[2      ], n1, n2);
			   ConstMapMatrix_t Y(&tx[2+n1*n2], n2, n3);
			   MapMatrix_t      Z(&ty[0      ], n1, n3);
			   Z = X * Y;
			   ,
			   // ATOMIC_REVERSE (W*Y^T, X^T*W)
			   typedef typename TypeDefs<Type>::MapMatrix MapMatrix_t;
			   int n1 = CppAD::Integer(tx[0]);
			   int n3 = CppAD::Integer(tx[1]);
			   int n2 = (tx.size() - 2) / (n1 + n3);
			   matrix<Type> Xt = vec2mat(tx, n1, n2, 2).transpose();
			   matrix<Type> Yt = vec2mat(tx, n2, n3, 2 + n1*n2).transpose();
			   matrix<Type> W = vec2mat(py, n1, n3);
			   MapMatrix_t res1(&px[2      ], n1, n2);
			   MapMatrix_t res2(&px[2+n1*n2], n2, n3);
			   res1 = matmul(W, Yt); // W*Y^T
			   res2 = matmul(Xt, W); // X^T*W
			   px[0] = 0; px[1] = 0;
			   )

/** \brief Atomic version of matrix inversion.
    Inverts n-by-n matrix by LU-decomposition.
    \param x Input vector of length n*n.
    \return Vector of length n*n.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   matinv
			   ,
			   // OUTPUT_DIM
			   tx.size()
			   ,
			   // ATOMIC_DOUBLE
			   typedef TypeDefs<double>::MapMatrix MapMatrix_t;
			   typedef TypeDefs<double>::ConstMapMatrix ConstMapMatrix_t;
			   int n = sqrt((double)tx.size());
			   ConstMapMatrix_t X(&tx[0], n, n);
			   MapMatrix_t      Y(&ty[0], n, n);
			   Y = X.inverse();   // Use Eigen matrix inverse (LU)
			   ,
			   // ATOMIC_REVERSE  (-f(X)^T*W*f(X)^T)
			   typedef typename TypeDefs<Type>::MapMatrix MapMatrix_t;
			   int n = sqrt((double)ty.size());
			   MapMatrix_t res(&px[0], n, n);
			   matrix<Type> W = vec2mat(py, n, n); // Range direction
			   matrix<Type> Y = vec2mat(ty, n, n); // f(X)
			   matrix<Type> Yt = Y.transpose();    // f(X)^T
			   matrix<Type> tmp = matmul(W, Yt);   // W*f(X)^T
			   res = -matmul(Yt, tmp);             // -f(X)^T*W*f(X)^T
			   )

/** \brief Atomic version of log determinant of positive definite n-by-n matrix.
    \param x Input vector of length n*n.
    \return Vector of length 1.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   logdet
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   int n=sqrt((double)tx.size());
			   matrix<double> X=vec2mat(tx,n,n);
			   matrix<double> LU=X.lu().matrixLU();    // Use Eigen LU decomposition
			   vector<double> LUdiag = LU.diagonal();
			   double res=LUdiag.abs().log().sum();    // TODO: currently PD only - take care of sign.
			   ty[0] = res;
			   ,
			   // ATOMIC_REVERSE  (X^-1*W[0])
			   CppAD::vector<Type> invX = matinv(tx);
			   for(size_t i=0; i<tx.size(); i++) px[i] = invX[i] * py[0];
			   )

/** \brief Atomic version of log determinant *and* inverse of positive definite n-by-n matrix.
    Calculated by Cholesky decomposition.
    \param x Input vector of length n*n.
    \return Vector of length 1+n*n.
*/
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   invpd
			   ,
			   // OUTPUT_DIM
			   1 + tx.size()
			   ,
			   // ATOMIC_DOUBLE
			   typedef TypeDefs<double>::LDLT LDLT_t;
			   using namespace Eigen;
			   int n=sqrt((double)tx.size());
			   matrix<double> X=vec2mat(tx,n,n);
			   matrix<double> I(X.rows(),X.cols());
			   I.setIdentity();
			   LDLT_t ldlt(X);
			   matrix<double> iX = ldlt.solve(I);
			   vector<double> D = ldlt.vectorD();
			   double logdetX = D.log().sum();
			   ty[0] = logdetX;
			   for(int i=0;i<n*n;i++)ty[i+1]=iX(i);
			   ,
			   // ATOMIC_REVERSE  (f2(X)*W1[0] - f2(X)^T*W2*f2(X)^T)
			   int n=sqrt((double)tx.size());
			   Type W1=py[0];                     // Range direction
			   matrix<Type> W2=vec2mat(py,n,n,1); // Range direction
			   matrix<Type> Y=vec2mat(ty,n,n,1);  // f2(X)
			   matrix<Type> Yt=Y.transpose();     // f2(X)^T
			   matrix<Type> tmp=matmul(W2,Yt);    // W2*f2(X)^T
			   matrix<Type> res=-matmul(Yt,tmp);  // -f2(X)^T*W2*f2(X)^T
			   res = res + Y*W1;
			   px=mat2vec(res);
			   )

/* ================================== INTERFACES
*/

/** \brief Matrix multiply

    Matrix multiplication of large dense matrices.

    \code
    matrix<Type> x;
    matrix<Type> y;
    atomic::matmul(x, y);
    \endcode

    For small matrices use

    \code
    x * y;
    \endcode

    \ingroup matrix_functions
*/
template<class Type>
matrix<Type> matmul(matrix<Type> x, matrix<Type> y){
  CppAD::vector<Type> arg(2+x.size()+y.size());
  arg[0] = x.rows(); arg[1] = y.cols();
  for(int i=0;i<x.size();i++){arg[2+i]=x(i);}
  for(int i=0;i<y.size();i++){arg[2+i+x.size()]=y(i);}
  CppAD::vector<Type> res(x.rows()*y.cols());
  matmul(arg,res);
  return vec2mat(res,x.rows(),y.cols());
}

/** \brief Matrix inverse

    Invert a matrix by LU-decomposition.

    \code
    matrix<Type> x;
    atomic::matinv(x);
    \endcode

    For small matrices use

    \code
    x.inverse();
    \endcode

    \ingroup matrix_functions
*/
template<class Type>
matrix<Type> matinv(matrix<Type> x){
  int n=x.rows();
  return vec2mat(matinv(mat2vec(x)),n,n);
}

/** \brief Matrix inverse and determinant

    Calculate matrix inverse *and* log-determinant of a positive
    definite matrix.

    \ingroup matrix_functions
*/
template<class Type>
matrix<Type> matinvpd(matrix<Type> x, Type &logdet){
  int n=x.rows();
  CppAD::vector<Type> res = invpd(mat2vec(x));
  logdet = res[0];
  return vec2mat(res,n,n,1);
}

/** \brief Log-determinant of positive definite matrix
    \ingroup matrix_functions
*/
template<class Type>
Type logdet(matrix<Type> x){
  return logdet(mat2vec(x))[0];
}

/* Temporary test of dmvnorm implementation based on atomic symbols.
   Should reduce tape size from O(n^3) to O(n^2).
*/
template<class Type>
Type nldmvnorm(vector<Type> x, matrix<Type> Sigma){
  matrix<Type> Q=matinv(Sigma);
  Type logdetQ = -logdet(Sigma);
  Type quadform = (x*(Q*x)).sum();
  return -Type(.5)*logdetQ + Type(.5)*quadform + x.size()*Type(log(sqrt(2.0*M_PI)));
}

} /* End namespace atomic */

#include "checkpoint_macro.hpp"
