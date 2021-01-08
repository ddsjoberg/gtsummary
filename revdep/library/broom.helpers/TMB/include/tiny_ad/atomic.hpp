/********************************************************************
 * Special math functions are available in TMB by including this file
 * in the cpp file.
 ********************************************************************/

/** \file
    \brief Special functions depending on 'tiny_ad'.
*/

template<class Type> Type lgamma(Type x);

namespace atomic {

#define TINY_AD_USE_TINY_VEC 1
#include "tiny_ad/tiny_ad.hpp"
#include "mask.hpp"

/********************************************************************
 * Adding 'pbeta'
 ********************************************************************/
#include "beta/pbeta.hpp"    // Get namespace 'toms708'
TMB_BIND_ATOMIC(pbeta,
		111,
		toms708::pbeta(x[0], x[1], x[2], 1, 0) )

/********************************************************************
 * Adding 'bessel_k'
 ********************************************************************/
#include "bessel/bessel.hpp" // Get namespace 'bessel_utils'
TMB_BIND_ATOMIC(bessel_k,
		11,
		bessel_utils::bessel_k(x[0], x[1], 1.) )

/********************************************************************
 * Adding 'bessel_i'
 ********************************************************************/
#include "bessel/bessel.hpp" // Get namespace 'bessel_utils'
TMB_BIND_ATOMIC(bessel_i,
		11,
		bessel_utils::bessel_i(x[0], x[1], 1.) )

/********************************************************************
 * Adding 'bessel_j'
 ********************************************************************/
#include "bessel/bessel.hpp" // Get namespace 'bessel_utils'
TMB_BIND_ATOMIC(bessel_j,
		11,
		bessel_utils::bessel_j(x[0], x[1]) )

/********************************************************************
 * Adding 'bessel_y'
 ********************************************************************/
#include "bessel/bessel.hpp" // Get namespace 'bessel_utils'
TMB_BIND_ATOMIC(bessel_y,
		11,
		bessel_utils::bessel_y(x[0], x[1]) )

/********************************************************************
 * Adding 'dtweedie'
 ********************************************************************/
#include "tweedie/tweedie.hpp"
TMB_BIND_ATOMIC(tweedie_logW,
		011,
		tweedie_utils::tweedie_logW(x[0], x[1], x[2]) )

/********************************************************************
 * Adding numerically robust utility functions
 ********************************************************************/
#include "robust/distributions.hpp"
TMB_BIND_ATOMIC(log_dnbinom_robust,
                011,
                robust_utils::dnbinom_robust(x[0], x[1], x[2], true) )

TMB_BIND_ATOMIC(log_dbinom_robust,
                001,
                robust_utils::dbinom_robust(x[0], x[1], x[2], true) )

TMB_BIND_ATOMIC(logspace_add,
                11,
                robust_utils::logspace_add(x[0], x[1]) )

TMB_BIND_ATOMIC(logspace_sub,
                11,
                robust_utils::logspace_sub(x[0], x[1]) )

/********************************************************************
 * Adding Conway-Maxwell_poisson distribution
 ********************************************************************/
#include "compois/compois.hpp"
TMB_BIND_ATOMIC(compois_calc_logZ,
                11,
                compois_utils::calc_logZ(x[0], x[1]) )

TMB_BIND_ATOMIC(compois_calc_loglambda,
                11,
                compois_utils::calc_loglambda(x[0], x[1]) )

/********************************************************************
 * Adding 'qbeta'
 ********************************************************************/
extern "C" double Rf_qbeta(double, double, double, int, int);
template <class Type>
Type dbeta(Type x, Type shape1, Type shape2) {
  Type logres =
    lgamma(shape1 + shape2) - lgamma(shape1) - lgamma(shape2) +
    (shape1 - 1.) * log(x) + (shape2 - 1.) * log(1. - x);
  return exp(logres);
}
TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   qbeta
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0] = Rf_qbeta(tx[0], tx[1], tx[2], 1, 0);
			   ,
			   // ATOMIC_REVERSE
			   Type p = ty[0];
			   Type a = tx[1];
			   Type b = tx[2];
			   Type tmp = atomic::dbeta(p, a, b);
			   px[0] = 1.0 / tmp * py[0];
			   CppAD::vector<Type> arg(4);
			   arg[0] = p;
			   arg[1] = a;
			   arg[2] = b;
			   arg[3] = Type(1); // 1st order partials wrt. a and b
			   CppAD::vector<Type> D_shape = pbeta(arg);
			   px[1] = -D_shape[1] / tmp * py[0];
			   px[2] = -D_shape[2] / tmp * py[0];
			   )

} // End namespace atomic


