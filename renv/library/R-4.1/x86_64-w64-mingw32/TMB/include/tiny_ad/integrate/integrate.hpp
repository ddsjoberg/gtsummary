#ifndef TINY_AD_INTEGRATE_H
#define TINY_AD_INTEGRATE_H

/* Standalone ? */
#ifndef R_RCONFIG_H
#include <vector>
using std::vector;
#include <float.h>  // INFINITY etc
#endif

/**
   \brief Namespace with utility functions for adaptive numerical integration

   Interfaces to R's integrator that can be used with forward mode AD.
*/
namespace gauss_kronrod {
// Fake that Rmath.h is included - and take explicitly what we need
// #ifndef RMATH_H
// #define RMATH_H
// #endif
// *** Update: Rmath.h inclusion now disabled in integrate.cpp

/*
  These fmin2 and fmax2 assume that derivatives are not required. This
  is OK for integrate since these functions are used to calculate
  error bounds - nothing else.

  FIXME: Put all helper functions for integrate in its own namespace
  to ensure that fmin2, fmax2 etc, are never used elsewhere.
*/
template<class T>
double value(T x){ return ((double*) &x)[0]; }
template<class S, class T> // FIXME: Would imin2(int,int) conflict with Rf_imin2 ?
int imin2(S x, T y) {return (x < y) ? x : y;}
template<class S, class T>
double fmin2(S x, T y) {return (value(x) < value(y)) ? value(x) : value(y);}
template<class S, class T>
double fmax2(S x, T y) {return (value(x) < value(y)) ? value(y) : value(x);}

#include "integrate.cpp"

/** \brief User control parameters for R's integrate

    These control parameters are similar to R's function 'integrate'.
    \param subdivisions The maximum number of subintervals.
    \param reltol Relative accuracy requested.
    \param abstol Absolute accuracy requested.
*/
struct control {
  int subdivisions;
  double reltol;
  double abstol;
  control(int subdivisions_ = 100,
	  double reltol_    = 1e-4,
	  double abstol_    = 1e-4) :
    subdivisions(subdivisions_),
    reltol(reltol_),
    abstol(abstol_) {}
};

/**
    \brief Interface to R's adaptive integrate routine.

    Takes Integrand (functor) as template parameter. Integrand must be
    a template class where:

    1. Template parameter is the scalar type.
    2. Contains a 'typedef Type Scalar;'
    3. Has an evaluation operator.

    Use this class if multiple integrals must be computed with
    different parameters. Otherwise use the 'integrate' function (see
    below).
*/
template<class Integrand>
struct Integral {
  typedef typename Integrand::Scalar Type;
  // R's integrators require a vectorized integrand
  struct vectorized_integrand {
    Integrand f;
    vectorized_integrand(Integrand f_) : f(f_) {}
    void operator() (Type *x, int n, void *ex) {
      for(int i=0; i<n; i++) x[i] = f(x[i]);
    }
  } fn;
  /** \brief Return reference to integrand so the user can change parameters. */
  Integrand& integrand(){return fn.f;}
  // Input to R's integrators
  Type epsabs, epsrel, result, abserr;
  int neval, ier, limit, lenw, last;
  vector<int> iwork;
  vector<Type> work;
  void setAccuracy(double epsrel_ = 1e-4, double epsabs_ = 1e-4) {
    epsabs = epsabs_; epsrel = epsrel_; result = 0; abserr = 1e4;
    neval = 0; ier = 0; last=0;
  }
  void setWorkspace(int subdivisions = 100) {
     limit = subdivisions; lenw = 4 * limit;
     iwork.resize(limit);
     work.resize(lenw);
  }
  Type a, b, bound;
  int inf;
  void setBounds(Type a_, Type b_) {
    int a_finite = (a_ != -INFINITY) && (a_ != INFINITY);
    int b_finite = (b_ != -INFINITY) && (b_ != INFINITY);
    if      ( a_finite &&  b_finite) { inf =  0; a = a_; b = b_; }
    else if ( a_finite && !b_finite) { inf =  1; bound = a_; }
    else if (!a_finite &&  b_finite) { inf = -1; bound = b_; }
    else                             { inf =  2; }
  }
  /** \brief Constructor
      \param f_ Functor integrand
      \param a_ Lower integration limit. Negative infinity allowed.
      \param b_ Upper integration limit. Positive infinity allowed.
      \param c_ Control parameters for accuracy.
  */
  Integral(Integrand f_, Type a_, Type b_,
	   control c = control()
	   ) : fn(f_) {
    setAccuracy(c.reltol, c.abstol);
    setWorkspace(c.subdivisions);
    setBounds(a_, b_);
  }
  Type operator()() {
    if (inf)
      Rdqagi(fn, NULL, &bound, &inf, &epsabs, &epsrel, &result, &abserr,
	     &neval, &ier, &limit, &lenw, &last, &iwork[0], &work[0]);
    else
      Rdqags(fn, NULL, &a, &b, &epsabs, &epsrel, &result, &abserr,
	     &neval, &ier, &limit, &lenw, &last, &iwork[0], &work[0]);
    return result;
  }
};

/** \brief Integrate function over finite or infinite interval
    \param f Univariate integrand (functor)
    \param a Lower integration limit. Default is negative infinity.
    \param a Upper integration limit. Default is positive infinity.
    \param c Optional control parameters.

    Example:
    \code
    template<class Float>
    struct Gauss_t {
      typedef Float Scalar;
      Float a;  // Parameter
      // Evaluate integrand
      Float operator(Float x) () {
        Float ans = exp(- a*x*x);
        return ans;
      }
      // Integrate wrt x
      Float my_integrate() {
        using gauss_kronrod::integrate;
        Float ans = integrate(*this);
        return ans;
      }
    };
    \endcode
*/
template<class Integrand>
typename Integrand::Scalar integrate(Integrand f,
				     typename Integrand::Scalar a = -INFINITY,
				     typename Integrand::Scalar b =  INFINITY,
				     control c = control() ) {
  Integral< Integrand > I(f, a, b, c);
  return I();
}

/**
   \brief Multivariate integral class.

   Takes Integrand (functor) as template parameter. Integrand must be
   a template class where:

   1. Template parameter is the integrand type.
   2. Contains a 'typedef Type Scalar;'
   3. Has an evaluation operator with void input.

   Use this class if multiple integrals must be computed with
   different parameters. Otherwise use the 'mvIntegrate' function (see
   below).
*/
template<class Integrand>
struct mvIntegral {
  typedef typename Integrand::Scalar Scalar;
  struct evaluator {
    typedef typename Integrand::Scalar Scalar;
    Integrand &f;
    Scalar &x; // Integration variable
    evaluator(Integrand &f_,
	      Scalar &x_ ) : f(f_), x(x_) {}
    Scalar operator()(const Scalar &x_) {
      x = x_;
      return f();
    }
  } ev;
  control c;
  Integral<evaluator> I;
  mvIntegral(Integrand &f_,
	     Scalar &x_,
	     Scalar a= -INFINITY,
	     Scalar b=  INFINITY,
	     control c_ = control()) :
    ev(f_, x_), c(c_), I(ev, a, b, c_) {}
  Scalar operator()() { return I(); }
  /** \brief With respect to */
  mvIntegral<mvIntegral> wrt(Scalar &x,
			     Scalar a= -INFINITY,
			     Scalar b=  INFINITY) {
    return mvIntegral<mvIntegral>(*this, x, a, b, c);
  }
};


template<class Integrand>
struct mvIntegral0 {
  typedef typename Integrand::Scalar Scalar;
  Integrand &f;
  control c;
  mvIntegral0(Integrand &f_,
	      control c_) : f(f_), c(c_) {}
  /** \brief With respect to */
  mvIntegral<Integrand> wrt(Scalar &x,
			    Scalar a= -INFINITY,
			    Scalar b=  INFINITY) {
    return mvIntegral<Integrand>(f, x, a, b, c);
  }
};
/** \brief Multivariate integration
    \param f Multivariate integrand (functor)
    \param c Optional control parameters

    Example:
    \code
    template<class Float>
    struct Gauss2D_t {
      typedef Float Scalar;
      Float a, b;  // Parameters
      Float x, y;  // Integration variables
      // Evaluate integrand (u1,u2)
      Float operator() () {
        Float ans = exp(- a*x*x - b*y*y);
        return ans;
      }
      // Integrate wrt (x,y)
      Float my_integrate() {
        using gauss_kronrod::mvIntegrate;
        Float ans = mvIntegrate(*this).
	  wrt(x, -INFINITY, INFINITY).
	  wrt(y, -INFINITY, INFINITY) ();
        return ans;
      }
    };
    \endcode
*/
template<class Integrand>
mvIntegral0<Integrand> mvIntegrate(Integrand &f,
				   control c = control() ) {
  return mvIntegral0<Integrand> (f, c);
}

} // End namespace gauss_kronrod

// using gauss_kronrod::Integral;
// using gauss_kronrod::integrate;
// using gauss_kronrod::mvIntegrate;

#endif
