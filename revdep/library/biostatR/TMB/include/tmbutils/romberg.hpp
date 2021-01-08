// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/**
   \brief Univariate and multivariate numerical integration

   - Based on CppAD's Romberg integration routines. Adapted so easy to
    use with TMB, e.g. allow multivariate dimension to be unknown at
    compile time.
*/
namespace romberg {
  /** \brief 1D numerical integration using Romberg's method.

      \param f Univariate functor
      \param a Lower scalar integration limit
      \param b Upper scalar integration limit
      \param n Subdivisions (\f$2^{n-1}+1\f$ function evaluations).

      \note The method is not adaptive and the user may have to specify a larger n.

      Example:

      \code
      #include <TMB.hpp>

      template<class Type>
      struct univariate {
        Type theta;               // Parameter in integrand
        univariate(Type theta_)   // Constructor of integrand
        : theta (theta_) {}       // Initializer list
        Type operator()(Type x){  // Evaluate integrand
          return exp( -theta * (x * x) );
        }
      };

      template<class Type>
      Type objective_function<Type>::operator() () {
        DATA_SCALAR(a);
        DATA_SCALAR(b);
        PARAMETER(theta);
        univariate<Type> f(theta);
        Type res = romberg::integrate(f, a, b);
	return res;
      }
      \endcode
  */
  template<class Type, class F>
  Type integrate(F f, Type a, Type b, int n = 7, int p = 2){
    Type e;
    return CppAD::RombergOne(f, a, b, n, p, e);
  }

  /* Construct 1D slice of vector function */
  template<class Type, class F>
  struct slicefun{
    F* f;
    vector<Type> x;
    int slice;
    slicefun(F* f_, vector<Type> x_, int slice_) {
      f=f_; x=x_; slice=slice_;
    }
    Type operator()(Type y){
      x(slice) = y;
      return f->operator()(x);
    }
  };

  /* integrate along i'th slice, f vector function */
  template<class Type, class F>
  Type integrate_slice(F &f, vector<Type> x, int i, Type a, Type b, int n=7, int p=2){
    return integrate(slicefun<Type, F>(&f, x, i), a, b, n, p);
  }

  template<class Type>
  struct multivariate_integrand{
    virtual Type operator()(vector<Type>) = 0;
  };

  template<class Type>
  struct integratelast : multivariate_integrand<Type>{
    slicefun<Type,multivariate_integrand<Type> >* fslice;
    int n, p;
    Type a, b;
    integratelast(multivariate_integrand<Type> &f, int dim, Type a_, Type b_, int n_ = 7, int p_ = 2){
      int slice = dim - 1;
      vector<Type> x(dim);
      fslice = new slicefun<Type,multivariate_integrand<Type> >(&f, x, slice);
      n=n_; p=p_;
      a=a_; b=b_;
    }
    ~integratelast(){delete fslice;}
    Type operator()(vector<Type> y){
      for(int i=0; i < y.size(); i++) fslice->x[i] = y[i];
      return integrate(*fslice, a, b, n, p);
    }
  };

  template<class Type>
  Type integrate_multi(multivariate_integrand<Type> &f, vector<Type> a, vector<Type> b, int n = 7, int p = 2){
    int dim = a.size();
    integratelast<Type> ffirst(f, dim, a(dim - 1), b(dim - 1), n, p);
    if(dim == 1){
      vector<Type> y(0);
      return ffirst(y);
    } else {
      vector<Type> afirst = a.segment(0, dim - 1);
      vector<Type> bfirst = b.segment(0, dim - 1);
      return integrate_multi(ffirst, afirst, bfirst, n, p);
    }
  }

  /** \brief Multi-dimensional numerical integration using Romberg's method.
      Dimension need not be known at compile time.

      \param f Multivariate functor
      \param a Lower vector integration limit
      \param b Upper vector integration limit
      \param n Subdivisions per dimension (\f$(2^{n-1}+1)^d\f$ function evaluations).

      \note The method is not adaptive and the user may have to specify a larger n.

      Example:

      \code
      #include <TMB.hpp>

      template<class Type>
      struct multivariate {
        Type theta;               // Parameter in integrand
        multivariate(Type theta_) // Constructor of integrand
        : theta (theta_) {}       // Initializer list
        Type operator()           // Evaluate integrand
	(vector<Type> x){
          return exp( -theta * (x * x).sum() );
        }
      };

      template<class Type>
      Type objective_function<Type>::operator() () {
        DATA_VECTOR(a);
        DATA_VECTOR(b);
        PARAMETER(theta);
        multivariate<Type> f(theta);
        Type res = romberg::integrate(f, a, b);
        return res;
      }
      \endcode
  */
  template<class Type, class F>
  Type integrate(F f, vector<Type> a, vector<Type> b, int n=7, int p=2){
    // Wrap f in container
    struct integrand : multivariate_integrand<Type>{
      F f;
      integrand(F f_) : f(f_) {}
      Type operator()(vector<Type> x){
	return f(x);
      }
    };
    integrand f_cpy(f);
    return integrate_multi(f_cpy, a, b, n, p);
  }
}
