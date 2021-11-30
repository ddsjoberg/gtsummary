// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/*
    Given function f0. Define recursively higher order reverse
    mode derivatives:

    f0: R^(n)         ->  R^(m)     (  x           -> f0 (x)    )
    f1: R^(n+m)       ->  R^(n)     ( (x,w1)       -> f0'(x)*w1 )
    f2: R^(n+m+n)     ->  R^(n+m)   ( (x,w1,w2)    -> f1'(x)*w2 )
    f3: R^(n+m+n+n+m) ->  R^(n+m+n) ( (x,w1,w2,w3) -> f2'(x)*w3 )

    1. We define a 'generalized symbol' to represent all of these.
    _Reverse_mode_AD_ is trivially obtained for this symbol by calling
    itself on a higher level. Each occurance on the tape will occupy
    O(n+m) memory units independent on the number of flops performed
    by f0.

    2. _Double_versions_ of the generalized symbol are obtained using
    nested AD types to tape f0, then recursively tape forward and
    reverse mode sweeps.

    Finally, given (1) and (2) the macro TMB_ATOMIC_VECTOR_FUNCTION
    will generate the atomic symbol.
*/

/* general helper functions */
namespace atomic{
/**
    \name User defined atomic functions
\note The following procedure is automatically performed with the macro REGISTER_ATOMIC.

\verbatim
    Given function f0. Define recursively higher order reverse
    mode derivatives:

    f0: R^(n)         ->  R^(m)     (  x           -> f0 (x)    )
    f1: R^(n+m)       ->  R^(n)     ( (x,w1)       -> f0'(x)*w1 )
    f2: R^(n+m+n)     ->  R^(n+m)   ( (x,w1,w2)    -> f1'(x)*w2 )
    f3: R^(n+m+n+n+m) ->  R^(n+m+n) ( (x,w1,w2,w3) -> f2'(x)*w3 )

    1. We define a 'generalized symbol' to represent all of these.
    _Reverse_mode_AD_ is trivially obtained for this symbol by calling
    itself on a higher level. Each occurance on the tape will occupy
    O(n+m) memory units independent of the number of flops performed
    by f0.

    2. _Double_versions_ of the generalized symbol are obtained using
    nested AD types to tape f0, then recursively tape forward and
    reverse mode sweeps.

    Finally, given (1) and (2) the macro TMB_ATOMIC_VECTOR_FUNCTION
    will generate the atomic symbol.
\endverbatim
    @{
*/

  /** \brief Construct a tape of a given template _functor_
     (Will be used to tape 'f0' for different nested AD types) */
  template <class Base, class Func>
  CppAD::ADFun<Base>* generate_tape(Func f, vector<double> x_){
    Rcout << "Generating tape\n";
    int n=x_.size();
    vector<AD<Base> > x(n);
    for(int i=0;i<n;i++)x[i]=AD<Base>(x_[i]);
    CppAD::Independent(x);
    vector<AD<Base> > y=f(x);
    vector<AD<Base> > y2(y.size());
    for(int i=0;i<y.size();i++)y2[i]=y[i];
    CppAD::ADFun<Base>* padf=new CppAD::ADFun<Base>(x,y2);
    return padf;
  }
  /** \brief Lift tape of fn up one level by taping forward and reverse sweeps.
     Note: x_ needs only have length equal to the input domain dimension
     of f0. Zeros are filled in for all range directions.
  */
  template <class Base>
  CppAD::ADFun<Base>* forrev(CppAD::ADFun<AD<Base> >* padf, vector<double> x_){
    size_t n=padf->Domain();
    size_t m=padf->Range();
    vector<AD<Base> > x(n+m);
    vector<AD<Base> > y(n);
    for(int i=0;i<x_.size();i++)x[i]=AD<Base>(x_[i]);
    for(int i=x_.size();i<x.size();i++)x[i]=AD<Base>(0);
    vector<AD<Base> > tmp1(n);
    vector<AD<Base> > tmp2(m);
    CppAD::Independent(x);
    for(size_t i=0;i<n;i++)tmp1[i]=x[i];
    for(size_t i=0;i<m;i++)tmp2[i]=x[i+n];
    padf->Forward(0,tmp1);
    y = padf->Reverse(1,tmp2);
    CppAD::ADFun<Base>* padf2=new CppAD::ADFun<Base>(x,y);
    delete padf;
    return padf2;
  }
  /** \brief Recursively apply forrev until the lowest Base level (double) */
  template <class ADBase>
  CppAD::ADFun<double>* multi_forrev(CppAD::ADFun<ADBase>* padf, vector<double> x_){
    return multi_forrev(forrev(padf, x_), x_);
  }
  template <>
  CppAD::ADFun<double>* multi_forrev<double>(CppAD::ADFun<double>* padf, vector<double> x_) CSKIP({
    return padf;
  })
  /** \brief Tape symbol up to any order */
  template<class Func>
  CppAD::ADFun<double>* tape_symbol(Func f, vector<double> x){
    typedef typename Func::ScalarType::value_type Base;
    CppAD::ADFun<Base>* f0=generate_tape<Base>(f,x);
    CppAD::ADFun<double>* fn=multi_forrev(f0,x);
    return fn;
  }
#ifdef _OPENMP
#define NTHREADS omp_get_max_threads()
#define THREAD omp_get_thread_num()
#else
#define NTHREADS 1
#define THREAD 0
#endif
  /** \brief General class to construct 'double versions' of the 
     generalized symbol. */
  template<template<class> class UserFunctor>
  struct forrev_derivatives{
    bool initialized;
    int n,m;
    forrev_derivatives(){
      initialized=false;
    }
    /* ADFun pointers used by the double versions 
       indexed as vpf[thread][level] */
    CppAD::vector<CppAD::vector<CppAD::ADFun<double>* > > vpf;
    void cpyADfunPointer(CppAD::ADFun<double>* padf, int i){
      padf->optimize();
      vpf[0][i] = padf;
      /* Copy object for other threads */
      for(int thread=1;thread<NTHREADS;thread++){
	vpf[thread][i]=new CppAD::ADFun<double>();
	vpf[thread][i]->operator=(*padf);
      }
    }
    void do_init(vector<double> x){
      UserFunctor<double> f;
      n=x.size();
      m=f(x).size();
      UserFunctor<AD<double> > f0;
      UserFunctor<AD<AD<double> > > f1;
      UserFunctor<AD<AD<AD<double> > > > f2;
      UserFunctor<AD<AD<AD<AD<double> > > > > f3;
      vpf.resize(NTHREADS);
      for(int thread=0;thread<NTHREADS;thread++){
        vpf[thread].resize(4);
      }
      cpyADfunPointer(tape_symbol(f0,x), 0);
      cpyADfunPointer(tape_symbol(f1,x), 1);
      cpyADfunPointer(tape_symbol(f2,x), 2);
      cpyADfunPointer(tape_symbol(f3,x), 3);
    }
    void init(vector<double> x){
      if(!initialized){
	do_init(x);
	initialized=true;
      }
    }
    int get_output_dim(int input_dim){
      int output_dim=-1;
      // Fibonacci type recursion for each 'column'
      if      (input_dim == n)         output_dim = m;
      else if (input_dim == n+m)       output_dim = n;
      else if (input_dim == n+m+n)     output_dim = n+m;
      else if (input_dim == n+m+n+n+m) output_dim = n+m+n;
      else Rf_error("get_output_dim failed");
      return output_dim;
    }
    // Calculate level from input dimension
    int get_level(int input_dim){
      int level=-1;
      if      (input_dim == n)         level = 0;
      else if (input_dim == n+m)       level = 1;
      else if (input_dim == n+m+n)     level = 2;
      else if (input_dim == n+m+n+n+m) level = 3;
      else Rf_error("get_level failed");
      return level;
    }
    // Evaluate
    CppAD::vector<double> operator()(CppAD::vector<double> tx){
      int level = get_level(tx.size());
      return vpf[THREAD][level]->Forward(0,tx);
    }
  }; /* end class forrev_derivatives */
#undef NTHREADS
#undef THREAD

/** \brief Wrap user function into a functor, generate double versions,
   and construct atomic function in a namespace */
#define REGISTER_ATOMIC(USERFUNCTION)					\
namespace USERFUNCTION##NAMESPACE{					\
  template<class Type>							\
  struct UserFunctor{							\
    typedef Type ScalarType;						\
    vector<Type> operator()(vector<Type> x){				\
      return USERFUNCTION(x);						\
    }									\
  };									\
  atomic::forrev_derivatives<UserFunctor> double_version;		\
  TMB_ATOMIC_VECTOR_FUNCTION(						\
			     generalized_symbol				\
			     ,						\
			     double_version.get_output_dim(tx.size())	\
			     ,						\
			     ty = double_version(tx);			\
			     ,						\
			     CppAD::vector<Type> concat(tx.size() + py.size());	\
			     for(size_t i=0; i < tx.size(); i++) concat[i] = tx[i]; \
			     for(size_t i=0; i < py.size(); i++) concat[tx.size()+i] = py[i]; \
			     px = generalized_symbol(concat);		\
			     )						\
  template<class Base>							\
  vector<Base> generalized_symbol(vector<Base> x){			\
    CppAD::vector<Base> xx(x.size());					\
    for(int i=0;i<x.size();i++)xx[i]=x[i];				\
    CppAD::vector<Base> yy=generalized_symbol(xx);			\
    vector<Base> y(yy.size());						\
    for(int i=0;i<y.size();i++)y[i]=yy[i];				\
    return y;								\
  }									\
}									\
vector<double> USERFUNCTION(vector<double> x){				\
  USERFUNCTION##NAMESPACE::double_version.init(x);			\
  return USERFUNCTION##NAMESPACE::generalized_symbol(x);		\
}									\
vector<AD<double> > USERFUNCTION(vector<AD<double> > x){		\
  return USERFUNCTION##NAMESPACE::generalized_symbol(x);		\
}									\
vector<AD<AD<double> > > USERFUNCTION(vector<AD<AD<double> > > x){	\
  return USERFUNCTION##NAMESPACE::generalized_symbol(x);		\
}									\
vector<AD<AD<AD<double> > > > USERFUNCTION(vector<AD<AD<AD<double> > > > x){ \
  return USERFUNCTION##NAMESPACE::generalized_symbol(x);		\
}

/**
   @}
*/
} /* end namespace atomic */
