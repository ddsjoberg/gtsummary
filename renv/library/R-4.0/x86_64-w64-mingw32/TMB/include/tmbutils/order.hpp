// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file 
   \brief Taped sorting of a vector.

   Example:
   \code
   order<Type> perm(x);
   vector<Type> xsort=perm(x);
   \endcode

 */
template <class Type>
class order{
public:
  vector<Type> iperm;
  matrix<Type> P;
  int n;
  order(vector<Type> x){
    Type Zero=0;
    Type One=1;
    n=x.size();
    iperm.resize(n);
    iperm.setZero();
    P.resize(n,n);
    P.setZero();
    /* Permutation vector - n^2 comparisons */
    for(int i=0;i<n;i++)
      for(int j=0;j<n;j++)
	iperm[i]+=CppAD::CondExpLt(x[j], x[i], One, Zero );
    /* Ties correction - n^2/2 comparisons */
    for(int i=0;i<n;i++)
      for(int j=0;j<i;j++)
	iperm[i]+=CppAD::CondExpEq(x[j], x[i], One, Zero );
    /* Corresponding permutation matrix - n^2 comparisons */
    for(int i=0;i<n;i++)
      for(int j=0;j<n;j++)
	P(j,i)=CppAD::CondExpEq(Type(j), iperm[i], One, Zero );
  }
  /* Apply permutation on other vector */
  vector<Type> operator()(vector<Type> x){
    vector<Type> y(x.size());
    y.setZero();
    /* y=P%*%x  - n^2 flops */
    for(int i=0;i<n;i++)
      for(int j=0;j<n;j++)
	y[i]+=P(i,j)*x[j];
    return y;
  }  
  /* Apply permutation on outer dimension of array */
  array<Type> operator()(array<Type> x){
    array<Type> y(x);
    y.setZero();
    /* y=P%*%x  - n^2 flops */
    for(int i=0;i<n;i++)
      for(int j=0;j<n;j++)
  	y.row(i)+=x(j)*P(i,j);
    return y;
  }  

};
