// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file 
   \brief Matrix exponential.

*/

/** \brief Matrix exponential: matrix of arbitrary dimension. */
template <class scalartype, int dim>
struct matexp{
  typedef Matrix<scalartype,dim,dim> matrix;
  typedef Matrix<std::complex<scalartype> ,dim,dim> cmatrix;
  typedef Matrix<std::complex<scalartype> ,dim,1> cvector;
  cmatrix V;
  cmatrix iV;
  cvector lambda;
  EigenSolver< matrix > eigensolver;
  matexp(){};
  matexp(matrix A_){
    eigensolver.compute(A_);
    V=eigensolver.eigenvectors();
    lambda=eigensolver.eigenvalues();
    iV=V.inverse();
  }
  matrix operator()(scalartype t){
    cmatrix tmp;
    tmp.setZero();
    matrix ans;
    for(int i=0;i<dim;i++)tmp(i,i)=exp(lambda(i)*t);
    //tmp=V*tmp*iV;
    tmp=tmp.operator*(iV);
    tmp=V.operator*(tmp);

    for(int i=0;i<dim;i++)
      for(int j=0;j<dim;j++)
	ans(i,j)=std::real(tmp(i,j));
    return ans;
  }
};

/** \brief Matrix exponential: 2x2 case which can be handled efficiently */
template <class scalartype>
struct matexp<scalartype,2>{
  typedef std::complex<scalartype> complex;
  typedef Matrix<scalartype,2,2> matrix;
  typedef Matrix<complex ,2,2> cmatrix;
  typedef Matrix<complex ,2,1> cvector;
  cmatrix V;
  cmatrix iV;
  cvector lambda;
  matexp(){};
  matexp(matrix A_){
    complex T=scalartype(0.5)*A_.trace(); /* half trace */
    complex D=A_.determinant();
    lambda << T+sqrt(T*T-D) , T-sqrt(T*T-D);
    scalartype a=A_(0,0);
    scalartype b=A_(0,1);
    V << b, b, lambda(0)-a, lambda(1)-a;
    iV=V.inverse();
  }
  matrix operator()(scalartype t){
    int dim=2;
    cmatrix tmp;
    tmp.setZero();
    matrix ans;
    for(int i=0;i<dim;i++)tmp(i,i)=exp(lambda(i)*t);
    //tmp=V*tmp*iV;
    tmp=tmp.operator*(iV);
    tmp=V.operator*(tmp);

    for(int i=0;i<dim;i++)
      for(int j=0;j<dim;j++)
	ans(i,j)=std::real(tmp(i,j));
    //ans(i,j)=(tmp(i,j));
    return ans;
  }
};
