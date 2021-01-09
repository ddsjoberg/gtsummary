#ifndef UTILS_H
#define UTILS_H

#include "tntsupp.h"
#include "geese.h"

void VecPrint(const DVector &v);

Fortran_Matrix<double> ident (int n);

Fortran_Matrix<double> MatRowCol(const Fortran_Matrix<double> &mat, const Vector<double> &r, const Vector<double> &c);

Fortran_Matrix<double> rho2mat(const Vector<double> &rho);

//solve(a, b = ident(n))
DMatrix solve(const DMatrix &a, const DMatrix &b); 

DVector solve(const DMatrix &A, const DVector &b);

DMatrix solve(const DMatrix &a);

DMatrix AtBiC(const DMatrix &A, const DMatrix &B, const DMatrix &C);

DVector AtBiC(const DMatrix &A, const DMatrix &B, const DVector &C);

DMatrix apply_elwise(const DMatrix &x, double f(double));

DVector apply_elwise(const DVector &x, double f(double));

DVector sqrt(const DVector &x);

double square(double x);

DVector square(const DVector &x);

double reciproot(double x);

DVector reciproot(const DVector &x);

double recip(double x);

DVector recip(const DVector &x);

int cluscount(DVector &ID);

Vector<int> clussize(DVector &ID);

DVector SMult(const DVector &v1, const DVector &v2);

DMatrix SMult(const DVector &v, const DMatrix &m);

DMatrix operator*(const DVector &v, const DMatrix &m);

DMatrix diag(const DVector &v);

DVector diag(const DMatrix &m);

DMatrix inv(const DMatrix &x);

DMatrix fabs(const DMatrix &m);

DVector fabs(const DVector &v);

#endif //UTILS_H
