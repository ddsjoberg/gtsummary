// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file
  \brief Templates to get convenient R-like syntax. 
*/

/** \brief  Similar to R's split function:  split(x,fac) devides  x  into groups defined by  fac . 
* \details
Returns a "vector of vectors". 
*/
template <class Type>
vector<vector<Type> > split(vector<Type> x, vector<int> fac) {
  if (x.size() != fac.size()) Rf_error("x and fac must have equal length.");
  int nlevels = 0;
  for (int i = 0; i < fac.size(); i++)
    if (fac[i] >= nlevels) nlevels = fac[i] + 1;
  vector<vector<Type> > ans(nlevels);
  vector<int> lngt(nlevels);
  lngt.setZero();
  for (int i = 0; i < fac.size(); i++) lngt[fac[i]]++;
  for (int i = 0; i < nlevels; i++) ans[i].resize(lngt[i]);
  lngt.setZero();
  for (int i = 0; i < fac.size(); i++) {
    ans[fac[i]][lngt[fac[i]]] = x[i];
    lngt[fac[i]]++;
  }
  return ans;
}

/** Sum of vector, matrix or array */
template <template <class> class Vector, class Type>
Type sum(Vector<Type> x) {
  return x.sum();
}

/** Matrix * vector 

  Simplifies syntax in that .matrix() can be avoided. Recall: TMB type vector is of Eigen type Array.
*/
template <class Type>
vector<Type> operator*(matrix<Type> A, vector<Type> x) {
  return A * x.matrix();
}

/** SparseMatrix * vector */
template <class Type>
vector<Type> operator*(Eigen::SparseMatrix<Type> A, vector<Type> x) {
  return (A * x.matrix()).array();
}

/** \brief Approximate normal cumulative distribution function,
    similar to R's pnorm (one-argument case only).

    Has been replaced by more accurate version based on Rmath library.
*/
template <class Type>
Type pnorm_approx(Type x) {
  Type a = 993. / 880.;
  Type b = 89. / 880.;
  x = x / sqrt(Type(2));
  return Type(.5) * tanh((a + b * x * x) * x) + Type(.5);
}
VECTORIZE1_t(pnorm_approx)

/** \brief Approximate inverse normal cumulative distribution
    function, similar to R's qnorm (one-argument case only).

    Has been replaced by more accurate version based on Rmath library.
*/
template <class Type>
Type qnorm_approx(Type x) {
  Type a = 993. / 880.;
  Type b = 89. / 880.;
  Type y = .5 * (log(x) - log(1 - x));
  Type p = a / b;
  Type q = -y / b;
  Type Delta0 = -3 * p;
  Type Delta1 = 27 * q;
  Type C = pow(.5 * Delta1 + .5 * sqrt(pow(Delta1, 2) - 4 * pow(Delta0, 3)),
               Type(1) / Type(3));
  return -(C + Delta0 / C) * sqrt(Type(2)) / Type(3);
}
VECTORIZE1_t(qnorm_approx)

/** Diff of vector

    Difference of vector elements just like diff in R, but only for
    vectors.
*/
template <class Type>
vector<Type> diff(vector<Type> x) {
  int n = x.size();
  vector<Type> ans(n - 1);
  for (int i = 0; i < n - 1; i++) ans[i] = x[i + 1] - x[i];
  return ans;
}

/** Logit

    Calculates the logit transformation; the same as qlogis in base or logit in the boot package in R.
*/
template<class Type>
Type logit(Type x){
  return log(x/(Type(1.0)-x));
}
VECTORIZE1_t(logit)

/** Inverse Logit

    Calculates the inverse of the logit transformation; the same as plogis in base or inv.logit in the boot package in R.
*/
template<class Type>
Type invlogit(Type x){
  return Type(1.0)/(Type(1.0)+exp(-x));
}
VECTORIZE1_t(invlogit)

/** Matern correlation function

    Compute values of the Matern correlation function for given distances and parameters. Same as 'matern' from the geoR package.

    \param u Distance.
    \param phi Range parameter.
    \param kappa Smoothness parameter.
    \ingroup special_functions
*/
template<class Type>
Type matern(Type u, Type phi, Type kappa){
  Type x = CppAD::CondExpEq(u, Type(0), Type(1), u / phi); /* Avoid NaN when u=0 */
  Type ans = 1.0 / ( exp(lgamma(kappa)) * pow(2, kappa - 1.0) ) * pow(x, kappa) * besselK(x, kappa);
  return CppAD::CondExpEq(u, Type(0), Type(1), ans);
}

/** 'squeeze' transform : [0,1] -> (0,1) to machine tolerance */
template<class Type>
Type squeeze(Type u){
  Type eps = std::numeric_limits<double>::epsilon();
  u = (1.0 - eps) * (u - .5) + .5;
  return u;
}

/** Taped maximum of vector */
template <class Type>
Type max(const vector<Type> &x)
{
  Type res = x[0];
  for(int i=0; i < x.size(); i++){
    res = CppAD::CondExpGt(res, x[i], res, x[i]);
  }
  return res;
}

/** Taped minimum of vector */
template <class Type>
Type min(const vector<Type> &x)
{
  Type res = x[0];
  for(int i = 0; i < x.size(); i++){
    res = CppAD::CondExpLt(res, x[i], res, x[i]);
  }
  return res;
}

/** \brief Addition in log-space

    Calculates \f$log( exp(logx) + exp(logy) )\f$ without causing
    unnecessary overflows or throwing away too much accuracy (see
    'writing R-extensions').
    The following reductions are applied while taping:

    - `log(exp(logx) + exp(-INFINITY)) = logx`
    - `log(exp(-INFINITY) + exp(logy)) = logy`

    \param logx The logarithm of x.
    \param logy The logarithm of y.

    \ingroup special_functions
*/
template<class Type>
Type logspace_add(Type logx, Type logy) {
  if ( !CppAD::Variable(logx) && logx == Type(-INFINITY) )
    return logy;
  if ( !CppAD::Variable(logy) && logy == Type(-INFINITY) )
    return logx;
  CppAD::vector<Type> tx(3);
  tx[0] = logx;
  tx[1] = logy;
  tx[2] = 0; // order
  return atomic::logspace_add(tx)[0];
}

/** \brief Subtraction in log-space

    Calculates \f$log( exp(logx) - exp(logy) )\f$ without causing
    unnecessary overflows or throwing away too much accuracy (see
    'writing R-extensions').

    \param logx The logarithm of x.
    \param logy The logarithm of y.

    \ingroup special_functions
*/
template<class Type>
Type logspace_sub(Type logx, Type logy) {
  CppAD::vector<Type> tx(3);
  tx[0] = logx;
  tx[1] = logy;
  tx[2] = 0; // order
  return atomic::logspace_sub(tx)[0];
}
