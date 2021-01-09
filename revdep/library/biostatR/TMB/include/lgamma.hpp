// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file
    \brief Gamma function and gamma probability densities
*/

/** \brief Logarithm of gamma function (following R argument convention).
    \ingroup special_functions
*/
template<class Type>
Type lgamma(Type x){
  CppAD::vector<Type> tx(2);
  tx[0] = x;
  tx[1] = Type(0);
  return atomic::D_lgamma(tx)[0];
}
VECTORIZE1_t(lgamma)

/** \brief Logarithm of factorial function (following R argument convention).
    \ingroup special_functions
*/
template<class Type>
Type lfactorial(Type x){
  CppAD::vector<Type> tx(2);
  tx[0] = x + Type(1);
  tx[1] = Type(0);
  return atomic::D_lgamma(tx)[0];
}
VECTORIZE1_t(lfactorial)

/* Old lgamma approximation */
template <class Type>
inline Type lgamma_approx(const Type &y)
{
  /* coefficients for gamma=7, kmax=8  Lanczos method */
  static const Type
    LogRootTwoPi_ = 0.9189385332046727418,
    lanczos_7_c[9] = {
      0.99999999999980993227684700473478,
      676.520368121885098567009190444019,
      -1259.13921672240287047156078755283,
      771.3234287776530788486528258894,
      -176.61502916214059906584551354,
      12.507343278686904814458936853,
      -0.13857109526572011689554707,
      9.984369578019570859563e-6,
      1.50563273514931155834e-7
    };
  Type x=y;
  int k;
  Type Ag;
  Type term1, term2;
  x -= Type(1.0); /* Lanczos writes z! instead of Gamma(z) */
  Ag = lanczos_7_c[0];
  for(k=1; k<=8; k++) { Ag += lanczos_7_c[k]/(x+k); }
  /* (x+0.5)*log(x+7.5) - (x+7.5) + LogRootTwoPi_ + log(Ag(x)) */
  term1 = (x+Type(0.5))*log((x+Type(7.5))/Type(M_E));
  term2 = LogRootTwoPi_ + log(Ag);
  return term1 + (term2 - Type(7.0));
}

/** \brief Negative binomial probability function.
  \ingroup R_style_distribution

    Parameterized through size and prob parameters, following R-convention.
*/
template<class Type>
inline Type dnbinom(const Type &x, const Type &size, const Type &prob,
		    int give_log=0)
{
  Type n=size;
  Type p=prob;
  Type logres = lgamma(x+n)-lgamma(n)-lgamma(x+Type(1))+
    n*log(p)+x*log(Type(1)-p);
  if (give_log) return logres; else return exp(logres);
}
VECTORIZE4_ttti(dnbinom)

/** \brief Negative binomial probability function.
  \ingroup R_style_distribution

    Alternative parameterization through mean and variance parameters.
*/
template<class Type>
inline Type dnbinom2(const Type &x, const Type &mu, const Type &var,
		    int give_log=0)
{
  Type p=mu/var;
  Type n=mu*p/(Type(1)-p);
  return dnbinom(x,n,p,give_log);
}
VECTORIZE4_ttti(dnbinom2)

/** \brief Negative binomial probability function.

    More robust parameterization through \f$log(\mu)\f$ and
    \f$log(\sigma^2-\mu)\f$ parameters.

    \ingroup R_style_distribution
*/
template<class Type>
inline Type dnbinom_robust(const Type &x,
                           const Type &log_mu,
                           const Type &log_var_minus_mu,
                           int give_log=0)
{
  CppAD::vector<Type> tx(4);
  tx[0] = x;
  tx[1] = log_mu;
  tx[2] = log_var_minus_mu;
  tx[3] = 0;
  Type ans = atomic::log_dnbinom_robust(tx)[0];
  return ( give_log ? ans : exp(ans) );
}
VECTORIZE4_ttti(dnbinom_robust)

/** \brief Poisson probability function. 
  \ingroup R_style_distribution
*/

template<class Type>
inline Type dpois(const Type &x, const Type &lambda, int give_log=0)
{
  Type logres = -lambda + x*log(lambda) - lgamma(x+Type(1));
  if (give_log) return logres; else return exp(logres);
}
VECTORIZE3_tti(dpois)

/** \brief Density of X where X~gamma distributed 
  \ingroup R_style_distribution
*/
template<class Type>
Type dgamma(Type y, Type shape, Type scale, int give_log=0)
{
  Type logres=-lgamma(shape)+(shape-Type(1.0))*log(y)-y/scale-shape*log(scale);
  if(give_log)return logres; else return exp(logres);
}
VECTORIZE4_ttti(dgamma)

/** \brief Density of log(X) where X~gamma distributed 
  \ingroup R_style_distribution
*/
template<class Type>
inline Type dlgamma(Type y, Type shape, Type scale, int give_log=0)
{
  Type logres=-lgamma(shape)-shape*log(scale)-exp(y)/scale+shape*y;
  if(give_log)return logres; else return exp(logres);
}
VECTORIZE4_ttti(dlgamma)

/** \brief Zero-Inflated Poisson probability function. 
  \ingroup R_style_distribution
* \details
    \param zip is the probaility of having extra zeros 
*/
template<class Type>
inline Type dzipois(const Type &x, const Type &lambda, const Type &zip, int give_log=0)
{
  Type logres;
  if (x==Type(0)) logres=log(zip + (Type(1)-zip)*dpois(x, lambda, false)); 
  else logres=log(Type(1)-zip) + dpois(x, lambda, true);
  if (give_log) return logres; else return exp(logres);
}
VECTORIZE4_ttti(dzipois)

/** \brief Zero-Inflated negative binomial probability function. 
  \ingroup R_style_distribution
* \details
    Parameterized through size and prob parameters, following R-convention.
    No vectorized version is currently available.
    \param zip is the probaility of having extra zeros 
*/
template<class Type>
inline Type dzinbinom(const Type &x, const Type &size, const Type &p, const Type & zip,
		    int give_log=0)
{
  Type logres;
  if (x==Type(0)) logres=log(zip + (Type(1)-zip)*dnbinom(x, size, p, false)); 
  else logres=log(Type(1)-zip) + dnbinom(x, size, p, true);
  if (give_log) return logres; else return exp(logres);
}

/** \brief Zero-Inflated negative binomial probability function. 
  \ingroup R_style_distribution
* \details
    Alternative parameterization through mean and variance parameters (conditional on not being an extra zero).
    No vectorized version is currently available.
    \param zip is the probaility of having extra zeros 
*/
template<class Type>
inline Type dzinbinom2(const Type &x, const Type &mu, const Type &var, const Type & zip,
		    int give_log=0)
{
  Type p=mu/var;
  Type n=mu*p/(Type(1)-p);
  return dzinbinom(x,n,p,zip,give_log);
}

/********************************************************************/
/* SIMULATON CODE                                                   */
/********************************************************************/

extern "C" {
  double Rf_rnbinom(double n, double p);
}
/** \brief Simulate from a negative binomial distribution  */
template<class Type>
Type rnbinom(Type n, Type p)
{
  return Rf_rnbinom(asDouble(n), asDouble(p));
}
VECTORIZE2_tt(rnbinom)
VECTORIZE2_n(rnbinom)

/** \brief Simulate from a negative binomial distribution  */
template<class Type>
Type rnbinom2(Type mu, Type var)
{
  Type p = mu / var;
  Type n = mu * p / (Type(1) - p);
  return Rf_rnbinom(asDouble(n), asDouble(p));
}
VECTORIZE2_tt(rnbinom2)
VECTORIZE2_n(rnbinom2)
