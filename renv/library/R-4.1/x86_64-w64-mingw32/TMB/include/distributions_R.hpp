// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/**	\file
	\brief Probability distribution functions.
	*/

/** \brief Distribution function of the normal distribution (following R argument convention).
    \ingroup R_style_distribution
*/
template<class Type>
Type pnorm(Type q, Type mean = 0., Type sd = 1.){
  CppAD::vector<Type> tx(1);
  tx[0] = (q - mean) / sd;
  return atomic::pnorm1(tx)[0];
}
VECTORIZE3_ttt(pnorm)
VECTORIZE1_t(pnorm)

/** \brief Quantile function of the normal distribution (following R argument convention).
    \ingroup R_style_distribution
*/
template<class Type>
Type qnorm(Type p, Type mean = 0., Type sd = 1.){
  CppAD::vector<Type> tx(1);
  tx[0] = p;
  return sd*atomic::qnorm1(tx)[0] + mean;
}
VECTORIZE3_ttt(qnorm)
VECTORIZE1_t(qnorm)

/** \brief Distribution function of the gamma distribution (following R argument convention).
    \ingroup R_style_distribution
*/
template<class Type>
Type pgamma(Type q, Type shape, Type scale = 1.){
  CppAD::vector<Type> tx(4);
  tx[0] = q/scale;
  tx[1] = shape;
  tx[2] = Type(0);        // 0'order deriv
  tx[3] = -lgamma(shape); // normalize
  return atomic::D_incpl_gamma_shape(tx)[0];
}
VECTORIZE3_ttt(pgamma)

/** \brief Quantile function of the gamma distribution (following R argument convention).
    \ingroup R_style_distribution
*/
template<class Type>
Type qgamma(Type q, Type shape, Type scale = 1.){
  CppAD::vector<Type> tx(3);
  tx[0] = q;
  tx[1] = shape;
  tx[2] = -lgamma(shape); // normalize
  return atomic::inv_incpl_gamma(tx)[0] * scale;
}
VECTORIZE3_ttt(qgamma)

/** \brief Distribution function of the poisson distribution (following R argument convention).
    \ingroup R_style_distribution
*/
template<class Type>
Type ppois(Type q, Type lambda){
  CppAD::vector<Type> tx(2);
  tx[0] = q;
  tx[1] = lambda;
  return atomic::ppois(tx)[0];
}
VECTORIZE2_tt(ppois)

/** 	@name Exponential distribution.
	Functions relative to the exponential distribution.
	*/
/**@{*/
/**	\brief Cumulative distribution function of the exponential distribution.
	\ingroup R_style_distribution
	\param rate Rate parameter. Must be strictly positive.
	*/
template<class Type> 
Type pexp(Type x, Type rate)
{
	return CppAD::CondExpGe(x,Type(0),1-exp(-rate*x),Type(0));
}

// Vectorize pexp
VECTORIZE2_tt(pexp)

/**	\brief Probability density function of the exponential distribution.
	\ingroup R_style_distribution
	\param rate Rate parameter. Must be strictly positive.
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template<class Type> 
Type dexp(Type x, Type rate, int give_log=0)
{
	if(!give_log)
		return CppAD::CondExpGe(x,Type(0),rate*exp(-rate*x),Type(0));
	else
		return CppAD::CondExpGe(x,Type(0),log(rate)-rate*x,Type(-INFINITY));
}

// Vectorize dexp
VECTORIZE3_tti(dexp)

/**	\brief Inverse cumulative distribution function of the exponential distribution.
	\ingroup R_style_distribution
	\param rate Rate parameter. Must be strictly positive.
	*/
template <class Type>
Type qexp(Type p, Type rate)
{
	return -log(1-p)/rate;
}

// Vectorize qexp.
VECTORIZE2_tt(qexp)
/**@}*/


/**	@name Weibull distribution.
	Functions relative to the Weibull distribution.
	*/
/**@{*/
/** 	\brief Cumulative distribution function of the Weibull distribution.
	\ingroup R_style_distribution
	\param shape Shape parameter. Must be strictly positive.
	\param scale Scale parameter. Must be strictly positive.
	*/
template<class Type> 
Type pweibull(Type x, Type shape, Type scale)
{
	return CppAD::CondExpGe(x,Type(0),1-exp(-pow(x/scale,shape)),Type(0));
}

// Vectorize pweibull
VECTORIZE3_ttt(pweibull)

/** 	\brief Probability density function of the Weibull distribution.
	\ingroup R_style_distribution
	\param shape Shape parameter. Must be strictly positive.
	\param scale Scale parameter. Must be strictly positive.
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template<class Type> 
Type dweibull(Type x, Type shape, Type scale, int give_log=0)
{
	if(!give_log)
		return CppAD::CondExpGe(x,Type(0),shape/scale * pow(x/scale,shape-1) * exp(-pow(x/scale,shape)),Type(0));
	else
		return CppAD::CondExpGe(x,Type(0),log(shape) - log(scale) + (shape-1)*(log(x)-log(scale)) - pow(x/scale,shape),Type(-INFINITY));
}

// Vectorize dweibull
VECTORIZE4_ttti(dweibull)

/**	\brief Inverse cumulative distribution function of the Weibull distribution.
	\ingroup R_style_distribution
	\param p Probability ; must be between 0 and 1.
	\param shape Shape parameter. Must be strictly positive.
	\param scale Scale parameter. Must be strictly positive.
	*/
template<class Type> 
Type qweibull(Type p, Type shape, Type scale)
{
	Type res = scale * pow( (-log(1-p)) , 1/shape );
	res = CppAD::CondExpLt(p,Type(0),Type(0),res);
	res = CppAD::CondExpGt(p,Type(1),Type(0),res);
	return res;
}

// Vectorize qweibull
VECTORIZE3_ttt(qweibull)
/**@}*/

/**	\brief Probability mass function of the binomial distribution.
	\ingroup R_style_distribution
	\param k Number of successes.
	\param size Number of trials.
	\param prob Probability of success.
	\param give_log true if one wants the log-probability, false otherwise.
*/
template<class Type> 
Type dbinom(Type k, Type size, Type prob, int give_log=0)
{
  Type logres = lgamma(size + 1) - lgamma(k + 1) - lgamma(size - k + 1);
  // Add 'k * log(prob)' only if k > 0
  logres += CppAD::CondExpGt(k, Type(0), k * log(prob), Type(0) );
  // Add '(size - k) * log(1 - prob)' only if size > k
  logres += CppAD::CondExpGt(size, k, (size - k) * log(1 - prob), Type(0) );
  if (!give_log) return exp(logres);
  else return logres;
}

// Vectorize dbinom
VECTORIZE4_ttti(dbinom)

/** \brief Density of binomial distribution parameterized via logit(prob)

    This version should be preferred when working on the logit scale
    as it is numerically stable for probabilities close to 0 or 1.

    \ingroup R_style_distribution
*/
template<class Type>
Type dbinom_robust(Type k, Type size, Type logit_p, int give_log=0)
{
  CppAD::vector<Type> tx(4);
  tx[0] = k;
  tx[1] = size;
  tx[2] = logit_p;
  tx[3] = 0;
  Type ans = atomic::log_dbinom_robust(tx)[0]; /* without norm. constant */
  if (size > 1) {
    ans += lgamma(size+1.) - lgamma(k+1.) - lgamma(size-k+1.);
  }
  return ( give_log ? ans : exp(ans) );
}
VECTORIZE4_ttti(dbinom_robust)

/**	\brief Probability density function of the beta distribution.
	\ingroup R_style_distribution
	\param shape1 First shape parameter. Must be strictly positive.
	\param shape2 Second shape parameter. Must be strictly positive.
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template <class Type>
Type dbeta(Type x, Type shape1, Type shape2, int give_log)
{
	Type res = exp(lgamma(shape1+shape2) - lgamma(shape1) - lgamma(shape2)) * pow(x,shape1-1) * pow(1-x,shape2-1);
	if(!give_log) 
		return res;
	else 
		return CppAD::CondExpEq(x,Type(0),log(res),lgamma(shape1+shape2) - lgamma(shape1) - lgamma(shape2) + (shape1-1)*log(x) + (shape2-1)*log(1-x));
}

// Vectorize dbeta
VECTORIZE4_ttti(dbeta)

/**	\brief Probability density function of the Fisher distribution.
	\ingroup R_style_distribution
	\param df1 Degrees of freedom 1.
	\param df2 Degrees of freedom 2.
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template <class Type>
Type df(Type x, Type df1, Type df2, int give_log)
{
	Type logres = lgamma((df1+df2)/2.) - lgamma(df1/2.) - lgamma(df2/2.) + df1/2.*log(Type(df1)/df2) + (df1/2.-1)*log(x) - (df1+df2)/2.*log(1+Type(df1)/df2*x);
	if(!give_log) return exp(logres);
	else return logres;
}

//Vectorize df
VECTORIZE4_ttti(df)

/**	\brief Probability density function of the logistic distribution.
	\ingroup R_style_distribution
	\param location Location parameter.
	\param scale Scale parameter. Must be strictly positive.
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template <class Type>
Type dlogis(Type x, Type location, Type scale, int give_log)
{
	Type logres = -(x-location)/scale - log(scale) - 2*log(1+exp(-(x-location)/scale));
	if(!give_log) return exp(logres);
	else return logres;
}

// Vectorize dlogis
VECTORIZE4_ttti(dlogis)

/**	\brief Probability density function of the skew-normal distribution.
	\ingroup R_style_distribution
	\param alpha Slant parameter.
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template <class Type>
Type dsn(Type x, Type alpha, int give_log=0)
{
	
	if(!give_log) return 2 * dnorm(x,Type(0),Type(1),0) * pnorm(alpha*x);
	else return log(2.0) + log(dnorm(x,Type(0),Type(1),0)) + log(pnorm(alpha*x));
}

// Vectorize dsn
VECTORIZE3_tti(dsn)

/** 	\brief Probability density function of the Student t-distribution.
	\ingroup R_style_distribution
	\param df Degree of freedom.
	\param give_log true if one wants the log-probability, false otherwise.
	*/	
template <class Type>
Type dt(Type x, Type df, int give_log)
{
	Type logres = lgamma((df+1)/2) - Type(1)/2*log(df*M_PI) -lgamma(df/2) - (df+1)/2*log(1+x*x/df);
	if(!give_log) return exp(logres);
	else return logres;
}

// Vectorize dt
VECTORIZE3_tti(dt)

/** 	\brief Probability mass function of the multinomial distribution.
	\ingroup R_style_distribution
	\param x Vector of length K of integers.
        \param p Vector of length K, specifying the probability for the K classes (note, unlike in R these must sum to 1).
	\param give_log true if one wants the log-probability, false otherwise.
	*/
template <class Type>
Type dmultinom(vector<Type> x, vector<Type> p, int give_log=0)
{
	vector<Type> xp1 = x+Type(1);
	Type logres = lgamma(x.sum() + Type(1)) - lgamma(xp1).sum() + (x*log(p)).sum();
	if(give_log) return logres;
	else return exp(logres);
}

/** 	@name Sinh-asinh distribution.
  	Functions relative to the sinh-asinh distribution.
		*/
/**@{*/
/**	\brief Probability density function of the sinh-asinh distribution.
  	\ingroup R_style_distribution
	\param mu Location.
	\param sigma Scale.
	\param nu Skewness.
	\param tau Kurtosis.
	\param give_log true if one wants the log-probability, false otherwise.
					
	Notation adopted from R package "gamlss.dist".
				
	Probability density given in (2) in __Jones and Pewsey (2009) Biometrika (2009) 96 (4): 761-780__.
								
	It is not possible to call this function with nu a vector or tau a vector.
*/
template <class Type>
Type dSHASHo(Type x, Type mu, Type sigma, Type nu, Type tau, int give_log = 0)
{
	// TODO : Replace log(x+sqrt(x^2+1)) by a better approximation for asinh(x).
		
	Type z = (x-mu)/sigma;
   	Type c = cosh(tau*log(z+sqrt(z*z+1))-nu);
   	Type r = sinh(tau*log(z+sqrt(z*z+1))-nu);
   	Type logres = -log(sigma) + log(tau) -0.5*log(2*M_PI) -0.5*log(1+(z*z)) +log(c) -0.5*(r*r);
					   	
  	if(!give_log) return exp(logres);
   	else return logres;
}

// Vectorize dSHASHo
VECTORIZE6_ttttti(dSHASHo)

/**	\brief Cumulative distribution function of the sinh-asinh distribution.
  	\ingroup R_style_distribution
	\param mu Location.
	\param sigma Scale.
	\param nu Skewness.
	\param tau Kurtosis.
	\param give_log true if one wants the log-probability, false otherwise.
		
	Notation adopted from R package "gamlss.dist".
	
	It is not possible to call this function with nu a vector or tau a vector.
*/
template <class Type>
Type pSHASHo(Type q,Type mu,Type sigma,Type nu,Type tau,int give_log=0)
{
	// TODO : Replace log(x+sqrt(x^2+1)) by a better approximation for asinh(x).

	Type z = (q-mu)/sigma;
	Type r = sinh(tau * log(z+sqrt(z*z+1)) - nu);
	Type p = pnorm(r);
				  	
	if (!give_log) return p;
	else return log(p);
}

// Vectorize pSHASHo
VECTORIZE6_ttttti(pSHASHo)

/**	\brief Quantile function of the sinh-asinh distribution.
	\ingroup R_style_distribution
	\param mu Location.
	\param sigma Scale.
	\param nu Skewness.
	\param tau Kurtosis.
	\param log_p true if p is log-probability, false otherwise.
	
	Notation adopted from R package "gamlss.dist".
	
	It is not possible to call this function with nu a vector or tau a vector.
	*/
template <class Type>
Type qSHASHo(Type p, Type mu, Type sigma, Type nu, Type tau, int log_p = 0)
{
	// TODO : Replace log(x+sqrt(x^2+1)) by a better approximation for asinh(x).

   	if(!log_p) return mu + sigma*sinh((1/tau)* log(qnorm(p)+sqrt(qnorm(p)*qnorm(p)+1)) + (nu/tau));
   	else return mu + sigma*sinh((1/tau)*log(qnorm(exp(p))+sqrt(qnorm(exp(p))*qnorm(exp(p))+1))+(nu/tau));
}

// Vectorize qSHASHo
VECTORIZE6_ttttti(qSHASHo)

/**	\brief Transforms a normal variable into a sinh-asinh variable.
	\param mu Location parameter of the result sinh-asinh distribution.
	\param sigma Scale parameter of the result sinh-asinh distribution.
	\param nu Skewness parameter of the result sinh-asinh distribution.
	\param tau Kurtosis parameter of the result sinh-asinh distribution.
	\param log_p true if p is log-probability, false otherwise.
	
	It is not possible to call this function with nu a vector or tau a vector.
	*/
template <class Type>
Type norm2SHASHo(Type x, Type mu, Type sigma, Type nu, Type tau, int log_p = 0)
{

	return qSHASHo(pnorm(x),mu,sigma,nu,tau,log_p);
}

// Vectorize norm2SHASHo
VECTORIZE6_ttttti(norm2SHASHo)
/**@}*/

/** \brief Distribution function of the beta distribution (following R
    argument convention).
    \note Non-centrality parameter (ncp) not implemented.
    \ingroup R_style_distribution
*/
template<class Type>
Type pbeta(Type q, Type shape1, Type shape2){
  CppAD::vector<Type> tx(4);
  tx[0] = q;
  tx[1] = shape1;
  tx[2] = shape2;
  tx[3] = 0; // order
  Type ans = atomic::pbeta(tx)[0];
  return ans;
}
VECTORIZE3_ttt(pbeta)

/** \brief Quantile function of the beta distribution (following R
    argument convention).
    \note Non-centrality parameter (ncp) not implemented.
    \ingroup R_style_distribution
*/
template<class Type>
Type qbeta(Type p, Type shape1, Type shape2){
  CppAD::vector<Type> tx(3);
  tx[0] = p;
  tx[1] = shape1;
  tx[2] = shape2;
  Type ans = atomic::qbeta(tx)[0];
  return ans;
}
VECTORIZE3_ttt(qbeta)

/** \brief besselK function (same as besselK from R).
    \note Derivatives wrt. both arguments are implemented
    \ingroup special_functions
*/
template<class Type>
Type besselK(Type x, Type nu){
  Type ans;
  if(CppAD::Variable(nu)) {
    CppAD::vector<Type> tx(3);
    tx[0] = x;
    tx[1] = nu;
    tx[2] = 0;
    ans = atomic::bessel_k(tx)[0];
  } else {
    CppAD::vector<Type> tx(2);
    tx[0] = x;
    tx[1] = nu;
    ans = atomic::bessel_k_10(tx)[0];
  }
  return ans;
}
VECTORIZE2_tt(besselK)

/** \brief besselI function (same as besselI from R).
    \note Derivatives wrt. both arguments are implemented
    \ingroup special_functions
*/
template<class Type>
Type besselI(Type x, Type nu){
  Type ans;
  if(CppAD::Variable(nu)) {
    CppAD::vector<Type> tx(3);
    tx[0] = x;
    tx[1] = nu;
    tx[2] = 0;
    ans = atomic::bessel_i(tx)[0];
  } else {
    CppAD::vector<Type> tx(2);
    tx[0] = x;
    tx[1] = nu;
    ans = atomic::bessel_i_10(tx)[0];
  }
  return ans;
}
VECTORIZE2_tt(besselI)

/** \brief besselJ function (same as besselJ from R).
    \note Derivatives wrt. both arguments are implemented
    \ingroup special_functions
*/
template<class Type>
Type besselJ(Type x, Type nu){
  CppAD::vector<Type> tx(3);
  tx[0] = x;
  tx[1] = nu;
  tx[2] = 0;
  Type ans = atomic::bessel_j(tx)[0];
  return ans;
}
VECTORIZE2_tt(besselJ)

/** \brief besselY function (same as besselY from R).
    \note Derivatives wrt. both arguments are implemented
    \ingroup special_functions
*/
template<class Type>
Type besselY(Type x, Type nu){
  CppAD::vector<Type> tx(3);
  tx[0] = x;
  tx[1] = nu;
  tx[2] = 0;
  Type ans = atomic::bessel_y(tx)[0];
  return ans;
}
VECTORIZE2_tt(besselY)

/** \brief dtweedie function (same as dtweedie.series from R package
    'tweedie').

    Silently returns NaN if not within the valid parameter range:
    \f[ (0 \leq y) \land (0 < \mu) \land (0 < \phi) \land (1 < p) \land (p < 2) \f] .

    \note Parameter order differs from the R version.

    \warning The derivative wrt. the y argument is disabled
    (zero). Hence the tweedie distribution can only be used for *data*
    (not random effects).

    \ingroup R_style_distribution
*/
template<class Type>
Type dtweedie(Type y, Type mu, Type phi, Type p, int give_log = 0) {
  Type p1 = p - 1.0, p2 = 2.0 - p;
  Type ans = -pow(mu, p2) / (phi * p2); // log(prob(y=0))
  if (y > 0) {
    CppAD::vector<Type> tx(4);
    tx[0] = y;
    tx[1] = phi;
    tx[2] = p;
    tx[3] = 0;
    ans += atomic::tweedie_logW(tx)[0];
    ans += -y / (phi * p1 * pow(mu, p1)) - log(y);
  }
  return ( give_log ? ans : exp(ans) );
}
VECTORIZE5_tttti(dtweedie)


/** \brief Conway-Maxwell-Poisson log normalizing constant.

    \f[ Z(\lambda, \nu) = \sum_{i=0}^{\infty} \frac{\lambda^i}{(i!)^\nu} \f] .

    \param loglambda \f$ \log(\lambda) \f$
    \param nu \f$ \nu \f$

    \return \f$ \log Z(\lambda, \nu) \f$
*/
template<class Type>
Type compois_calc_logZ(Type loglambda, Type nu) {
  CppAD::vector<Type> tx(3);
  tx[0] = loglambda;
  tx[1] = nu;
  tx[2] = 0;
  return atomic::compois_calc_logZ(tx)[0];
}
VECTORIZE2_tt(compois_calc_logZ)

/** \brief Conway-Maxwell-Poisson. Calculate log(lambda) from
    log(mean).

    \param logmean \f$ \log(E[X]) \f$
    \param nu \f$ \nu \f$

    \return \f$ \log \lambda \f$
*/
template<class Type>
Type compois_calc_loglambda(Type logmean, Type nu) {
  CppAD::vector<Type> tx(3);
  tx[0] = logmean;
  tx[1] = nu;
  tx[2] = 0;
  return atomic::compois_calc_loglambda(tx)[0];
}
VECTORIZE2_tt(compois_calc_loglambda)

/** \brief Conway-Maxwell-Poisson. Calculate density.

    \f[ P(X=x) \propto \frac{\lambda^x}{(x!)^\nu}\:,x=0,1,\ldots \f]

    Silently returns NaN if not within the valid parameter range:
    \f[ (0 \leq x) \land (0 < \lambda) \land (0 < \nu) \f] .

    \param x Observation
    \param mode Approximate mode \f$ \lambda^\frac{1}{\nu} \f$
    \param nu   \f$ \nu \f$

    \ingroup R_style_distribution
*/
template<class T1, class T2, class T3>
T1 dcompois(T1 x, T2 mode, T3 nu, int give_log = 0) {
  T2 loglambda = nu * log(mode);
  T1 ans = x * loglambda - nu * lfactorial(x);
  ans -= compois_calc_logZ(loglambda, nu);
  return ( give_log ? ans : exp(ans) );
}

/** \brief Conway-Maxwell-Poisson. Calculate density parameterized via
    the mean.

    Silently returns NaN if not within the valid parameter range:
    \f[ (0 \leq x) \land (0 < E[X]) \land (0 < \nu) \f] .

    \param x Observation
    \param mean \f$ E[X] \f$
    \param nu   \f$ \nu \f$

    \ingroup R_style_distribution
*/
template<class T1, class T2, class T3>
T1 dcompois2(T1 x, T2 mean, T3 nu, int give_log = 0) {
  T2 logmean = log(mean);
  T2 loglambda = compois_calc_loglambda(logmean, nu);
  T1 ans = x * loglambda - nu * lfactorial(x);
  ans -= compois_calc_logZ(loglambda, nu);
  return ( give_log ? ans : exp(ans) );
}

/********************************************************************/
/* SIMULATON CODE                                                   */
/********************************************************************/

extern "C" {
  double Rf_rnorm(double mu, double sigma);
}
/** \brief Simulate from a normal distribution  */
template<class Type>
Type rnorm(Type mu, Type sigma)
{
  return Rf_rnorm(asDouble(mu), asDouble(sigma));
}
VECTORIZE2_tt(rnorm)
VECTORIZE2_n(rnorm)

extern "C" {
  double Rf_rpois(double mu);
}
/** \brief Simulate from a Poisson distribution  */
template<class Type>
Type rpois(Type mu)
{
  return Rf_rpois(asDouble(mu));
}
VECTORIZE1_t(rpois)
VECTORIZE1_n(rpois)

extern "C" {
  double Rf_runif(double a, double b);
}
/** \brief Simulate from a uniform distribution  */
template<class Type>
Type runif(Type a, Type b)
{
  return Rf_runif(asDouble(a), asDouble(b));
}
VECTORIZE2_tt(runif)
VECTORIZE2_n(runif)

extern "C" {
  double Rf_rbinom(double size, double prob);
}
/** \brief Simulate from a binomial distribution  */
template<class Type>
Type rbinom(Type size, Type prob)
{
  return Rf_rbinom(asDouble(size), asDouble(prob));
}
VECTORIZE2_tt(rbinom)
VECTORIZE2_n(rbinom)

extern "C" {
  double Rf_rgamma(double shape, double scale);
}
/** \brief Simulate from a gamma distribution  */
template<class Type>
Type rgamma(Type shape, Type scale)
{
  return Rf_rgamma(asDouble(shape), asDouble(scale));
}
VECTORIZE2_tt(rgamma)
VECTORIZE2_n(rgamma)

extern "C" {
  double Rf_rexp(double rate);
}
/** \brief Simulate from an exponential distribution */
template<class Type>
Type rexp(Type rate)
{
  return Rf_rexp(asDouble(rate));
}

VECTORIZE1_t(rexp)
VECTORIZE1_n(rexp)

extern "C" {
	double Rf_rbeta(double shape1, double shape2);
}
/** \brief Simulate from a beta distribution */
template<class Type>
Type rbeta(Type shape1, Type shape2)
{
	return Rf_rbeta(asDouble(shape1), asDouble(shape2));
}

VECTORIZE2_tt(rbeta)
VECTORIZE2_n(rbeta)

extern "C" {
	double Rf_rf(double df1, double df2);
}
/** \brief Simulate from an F distribution */
template<class Type>
Type rf(Type df1, Type df2)
{
	return Rf_rf(asDouble(df1), asDouble(df2));
}

VECTORIZE2_tt(rf)
VECTORIZE2_n(rf)

extern "C" {
	double Rf_rlogis(double location, double scale);
}
/** \brief Simulate from a logistic distribution */
template<class Type>
Type rlogis(Type location, Type scale)
{
	return Rf_rlogis(asDouble(location), asDouble(scale));
}

VECTORIZE2_tt(rlogis)
VECTORIZE2_n(rlogis)

extern "C" {
	double Rf_rt(double df);
}
/** \brief Simulate from a Student's t distribution */
template<class Type>
Type rt(Type df)
{
	return Rf_rt(asDouble(df));
}

VECTORIZE1_t(rt)
VECTORIZE1_n(rt)

extern "C" {
	double Rf_rweibull(double shape, double scale);
}
/** \brief Simulate from a Weibull distribution */
template<class Type>
Type rweibull(Type shape, Type scale)
{
	return Rf_rweibull(asDouble(shape), asDouble(scale));
}

VECTORIZE2_tt(rweibull)
VECTORIZE2_n(rweibull)

/** \brief Simulate from a Conway-Maxwell-Poisson distribution  */
template<class Type>
Type rcompois(Type mode, Type nu)
{
  Type loglambda = nu * log(mode);
  return atomic::compois_utils::simulate(asDouble(loglambda), asDouble(nu));
}
VECTORIZE2_tt(rcompois)
VECTORIZE2_n(rcompois)

/** \brief Simulate from a Conway-Maxwell-Poisson distribution  */
template<class Type>
Type rcompois2(Type mean, Type nu)
{
  Type logmean = log(mean);
  Type loglambda = compois_calc_loglambda(logmean, nu);
  return atomic::compois_utils::simulate(asDouble(loglambda), asDouble(nu));
}
VECTORIZE2_tt(rcompois2)

// Note: Vectorize manually to avoid many identical calls to
// 'calc_loglambda'.
template<class Type>
vector<Type> rcompois2(int n, Type mean, Type nu)
{
  Type logmean = log(mean);
  Type loglambda = compois_calc_loglambda(logmean, nu);
  Type mode = exp(loglambda / nu);
  return rcompois(n, mode, nu);
}

/** \brief Simulate from tweedie distribution */
template<class Type>
Type rtweedie(Type mu, Type phi, Type p) {
  // Copied from R function tweedie::rtweedie
  Type lambda = pow(mu, 2. - p) / (phi * (2. - p));
  Type alpha  = (2. - p) / (1. - p);
  Type gam = phi * (p - 1.) * pow(mu, p - 1.);
  Type N = rpois(lambda);
  Type ans = rgamma( -alpha * N /* shape */, gam /* scale */);
  return ans;
}
VECTORIZE3_ttt(rtweedie)
VECTORIZE3_n(rtweedie)
