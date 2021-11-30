// Regression model with random slope and intercept.
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_FACTOR(group);
  DATA_VECTOR(x);
  DATA_VECTOR(t);
  PARAMETER_VECTOR(a);     // Slope for given individual
  PARAMETER_VECTOR(b);     // Intercept for given individual
  PARAMETER_VECTOR(mu);    // length 2 - one for slope, one for intercept
  PARAMETER_VECTOR(sigma); // length 2 - one for slope, one for intercept
  PARAMETER(sigma0);

  int nobs = x.size();
  int ngroups = a.size();
  Type res = 0;
  int j;

  /* Prior: slope ~ N(mu0,sd0), intercept ~ N(mu1,sd1) */
  for(int j=0; j < ngroups; j++){
    res -= dnorm(a[j], mu[0], sigma[0], true);
    res -= dnorm(b[j], mu[1], sigma[1], true);
  }

  /* Observations: x|a,b ~ N(a*t+b,sigma0) */
  for(int i = 0; i < nobs; i++){
    j = group[i];
    res -= dnorm(x[i], a[j] * t[i] + b[j], sigma0, true);
  }
  return res;
}

