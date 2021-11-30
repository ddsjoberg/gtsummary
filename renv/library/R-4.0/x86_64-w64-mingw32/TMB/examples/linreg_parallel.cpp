// Linear regression model in parallel.
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Y);
  DATA_VECTOR(x);
  PARAMETER(a);
  PARAMETER(b);
  PARAMETER(logSigma);
  parallel_accumulator<Type> nll(this);
  for(int i=0; i<x.size(); i++)
    nll -= dnorm(Y[i], a + b * x[i], exp(logSigma), true);
  return nll;
}
