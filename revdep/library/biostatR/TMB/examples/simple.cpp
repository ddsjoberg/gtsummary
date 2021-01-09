// Normal linear mixed model specified through sparse design matrices.
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(x);         // Observations
  DATA_SPARSE_MATRIX(B);  // Random effect design matrix
  DATA_SPARSE_MATRIX(A);  // Fixed effect design matrix
  PARAMETER_VECTOR(u);    // Random effects vector
  PARAMETER_VECTOR(beta); // Fixed effects vector
  PARAMETER(logsdu);      // Random effect standard deviations
  PARAMETER(logsd0);      // Measurement standard deviation

  // Distribution of random effect (u):
  Type ans = 0;
  ans -= dnorm(u, Type(0), exp(logsdu), true).sum();

  // Optionally: How to simulate the random effects
  SIMULATE {
    u = rnorm(u.size(), Type(0), exp(logsdu));
    REPORT(u);
  }

  // Distribution of obs given random effects (x|u):
  vector<Type> y = A * beta + B * u;
  ans -= dnorm(x, y, exp(logsd0), true).sum();

  // Optionally: How to simulate the data
  SIMULATE {
    x = rnorm(y, exp(logsd0));
    REPORT(x);
  }

  // Apply delta method on sd0:
  Type sd0=exp(logsd0);	
  ADREPORT( sd0 );
  REPORT(sd0);

  // Report posterior mode and mean of sum(exp(u))
  ADREPORT( sum(exp(u)) );
 

  return ans;
}

