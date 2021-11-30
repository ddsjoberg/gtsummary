// Separable covariance on lattice with AR1 structure in each direction.
#include <TMB.hpp>

/* Parameter transform */
template <class Type>
Type f(Type x){return Type(2)/(Type(1) + exp(-Type(2) * x)) - Type(1);}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(N)
  PARAMETER_ARRAY(eta);
  PARAMETER(transf_phi1); /* fastest running dim */
  PARAMETER(transf_phi2); /* slowest running dim */
  Type phi1 = f(transf_phi1);
  Type phi2 = f(transf_phi2);

  using namespace density;
  Type res = 0;
  res += SEPARABLE( AR1(phi2), AR1(phi1) )(eta);

  /* keep = vector of ones */
  DATA_VECTOR_INDICATOR(keep, N);

  for(int i=0; i < N.size(); i++){
    res -= keep[i] * dpois(N[i], exp(eta[i]), true);
    /* For OSA residuals only: */
    Type cdf = squeeze( ppois(N[i], exp(eta[i])) );
    res -= keep.cdf_lower[i] * log( cdf );       // NaN protected
    res -= keep.cdf_upper[i] * log( 1.0 - cdf ); // NaN protected
  }

  return res;
}
