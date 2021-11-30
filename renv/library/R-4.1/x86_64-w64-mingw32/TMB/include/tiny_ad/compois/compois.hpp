namespace compois_utils {
//#define TRACE(x) std::cout << #x << "=" << x << "\n";

/** \brief Conway-Maxwell-Poisson. Calculate log-normalizing
    constant. */
template<class Type>
Type calc_logZ(Type loglambda, Type nu){
  using atomic::tiny_ad::isfinite;
  bool ok = (0 < nu && isfinite(loglambda) && isfinite(nu) );
  if (!ok) return NAN;
  using atomic::robust_utils::logspace_add;
  using atomic::robust_utils::logspace_sub;
  int iter_max = 1e4;
  Type logZ = 0.;
  Type logmu = loglambda / nu;
  Type mu = exp(logmu); // mode: lambda^(1/nu)
  if (false) {
    // Asymptotic expansion for large mu (dropped because inaccurate)
    logZ =
      nu * mu - 0.5 * log(nu) -
      ((nu - 1) / 2) * log(mu) -
      ((nu - 1) / 2) * log(2 * M_PI);
  } else if ( (mu > 100) && (mu * nu > 200) && (nu < 2 * mu) ) {
    using atomic::tiny_ad::lgamma;
    // Laplace approximation when nu=1 (Poisson case)
    Type jhat = mu - .5;
    Type H1 = lgamma<2>(jhat + 1.);
    Type fhat1 = jhat * logmu - lgamma(jhat + 1.);
    Type logL1 = log(sqrt(2. * M_PI)) - .5 * log(H1) + fhat1;
    // Laplace approximation error nu=1
    Type err1 = logL1 - mu;
    // Laplace approximation general nu
    Type H = nu * H1;
    Type fhat = nu * fhat1;
    Type logL = log(sqrt(2. * M_PI)) - .5 * log(H) + fhat;
    // Apply correction
    logL -= err1 / nu;
    logZ = logL;
  }
  else {
    // Series summation
    int index;             // Current index
    Type logT;             // Current log term
    Type dlogT;            // logT(index) - logT(index-1)
    double reltol = 1e-12; // Break if Term_current / Sum_current < reltol
    int i;
    // Initialize largest term and sum
    int index_mode = floor(mu);
    Type logT_mode = index_mode * loglambda - nu * lgamma(index_mode + 1.);
    logZ = logT_mode;
    // Left tail. FIXME: Use half gaussian approximation for mu >~ maxit.
    logT = logT_mode; // Initialize
    for(i = 1; i<iter_max; i++) {
      index = index_mode - i;
      if (index < 0) break;
      dlogT = loglambda - nu * log( (double) index + 1);
      logT -= dlogT;
      logZ = logspace_add(logZ, logT);
      if ( logT - logZ < log(reltol) ) break;
    }
    // Right tail
    logT = logT_mode; // Initialize
    for(i = 1; i<iter_max; i++) {
      index = index_mode + i;
      dlogT = loglambda - nu * log( (double) index);
      logT += dlogT;
      logZ = logspace_add(logZ, logT);
      if ( logT - logZ < log(reltol) ) break;
    }
    // Tail upper bound via geometric series
    // T_j = T_i * exp( dlogT_i * j ) , j>=i
    //  sum( rho^j , j=i,...) = rho^i * 1/(1-rho)
    // T_i * rho^i * 1/(1-rho)
    // logT_tail = log(T_i) + i * log(rho) - log(1-rho)
    Type logT_tail = logT + index * dlogT - logspace_sub(Type(0), dlogT);
    logZ = logspace_add(logZ, logT_tail);
  }
  return logZ;
}

/** \brief Conway-Maxwell-Poisson. Calculate mean from log(lambda). */
template<class Type>
Type calc_mean(Type loglambda, Type nu){
  typedef atomic::tiny_ad::variable<1, 1, Type> ADType;
  ADType loglambda_ (loglambda, 0);
  ADType ans = calc_logZ<ADType>(loglambda_, nu);
  return ans.getDeriv()[0];
}

/** \brief Conway-Maxwell-Poisson. Calculate log(lambda) from
    log(mean). */
template<class Type>
Type calc_loglambda(Type logmean, Type nu) {
  using atomic::tiny_ad::isfinite;
  bool ok = (0 < nu && isfinite(logmean) && isfinite(nu) );
  if (!ok) return NAN;
  int iter_max = 100; double reltol = 1e-9, abstol = 1e-12;
  typedef atomic::tiny_ad::variable<1, 1, Type> ADType;
  ADType x = nu * logmean; // Initial guess
  ADType step = 0;
  ADType f_previous = INFINITY;
  int i=0;
  for ( ; i < iter_max ; i++ ) {
    x.deriv[0] = 1.; // Seed
    ADType y = calc_mean<ADType>(x, nu);
    if( ! isfinite(y) ) {
      if (i==0) return NAN; // Undefined initial value
      // Step halfway back
      step = step / 2;
      x -= step;
      continue;
    }
    ADType f = ( y > 0 ?
                 log(y) - ADType( logmean ) :
                 y - ADType( exp(logmean) ) );
    if( fabs(f) > fabs(f_previous) ) {
      // Step halfway back
      step = step / 2;
      x -= step;
      continue;
    }
    step = ( f.deriv[0] != 0 ?
             -f.value / f.deriv[0] : 0 ); // Newton step
    x += step; f_previous = f;
    if(fabs(step) <= reltol * fabs(x))
      break;
    if(fabs(step) <= abstol)
      break;
  }
  if (i == iter_max)
    Rf_warning("calc_loglambda: Maximum number of iterations exceeded");
  return x.value;
}

extern "C" {
  double Rf_lgammafn(double);
  double Rf_psigamma(double, double);
  double Rf_expm1(double);
  double Rf_pgeom(double x, double p, int lower_tail, int log_p);
  double Rf_runif(double a, double b);
  double Rf_qgeom(double p, double prob, int lower_tail, int log_p);
  double Rf_rgeom(double p);
}

/** \brief Conway-Maxwell-Poisson. Rejection sampler using a Piecewise
    log-linear envelope. Does not require the normalizing constant.

    Gives fairly high acceptance rates - typically around 0.7 even for
    extreme parameters. The values j0 and j1 (tangent-points for the
    linear envelope) used here, are the mode plus/minus one standard
    deviation of the approximating Gaussian. This is 'hand-tuned' thus
    probably not optimal.
*/
#ifdef WITH_LIBTMB
double simulate(double loglambda, double nu);
#else
double simulate(double loglambda, double nu) {
#define logf_target(x)  ( nu * ( x * logmu - Rf_lgammafn(x+1) ) )
#define logf_propose(x) ( x < jhat ? logf0(x) : logf1(x) )
#define logf0(x) ( v0 + slope0 * (x - j0) )
#define logf1(x) ( v1 + slope1 * (x - j1) )
  double logmu = loglambda / nu;
  double mu = exp(logmu);
  double jhat = (mu > 1 ? mu - .5 : 1);
  double sd = 1. / sqrt(nu * Rf_psigamma(jhat+1, 1));    // Approx Gaussian
  double j0 = (mu > 1 ? jhat - fmin(sd, .5 * jhat) : 0); // left tangent point
  double j1 = jhat + sd;                                 // right tangent point
  double slope0 = (mu > 1 ? nu * ( logmu - Rf_psigamma(j0+1, 0) ) : 0);
  double slope1 = nu * ( logmu - Rf_psigamma(j1+1, 0) );
  double v0 = nu * ( j0 * logmu - Rf_lgammafn(j0+1) );
  double v1 = nu * ( j1 * logmu - Rf_lgammafn(j1+1) );
  // Geometric success probabilities
  double prob0 = (mu > 1 ? -expm1(-slope0) : 1);
  double prob1 = -expm1(slope1);
  double mu0 = (mu > 1 ? floor(jhat) : 0); // Left offset
  double mu1 = mu0 + 1;                    // right offset
  double p0 = Rf_pgeom(mu0, prob0, 1, 0); // Left tail is truncated
  double w0 = p0 * exp(logf0(mu0)) / prob0; // Mass of left tail proposal
  double w1 = 1  * exp(logf1(mu1)) / prob1; // Mass of right tail proposal
  double samp = NAN;
  if(false) { // Debug
    // Calculate accept probs
    double St=0, Sp=0;
    for(int i=0; i<1e6; i++) {
      St += exp(logf_target(i));
      Sp += exp(logf_propose(i));
    }
    double paccept = St / Sp;
    return paccept;
  }
  int i=0, iter_max = 1e4;
  for ( ; i < iter_max; i++ ) {
    // Draw proposal sample
    if( Rf_runif(0, 1) < w0 / (w0 + w1) ) {
      samp = mu0 - Rf_qgeom(Rf_runif(0, p0), prob0, 1, 0);
    } else {
      samp = mu1 + Rf_rgeom(prob1);
    }
    double paccept = exp(logf_target(samp) - logf_propose(samp));
    if(paccept > 1) {
      samp = NAN;
      Rf_warning("compois sampler failed (probably overflow: paccept = %f)", paccept);
      break;
    }
    if( Rf_runif(0, 1) < paccept ) break;
  }
  if (i == iter_max) {
    samp = NAN;
    Rf_warning("compois sampler failed (iteration limit exceeded)");
  }
  if (samp != samp) { // NAN
    Rf_warning("compois sampler returned NaN for mu=%f nu=%f", mu, nu);
  }
#undef logf_target
#undef logf_propose
#undef logf0
#undef logf1
  return samp;
}
#endif

} // compois_utils
