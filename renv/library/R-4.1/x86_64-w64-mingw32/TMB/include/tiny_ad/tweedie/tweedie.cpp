// Re-structured version of tweedie density function from 'cplm' package.

/* the threshold used in finding the bounds of the series */
#define TWEEDIE_DROP 37.0
/* the loop increment used in finding the bounds of the series */
#define TWEEDIE_INCRE 5
/* the max number of terms allowed in the finite sum approximation*/
#define TWEEDIE_NTERM 20000

/** \brief Calculate \f$\log W(y, \phi, p)$\f with notation as in Dunn
    and Smyth 2005 page 269 equation 2.  Required to calculate the
    density of the Tweedie distribution.

    \param y _positive_ observation
    \param phi scalar: the dispersion parameter
    \param p scalar: the index parameter
*/
template<class Float>
Float tweedie_logW(Float y, Float phi, Float p){
  bool ok = (0 < y) && (0 < phi) && (1 < p) && (p < 2);
  if (!ok) return NAN;

  Float p1 = p - 1.0, p2 = 2.0 - p;
  Float a = - p2 / p1, a1 = 1.0 / p1;
  Float cc, w, sum_ww = 0.0, ww_max ;
  double j;

  /* only need the lower bound and the # terms to be stored */
  int jh, jl, jd;
  double jmax = 0;
  Float logz = 0;

  /* compute jmax for the given y > 0*/
  cc = a * log(p1) - log(p2);
  jmax = asDouble( fmax2(1.0, pow(y, p2) / (phi * p2)) );
  logz = - a * log(y) - a1 * log(phi) + cc;

  /* find bounds in the summation */
  /* locate upper bound */
  cc = logz + a1 + a * log(-a);
  j = jmax ;
  w = a1 * j ;
  while (1) {
    j += TWEEDIE_INCRE ;
    if (j * (cc - a1 * log(j)) < (w - TWEEDIE_DROP))
      break ;
  }
  jh = ceil(j);
  /* locate lower bound */
  j = jmax;
  while (1) {
    j -= TWEEDIE_INCRE ;
    if (j < 1 || j * (cc - a1 * log(j)) < w - TWEEDIE_DROP)
      break ;
  }
  jl = imax2(1, floor(j)) ;
  jd = jh - jl + 1;

  /* set limit for # terms in the sum */
  int nterms = imin2(imax(&jd, 1), TWEEDIE_NTERM), iterm ;
  Float *ww = Calloc(nterms, Float) ;
  /* evaluate series using the finite sum*/
  /* y > 0 */
  sum_ww = 0.0 ;
  iterm = imin2(jd, nterms) ;
  for (int k = 0; k < iterm; k++) {
    j = k + jl ;
    ww[k] = j * logz - lgamma(1 + j) - lgamma(-a * j);
  }
  ww_max = dmax(ww, iterm) ;
  for (int k = 0; k < iterm; k++)
    sum_ww += exp(ww[k] - ww_max);
  Float ans = log(sum_ww) + ww_max  ;
  Free(ww);

  return ans;
}
