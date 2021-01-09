// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file
    \brief Univariate normal density
    \ingroup R_style_distribution
*/
template<class Type>
Type dnorm(Type x, Type mean, Type sd, int give_log=0)
{
  Type resid = (x - mean) / sd;
  Type logans = -log(sqrt(2*M_PI)) - log(sd) - Type(.5) * resid * resid;
  if(give_log) return logans; else return exp(logans);
}
VECTORIZE4_ttti(dnorm)
