// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// rmultinom.h: Rcpp/Armadillo equivalent to R's stats::rmultinom().  
// This is intended for use in C++ functions, and should *not* be called from R.
// It should yield identical results to R.
//
// Copyright (C)  2014  Christian Gunning
//
// This file is part of RcppArmadillo.
//
// RcppArmadillo is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppArmadillo is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

#ifndef RCPPARMADILLO__EXTENSIONS__MULTINOM_H
#define RCPPARMADILLO__EXTENSIONS__MULTINOM_H

#include <RcppArmadillo.h>
namespace Rcpp{
    namespace RcppArmadillo{

        IntegerVector rmultinom(int size,  NumericVector prob) {
            // meaning of n, size, prob as in ?rmultinom
            // opposite of sample() - n=number of draws
            double pp;            
            int ii;
            int probsize = prob.size();
            // Return object
            IntegerVector draws(probsize);
            if (size < 0 || size == NA_INTEGER) throw std::range_error( "Invalid size");
            long double p_tot = 0.;
            p_tot = std::accumulate(prob.begin(), prob.end(), p_tot);
            if (fabs((double)(p_tot - 1.)) > 1e-7) {
                throw std::range_error("Probabilities don't sum to 1, please use FixProb");
            }
            
            // do as rbinom
            if (size == 0 ) {
                 return draws;
            }
            //rmultinom(size, REAL(prob), k, &INTEGER(ans)[ik]);
            // for each slot
            for (ii = 0; ii < probsize-1; ii++) { /* (p_tot, n) are for "remaining binomial" */
                if (prob[ii]) {
                    pp = prob[ii] / p_tot;
                    // >= 1; > 1 happens because of rounding 
                    draws[ii] = ((pp < 1.) ? (int) Rf_rbinom((double) size,  pp) : size);
                    size -= draws[ii];
                } // else { ret[ii] = 0; }
                // all done
                if (size <= 0)  return draws;
                // i.e. p_tot = sum(prob[(k+1):K]) 
                p_tot -= prob[ii]; 
            }
            // the rest go here
            draws[probsize-1] = size;
            return draws;
        }
    }
}

#endif
