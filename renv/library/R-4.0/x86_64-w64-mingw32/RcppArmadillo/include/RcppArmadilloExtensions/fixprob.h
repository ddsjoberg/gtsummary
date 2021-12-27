// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// fixprob.h: helper function for Rcpp/Armadillo random number draws, including sample().  
// Copyright (C)  2012 - 2014  Christian Gunning
// Copyright (C)  2013  Romain Francois
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
#ifndef RCPPARMADILLO__EXTENSIONS__FIXPROB_H
#define RCPPARMADILLO__EXTENSIONS__FIXPROB_H

#include <RcppArmadillo.h>
namespace Rcpp{
    namespace RcppArmadillo{
    
        void FixProb(arma::vec &prob, const int size, const bool replace) {
            // prob is modified in-place.  
            double sum = 0.0;
            int ii, nPos = 0;
            int nn = prob.size();
            for (ii = 0; ii < nn; ii++) {
                // pop stack
                double prob_value = prob(ii);
              
                if (!arma::is_finite(prob_value)) //does this work??
                    throw std::range_error( "NAs not allowed in probability" ) ;
                if (prob_value < 0.0)
                    throw std::range_error( "Negative probabilities not allowed" ) ;
                if (prob_value > 0.0) {
                    nPos++;
                    sum += prob_value;
                }
            }
            if (nPos == 0 || (!replace && size > nPos)) {
                throw std::range_error("Not enough positive probabilities");
            }
            prob = prob / sum;  //sugar
        }
    }
}
#endif
