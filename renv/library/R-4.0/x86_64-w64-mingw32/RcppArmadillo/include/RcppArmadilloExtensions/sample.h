// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// sample.h: Rcpp/Armadillo equivalent to R's sample().  
// This is intended for use in C++ functions, and should *not* be called from R.
// It should yield identical results to R in most cases, 
// and stop with errors when results are expected to differ.
//
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

#ifndef RCPPARMADILLO__EXTENSIONS__SAMPLE_H
#define RCPPARMADILLO__EXTENSIONS__SAMPLE_H

#include <RcppArmadilloExtensions/fixprob.h>

namespace Rcpp{
    namespace RcppArmadillo{

        template <class T> T sample_main(const T &x, const int size, const bool replace, const arma::vec &prob_);
        void SampleNoReplace(arma::uvec &index, int nOrig, int size);
        void SampleReplace(arma::uvec &index, int nOrig, int size);
        void ProbSampleNoReplace(arma::uvec &index, int nOrig, int size, arma::vec &prob);
        void ProbSampleReplace(arma::uvec &index, int nOrig, int size, arma::vec &prob);
        void WalkerProbSampleReplace(arma::uvec &index, int nOrig, int size, arma::vec &prob);

        
        // Setup default function calls for pre-exisiting dependencies that use NumericVector

        // No probabilities passed in
        template <class T> 
        T sample(const T &x, const int size, const bool replace){
          // Creates a zero-size vector in arma (cannot directly call arma::vec(0))
          const arma::vec prob = arma::zeros<arma::vec>(0);
          return sample_main(x, size, replace, prob);
        }

        // Convert from NumericVector to arma vector
        template <class T> 
        T sample(const T &x, const int size, const bool replace, NumericVector prob_){
          const arma::vec prob(prob_.begin(), prob_.size(), false);
          return sample_main(x, size, replace, prob);
        }
        
        // Enables supplying an arma probability
        template <class T> 
        T sample(const T &x, const int size, const bool replace, const arma::vec &prob_){
          return sample_main(x, size, replace, prob_);
        }

        // ------ Main sampling logic
        
        // Supply any class
        template <class T> 
        T sample_main(const T &x, const int size, const bool replace, const arma::vec &prob) {
            // Templated sample -- should work on any Rcpp Vector
            int ii, jj;
            int nOrig = x.size();
            int probsize = prob.n_elem;
            
            // Create return object 
            T ret(size);
            if ( size > nOrig && !replace) throw std::range_error( "Tried to sample more elements than in x without replacement" ) ;
            if ( !replace && (probsize==0) && nOrig > 1e+07 && size <= nOrig/2) {
                throw std::range_error( "R uses .Internal(sample2(n, size) for this case, which is not implemented." ) ;
            }
            
            // Store the sample ids here, modify in-place
            arma::uvec index(size);
            if (probsize == 0) { // No probabilities given
                if (replace) {
                    SampleReplace(index, nOrig, size);
                } else {
                    SampleNoReplace(index, nOrig, size);
                }
            } else { 
                if (probsize != nOrig) throw std::range_error( "Number of probabilities must equal input vector length" ) ;

                // copy prob
                // fprob will be modified in-place
                // (and possibly clobbered by workers)
                arma::vec fprob = prob;
                FixProb(fprob, size, replace);
                
                // Reuse the values
                if (replace) {
                    // check for walker alias conditions 
                    int walker_test = sum( (fprob * nOrig) > 0.1);
                    if (walker_test > 200) {
                        WalkerProbSampleReplace(index, nOrig, size, fprob);
                    } else {
                        ProbSampleReplace(index, nOrig, size, fprob);
                    }
                } else {
                    ProbSampleNoReplace(index, nOrig, size, fprob);
                }
            }
            // copy the results into the return vector
            for (ii=0; ii<size; ii++) {
                jj = index(ii);  // arma 
                
                ret[ii] = x[jj]; // templated
            }
            return(ret);
        }

        // ------------------ Worker functions
        
        void SampleReplace( arma::uvec &index, int nOrig, int size) {
            int ii;
            for (ii = 0; ii < size; ii++) {
                index(ii) = nOrig * unif_rand();
            }
        }

        void SampleNoReplace( arma::uvec &index, int nOrig, int size) {
            int ii, jj;
            arma::uvec sub(nOrig);
            for (ii = 0; ii < nOrig; ii++) {
                sub(ii) = ii;
            }
            for (ii = 0; ii < size; ii++) {
                jj = nOrig * unif_rand();
                index(ii) = sub(jj);
                // replace sampled element with last, decrement
                sub(jj) = sub(--nOrig);
            }
        }


        // Unequal probability sampling with replacement 
        void ProbSampleReplace(arma::uvec &index, int nOrig, int size, arma::vec &prob){
            double rU;
            int ii, jj;
            int nOrig_1 = nOrig - 1;
            arma::uvec perm = arma::sort_index(prob, "descend"); //descending sort of index
            prob = arma::sort(prob, "descend");  // descending sort of prob
            // cumulative probabilities 
            prob = arma::cumsum(prob);
            // compute the sample 
            for (ii = 0; ii < size; ii++) {
                rU = unif_rand();
                for (jj = 0; jj < nOrig_1; jj++) {
                    if (rU <= prob[jj])
                        break;
                }
                index[ii] = perm[jj];
            }
        }

        // Unequal probability sampling with replacement, prob.size() large and sum(prob) >0.1
        void WalkerProbSampleReplace(arma::uvec &index, int nOrig, int size, arma::vec &prob){
            double rU;
            int ii, jj, kk; // indices, ii for loops
            // index tables, fill with zeros
            arma::vec HL_dat(nOrig);
            arma::vec alias_tab(nOrig); 
            arma::vec::iterator H, L, H0, L0;
            //HL0 = HL_dat.begin();
            H0 = H = HL_dat.begin();
            L0 = L = HL_dat.end();
            //prob *= nOrig; // scale probability table
            // fill HL_dat from beginning (small prob) and end (large prob) with indices
            for (ii = 0; ii < nOrig; ii++) {
                prob[ii] *= nOrig;
                if( prob[ii] < 1.0) {
                    *(H++) = ii;
                } else {
                    *(--L) = ii;
                }
            }
            
            // some of both large and small
            if ( (H > H0) && (L < L0) ) {
                for (kk = 0; kk < nOrig; kk++) {
                    ii = HL_dat[kk];
                    jj = *L;
                    alias_tab[ii] = jj;
                    prob[jj] += (prob[ii] - 1);
                    if (prob[jj] < 1.) L++;
                    if(L == L0) break; // now all prob >= 1
                }
            }
            for (ii = 0; ii < nOrig; ii++)  prob[ii] += ii;
            /* generate sample */
            for (ii = 0; ii < size; ii++) {
                rU = unif_rand() * nOrig;
                kk = (int) rU;
                index[ii] = (rU < prob[kk]) ? kk : alias_tab[kk];
            }
        }

        // Unequal probability sampling without replacement 
        void ProbSampleNoReplace(arma::uvec &index, int nOrig, int size, arma::vec &prob){
            int ii, jj, kk;
            int nOrig_1 = nOrig - 1;
            double rT, mass, totalmass = 1.0;
            arma::uvec perm = arma::sort_index(prob, "descend"); //descending sort of index
            prob = arma::sort(prob, "descend");  // descending sort of prob
            // compute the sample 
            for (ii = 0; ii < size; ii++, nOrig_1--) {
                rT = totalmass * unif_rand();
                mass = 0;
                for (jj = 0; jj < nOrig_1; jj++) {
                    mass += prob[jj];
                    if (rT <= mass)
                        break;
                }
                index[ii] = perm[jj];
                totalmass -= prob[jj];
                for ( kk = jj; kk < nOrig_1; kk++) {
                    prob[kk] = prob[kk+1];
                    perm[kk] = perm[kk+1];
                }
            }
        }
    }
}

#endif
