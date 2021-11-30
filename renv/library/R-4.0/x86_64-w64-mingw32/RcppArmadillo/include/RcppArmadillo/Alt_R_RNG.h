// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// Copyright (C) 2013-2014 Conrad Sanderson
// Copyright (C) 2013-2014 NICTA (www.nicta.com.au)
// 
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This file is based on Conrad's default generators and as such licensed under both 
// the MPL 2.0 for his as well as the GNU GPL 2.0 or later for my modification to it.

// Copyright (C)  2014  Dirk Eddelbuettel
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


// NB This files use R's uniform generator and can be compiled only when the R 
//    headers are available as is the case for RcppArmadillo.
//
//    Also note that you MUST set / reset the R RNG state.  When using RcppArmadillo
//    via Rcpp Atttributes or the inline package, the RNGScope object is added which
//    ensure this automatically.  Should you build by hand, and omit both RNGScope as
//    as manual calls to GetRNGState() and PutRNGState() you may get unstable results.
//
//    See http://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Random-numbers

class arma_rng_alt {
public:
  
    typedef unsigned int seed_type;
  
    inline static void set_seed(const seed_type val);
  
    arma_inline static int    randi_val();
    arma_inline static double randu_val();
         inline static double randn_val();
    
    template<typename eT>
    inline static void randn_dual_val(eT& out1, eT& out2);
  
    template<typename eT>
    inline static void randi_fill(eT* mem, const uword N, const int a, const int b);
  
    inline static int randi_max_val();
};

inline void arma_rng_alt::set_seed(const arma_rng_alt::seed_type val) {
    // null-op, cannot set seed in R from C level code
    // see http://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Random-numbers
    //
    // std::srand(val);
    (void) val;                 // to suppress a -Wunused warning
    //
    static int havewarned = 0;
    if (havewarned++ == 0) {
        ::Rf_warning("When called from R, the RNG seed has to be set at the R level via set.seed()");
    }
}

arma_inline int arma_rng_alt::randi_val() {
    return ::Rf_runif(0, RAND_MAX);  //std::rand();
}

arma_inline double arma_rng_alt::randu_val() {
    return double(::Rf_runif(0, 1));
    //return double( double(std::rand()) * ( double(1) / double(RAND_MAX) ) );
}

inline double arma_rng_alt::randn_val() {
    // polar form of the Box-Muller transformation:
    // http://en.wikipedia.org/wiki/Box-Muller_transformation
    // http://en.wikipedia.org/wiki/Marsaglia_polar_method
  
    double tmp1;
    double tmp2;
    double w;
  
    do {
        tmp1 = double(2) * double(::Rf_runif(0, 1)) - double(1);
        tmp2 = double(2) * double(::Rf_runif(0, 1)) - double(1);
        //tmp1 = double(2) * double(std::rand()) * (double(1) / double(RAND_MAX)) - double(1);
        //tmp2 = double(2) * double(std::rand()) * (double(1) / double(RAND_MAX)) - double(1);
    
        w = tmp1*tmp1 + tmp2*tmp2;
    } while ( w >= double(1) );
  
    return double( tmp1 * std::sqrt( (double(-2) * std::log(w)) / w) );
}

template<typename eT> 
inline void arma_rng_alt::randn_dual_val(eT& out1, eT& out2) {
    // make sure we are internally using at least floats
    typedef typename promote_type<eT,float>::result eTp;
  
    eTp tmp1;
    eTp tmp2;
    eTp w;
  
    do {
        tmp1 = eTp(2) * eTp(::Rf_runif(0, RAND_MAX)) * (eTp(1) / eTp(RAND_MAX)) - eTp(1);
        tmp2 = eTp(2) * eTp(::Rf_runif(0, RAND_MAX)) * (eTp(1) / eTp(RAND_MAX)) - eTp(1);
        //tmp1 = eTp(2) * eTp(std::rand()) * (eTp(1) / eTp(RAND_MAX)) - eTp(1);
        //tmp2 = eTp(2) * eTp(std::rand()) * (eTp(1) / eTp(RAND_MAX)) - eTp(1);
    
        w = tmp1*tmp1 + tmp2*tmp2;
    } while ( w >= eTp(1) );
  
    const eTp k = std::sqrt( (eTp(-2) * std::log(w)) / w);
    
    out1 = eT(tmp1*k);
    out2 = eT(tmp2*k);
}



template<typename eT>
inline void arma_rng_alt::randi_fill(eT* mem, const uword N, const int a, const int b) {
    if( (a == 0) && (b == RAND_MAX) ) {
        for(uword i=0; i<N; ++i) {
            mem[i] = ::Rf_runif(0, RAND_MAX);
            //mem[i] = std::rand();
        }
    } else {
        const uword length = b - a + 1;
    
        const double scale = double(length) / double(RAND_MAX);
    
        for(uword i=0; i<N; ++i) {
            mem[i] = (std::min)( b, (int( double(::Rf_runif(0,RAND_MAX)) * scale ) + a) );
            //mem[i] = (std::min)( b, (int( double(std::rand()) * scale ) + a) );
        }
    }
}

inline int arma_rng_alt::randi_max_val() {
    return RAND_MAX;
}
