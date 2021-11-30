// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// RcppArmadilloSugar.h: Rcpp/Armadillo glue
//
// Copyright (C)  2010 - 2013  Dirk Eddelbuettel, Romain Francois and Douglas Bates
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

#ifndef RcppArmadillo__RcppArmadilloSugar__h
#define RcppArmadillo__RcppArmadilloSugar__h

namespace Rcpp{

// forward is not needed anymore, but we just leave this
// for backwards compatibility
template <typename T>
inline const T& forward(const T& x) { return x; }

template <typename T> List simple_triplet_matrix ( const arma::SpMat<T>& sm ){
    const int  RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype;
    sm.sync();              // important: update internal state of SpMat object

    // copy the data into R objects
    Vector<RTYPE> x(sm.values, sm.values + sm.n_nonzero);
    IntegerVector i(sm.row_indices, sm.row_indices + sm.n_nonzero);
    i=i+1;
    IntegerVector p(sm.col_ptrs, sm.col_ptrs + sm.n_cols + 1);
    IntegerVector j(i.size());
    for (size_t cp=1, ie=0; ie < sm.n_nonzero; ie++) {
        for (; static_cast<size_t>(p[cp]) <= ie && cp < sm.n_cols; cp++)
            ;
        j[ie] = cp;
    }

    List s;
    s("i") = i;
    s("j") = j;
    s("v") = x;
    s("nrow") = sm.n_rows;
    s("ncol") = sm.n_cols;
    s("dimnames") = R_NilValue;
    s.attr("class") = "simple_triplet_matrix";
    return s;
}

} // Rcpp

#endif
