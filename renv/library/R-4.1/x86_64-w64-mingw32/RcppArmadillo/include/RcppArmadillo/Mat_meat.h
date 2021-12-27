// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// Mat_meat.h: Rcpp/Armadillo glue
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

#ifndef RCPPARMADILLO_MAT_MEAT_H
#define RCPPARMADILLO_MAT_MEAT_H

namespace RcppArmadillo{
    
    template <typename eT, typename rcpp_type>
    inline void check(){
#if !defined(ARMA_USE_CXX11)
	arma_type_check_cxx1998< is_same_type< eT, rcpp_type >::value == false >::apply();
#else
	static_assert( is_same_type< eT, rcpp_type >::value , "error: incorrect or unsupported type" );
#endif
    }
    
    template <>
    inline void check< std::complex<double>, Rcomplex >(){}
    
    
    template <typename eT, int RTYPE, bool NA, typename VECTOR>
    inline void fill_ptr__impl( eT* ptr, const Rcpp::VectorBase<RTYPE,NA,VECTOR>& X, u32 n_elem, ::Rcpp::traits::true_type ){
        for( u32 i=0; i<n_elem; ++i){
	    	ptr[i] = Rcpp::internal::caster< typename Rcpp::traits::storage_type<RTYPE>::type, eT>( X[i] ) ;
	    }
    }
    
    template <typename eT, int RTYPE, bool NA, typename VECTOR>
    inline void fill_ptr__impl( eT* ptr, const Rcpp::VectorBase<RTYPE,NA,VECTOR>& X, u32 n_elem, ::Rcpp::traits::false_type ){
        for( u32 i=0; i<n_elem; ++i){
	    	ptr[i] = X[i] ;
	    }
    }
    
    
    template <typename eT, int RTYPE, bool NA, typename VECTOR>
    inline void fill_ptr( eT* ptr, const Rcpp::VectorBase<RTYPE,NA,VECTOR>& X, u32 n_elem ){
        return fill_ptr__impl<eT, RTYPE, NA, VECTOR>( ptr, X, n_elem, 
            typename ::Rcpp::traits::r_sexptype_needscast<eT>()
            ) ;
    }
    
    
    
    
    
    template <typename eT, int RTYPE, bool NA, typename MATRIX>
    inline void fill_ptr_matrix__impl( eT* ptr, const Rcpp::MatrixBase<RTYPE,NA,MATRIX>& X, u32 nr, u32 nc, ::Rcpp::traits::true_type ){
        u32 k, i_col, i_row ;
        for( i_col=0, k=0 ; i_col < nc; ++i_col){
	    	for( i_row = 0; i_row < nr ; ++i_row, ++k ){
	    		ptr[k] = Rcpp::internal::caster< typename Rcpp::traits::storage_type<RTYPE>::type, eT>( X(i_row,i_col)) ;
	    	}
	    }
	}
    
    template <typename eT, int RTYPE, bool NA, typename MATRIX>
    inline void fill_ptr_matrix__impl( eT* ptr, const Rcpp::MatrixBase<RTYPE,NA,MATRIX>& X, u32 nr, u32 nc, ::Rcpp::traits::false_type ){
        u32 k, i_col, i_row ;
        for( i_col=0, k=0 ; i_col < nc; ++i_col){
	    	for( i_row = 0; i_row < nr ; ++i_row, ++k ){
	    		ptr[k] = X(i_row,i_col) ;
	    	}
	    }
    }
    
    
    
    template <typename eT, int RTYPE, bool NA, typename MATRIX>
    inline void fill_ptr_matrix( eT* ptr, const Rcpp::MatrixBase<RTYPE,NA,MATRIX>& X, u32 nr, u32 nc){
        return fill_ptr_matrix__impl<eT, RTYPE, NA, MATRIX>( ptr, X, nr, nc, 
            typename ::Rcpp::traits::r_sexptype_needscast<eT>()
            ) ;
    }
   
}

template <typename eT>
template <int RTYPE, bool NA, typename VECTOR>
inline Mat<eT>::Mat( const Rcpp::VectorBase<RTYPE,NA,VECTOR>& X ) 
	: n_rows(0)
	, n_cols(0)
	, n_elem(0)
        , n_alloc(0)
	, vec_state(0)
	, mem_state(0)
	, mem(0)
{
	
	arma_extra_debug_sigprint_this(this);
	
	RcppArmadillo::check<eT, typename Rcpp::traits::storage_type<RTYPE>::type >() ;
	
	set_size(X.size(), 1);
	RcppArmadillo::fill_ptr<eT, RTYPE, NA, VECTOR>( memptr(), X, n_elem ) ; 
}

template <typename eT>
template <int RTYPE, bool NA, typename MATRIX>
inline Mat<eT>::Mat( const Rcpp::MatrixBase<RTYPE,NA,MATRIX>& X ) 
	: n_rows(0)
	, n_cols(0)
	, n_elem(0)
        , n_alloc(0)
	, vec_state(0)
	, mem_state(0)
	, mem(0)
{
	
	arma_extra_debug_sigprint_this(this);
	
	RcppArmadillo::check<eT, typename Rcpp::traits::storage_type<RTYPE>::type >() ;
	
	u32 nr = X.nrow(), nc = X.ncol() ;
	set_size( nr, nc ) ;
		
	RcppArmadillo::fill_ptr_matrix( memptr(), X, nr, nc ); 
	
}

#endif
