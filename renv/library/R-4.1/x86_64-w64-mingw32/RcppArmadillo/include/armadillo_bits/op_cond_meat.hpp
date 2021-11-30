// Copyright 2008-2016 Conrad Sanderson (http://conradsanderson.id.au)
// Copyright 2008-2016 National ICT Australia (NICTA)
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ------------------------------------------------------------------------


//! \addtogroup op_cond
//! @{



template<typename T1>
inline
typename T1::pod_type
op_cond::cond(const Base<typename T1::elem_type, T1>& X)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::elem_type eT;
  typedef typename T1::pod_type   T;
  
  Mat<eT> A(X.get_ref());
  
  Col<T> S;
  
  const bool status = auxlib::svd_dc(S, A);
  
  if(status == false)
    {
    arma_debug_warn_level(3, "cond(): svd failed");
    
    return T(0);
    }
  
  return (S.n_elem > 0) ? T( max(S) / min(S) ) : T(0);
  }



template<typename T1>
inline
typename T1::pod_type
op_cond::rcond(const Base<typename T1::elem_type, T1>& X)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::elem_type eT;
  typedef typename T1::pod_type   T;
  
  if(strip_trimat<T1>::do_trimat)
    {
    const strip_trimat<T1> S(X.get_ref());
    
    const quasi_unwrap<typename strip_trimat<T1>::stored_type> U(S.M);
    
    arma_debug_check( (U.M.is_square() == false), "rcond(): matrix must be square sized" );
    
    const uword layout = (S.do_triu) ? uword(0) : uword(1);
    
    return auxlib::rcond_trimat(U.M, layout);
    }
  
  Mat<eT> A = X.get_ref();
  
  arma_debug_check( (A.is_square() == false), "rcond(): matrix must be square sized" );
  
  if(A.is_empty()) { return Datum<T>::inf; }
  
  const bool is_triu =                     trimat_helper::is_triu(A);
  const bool is_tril = (is_triu) ? false : trimat_helper::is_tril(A);
  
  if(is_triu || is_tril)
    {
    const uword layout = (is_triu) ? uword(0) : uword(1);
    
    return auxlib::rcond_trimat(A, layout);
    }
  
  #if defined(ARMA_OPTIMISE_SYMPD)
    const bool try_sympd = auxlib::crippled_lapack(A) ? false : sympd_helper::guess_sympd_anysize(A);
  #else
    const bool try_sympd = false;
  #endif
  
  if(try_sympd)
    {
    bool calc_ok = false;
    
    const T out_val = auxlib::rcond_sympd(A, calc_ok);
    
    if(calc_ok)  { return out_val; }
    
    // auxlib::rcond_sympd() may have failed because A isn't really sympd
    // restore A, as auxlib::rcond_sympd() may have destroyed it
    A = X.get_ref();
    // fallthrough to the next return statement
    }
  
  return auxlib::rcond(A);
  }



//! @}
