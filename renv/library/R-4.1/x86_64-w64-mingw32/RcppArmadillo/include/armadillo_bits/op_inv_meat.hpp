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


//! \addtogroup op_inv
//! @{



template<typename T1>
inline
void
op_inv::apply(Mat<typename T1::elem_type>& out, const Op<T1,op_inv>& X)
  {
  arma_extra_debug_sigprint();
  
  const bool status = op_inv::apply_direct(out, X.m, "inv()");
  
  if(status == false)
    {
    out.soft_reset();
    arma_stop_runtime_error("inv(): matrix is singular");
    }
  }



template<typename T1>
inline
bool
op_inv::apply_direct(Mat<typename T1::elem_type>& out, const Base<typename T1::elem_type,T1>& expr, const char* caller_sig)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::elem_type eT;
  
  if(strip_diagmat<T1>::do_diagmat)
    {
    const strip_diagmat<T1> strip(expr.get_ref());
    
    return op_inv::apply_diagmat(out, strip.M, caller_sig);
    }
  
  if(strip_trimat<T1>::do_trimat)
    {
    const strip_trimat<T1> strip(expr.get_ref());
    
    out = strip.M;
    
    arma_debug_check( (out.is_square() == false), caller_sig, ": given matrix must be square sized" );
    
    return auxlib::inv_tr(out, (strip.do_triu ? uword(0) : uword(1)));
    }
  
  out = expr.get_ref();
  
  arma_debug_check( (out.is_square() == false), caller_sig, ": given matrix must be square sized" );
  
  if((out.n_rows <= 4) && is_cx<eT>::no)
    {
    Mat<eT> tmp(out.n_rows, out.n_rows, arma_nozeros_indicator());
    
    const bool status = op_inv::apply_tiny_noalias(tmp, out);
    
    if(status)  { arrayops::copy(out.memptr(), tmp.memptr(), tmp.n_elem); return true; }
    
    // fallthrough if optimisation failed
    }
  
  if(out.is_diagmat())  { return op_inv::apply_diagmat(out, out, caller_sig); }
  
  const bool is_triu =                     trimat_helper::is_triu(out);
  const bool is_tril = (is_triu) ? false : trimat_helper::is_tril(out);
  
  if(is_triu || is_tril)  { return auxlib::inv_tr(out, ((is_triu) ? uword(0) : uword(1))); }

  #if defined(ARMA_OPTIMISE_SYMPD)
    const bool try_sympd = sympd_helper::guess_sympd_anysize(out);
  #else
    const bool try_sympd = false;
  #endif
  
  if(try_sympd)
    {
    arma_extra_debug_print("op_inv: attempting sympd optimisation");
    
    Mat<eT> tmp = out;
    
    const bool status = auxlib::inv_sympd(tmp);
    
    if(status)  { out.steal_mem(tmp); return true; }
    
    arma_extra_debug_print("op_inv: sympd optimisation failed");
    
    // fallthrough if optimisation failed
    }
  
  return auxlib::inv(out);
  }



template<typename T1>
inline
bool
op_inv::apply_diagmat(Mat<typename T1::elem_type>& out, const T1& X, const char* caller_sig)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::elem_type eT;
  
  const diagmat_proxy<T1> A(X);
  
  arma_debug_check( (A.n_rows != A.n_cols), caller_sig, ": given matrix must be square sized" );
  
  const uword N = (std::min)(A.n_rows, A.n_cols);
  
  bool status = true;
  
  if(A.is_alias(out) == false)
    {
    out.zeros(N,N);
    
    for(uword i=0; i<N; ++i)
      {
      const eT val = A[i];
      
      status = (val == eT(0)) ? false : status;
      
      out.at(i,i) = eT(1) / val;
      }
    }
  else
    {
    Mat<eT> tmp(N, N, arma_zeros_indicator());
    
    for(uword i=0; i<N; ++i)
      {
      const eT val = A[i];
      
      status = (val == eT(0)) ? false : status;
      
      tmp.at(i,i) = eT(1) / val;
      }
    
    out.steal_mem(tmp);
    }
  
  return status;
  }



template<typename eT>
arma_cold
inline
bool
op_inv::apply_tiny_noalias(Mat<eT>& out, const Mat<eT>& X)
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  // NOTE: assuming matrix X is square sized
  
  const uword N = X.n_rows;
  
  out.set_size(N,N);
  
  constexpr T det_min =        std::numeric_limits<T>::epsilon();
  constexpr T det_max = T(1) / std::numeric_limits<T>::epsilon();
  
  const eT* Xm   =   X.memptr();
        eT* outm = out.memptr();
  
  if(N == 0)  { return true; }
  
  if(N == 1)  { outm[0] = eT(1) / Xm[0]; return true; };
  
  if(N == 2)
    {
    const eT a = Xm[pos<0,0>::n2];
    const eT b = Xm[pos<0,1>::n2];
    const eT c = Xm[pos<1,0>::n2];
    const eT d = Xm[pos<1,1>::n2];
    
    const eT     det_val = (a*d - b*c);
    const  T abs_det_val = std::abs(det_val);
    
    if((abs_det_val < det_min) || (abs_det_val > det_max))  { return false; }
    
    outm[pos<0,0>::n2] =  d / det_val;
    outm[pos<0,1>::n2] = -b / det_val;
    outm[pos<1,0>::n2] = -c / det_val;
    outm[pos<1,1>::n2] =  a / det_val;
    
    return true;
    }
  
  if(N == 3)
    {
    const eT     det_val = op_det::apply_tiny(X);
    const  T abs_det_val = std::abs(det_val);
    
    if((abs_det_val < det_min) || (abs_det_val > det_max))  { return false; }
    
    outm[pos<0,0>::n3] =  (Xm[pos<2,2>::n3]*Xm[pos<1,1>::n3] - Xm[pos<2,1>::n3]*Xm[pos<1,2>::n3]) / det_val;
    outm[pos<1,0>::n3] = -(Xm[pos<2,2>::n3]*Xm[pos<1,0>::n3] - Xm[pos<2,0>::n3]*Xm[pos<1,2>::n3]) / det_val;
    outm[pos<2,0>::n3] =  (Xm[pos<2,1>::n3]*Xm[pos<1,0>::n3] - Xm[pos<2,0>::n3]*Xm[pos<1,1>::n3]) / det_val;
    
    outm[pos<0,1>::n3] = -(Xm[pos<2,2>::n3]*Xm[pos<0,1>::n3] - Xm[pos<2,1>::n3]*Xm[pos<0,2>::n3]) / det_val;
    outm[pos<1,1>::n3] =  (Xm[pos<2,2>::n3]*Xm[pos<0,0>::n3] - Xm[pos<2,0>::n3]*Xm[pos<0,2>::n3]) / det_val;
    outm[pos<2,1>::n3] = -(Xm[pos<2,1>::n3]*Xm[pos<0,0>::n3] - Xm[pos<2,0>::n3]*Xm[pos<0,1>::n3]) / det_val;
    
    outm[pos<0,2>::n3] =  (Xm[pos<1,2>::n3]*Xm[pos<0,1>::n3] - Xm[pos<1,1>::n3]*Xm[pos<0,2>::n3]) / det_val;
    outm[pos<1,2>::n3] = -(Xm[pos<1,2>::n3]*Xm[pos<0,0>::n3] - Xm[pos<1,0>::n3]*Xm[pos<0,2>::n3]) / det_val;
    outm[pos<2,2>::n3] =  (Xm[pos<1,1>::n3]*Xm[pos<0,0>::n3] - Xm[pos<1,0>::n3]*Xm[pos<0,1>::n3]) / det_val;
    
    const eT check_val = Xm[pos<0,0>::n3]*outm[pos<0,0>::n3] + Xm[pos<0,1>::n3]*outm[pos<1,0>::n3] + Xm[pos<0,2>::n3]*outm[pos<2,0>::n3];
    
    const  T max_diff  = (is_float<T>::value) ? T(1e-4) : T(1e-10);  // empirically determined; may need tuning
    
    if(std::abs(T(1) - check_val) >= max_diff)  { return false; }
    
    return true;
    }
  
  if(N == 4)
    {
    const eT     det_val = op_det::apply_tiny(X);
    const  T abs_det_val = std::abs(det_val);
    
    if((abs_det_val < det_min) || (abs_det_val > det_max))  { return false; }
    
    outm[pos<0,0>::n4] = ( Xm[pos<1,2>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,1>::n4] - Xm[pos<1,3>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,1>::n4] + Xm[pos<1,3>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,2>::n4] - Xm[pos<1,1>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,2>::n4] - Xm[pos<1,2>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,3>::n4] + Xm[pos<1,1>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<1,0>::n4] = ( Xm[pos<1,3>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,0>::n4] - Xm[pos<1,2>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,0>::n4] - Xm[pos<1,3>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,2>::n4] + Xm[pos<1,0>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,2>::n4] + Xm[pos<1,2>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,3>::n4] - Xm[pos<1,0>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<2,0>::n4] = ( Xm[pos<1,1>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,0>::n4] - Xm[pos<1,3>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,0>::n4] + Xm[pos<1,3>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,1>::n4] - Xm[pos<1,0>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,1>::n4] - Xm[pos<1,1>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,3>::n4] + Xm[pos<1,0>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<3,0>::n4] = ( Xm[pos<1,2>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,0>::n4] - Xm[pos<1,1>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,0>::n4] - Xm[pos<1,2>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,1>::n4] + Xm[pos<1,0>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,1>::n4] + Xm[pos<1,1>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,2>::n4] - Xm[pos<1,0>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,2>::n4] ) / det_val;
    
    outm[pos<0,1>::n4] = ( Xm[pos<0,3>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,2>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,3>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,2>::n4] + Xm[pos<0,1>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,2>::n4] + Xm[pos<0,2>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,3>::n4] - Xm[pos<0,1>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<1,1>::n4] = ( Xm[pos<0,2>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,3>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,0>::n4] + Xm[pos<0,3>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,2>::n4] - Xm[pos<0,0>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,2>::n4] - Xm[pos<0,2>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,3>::n4] + Xm[pos<0,0>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<2,1>::n4] = ( Xm[pos<0,3>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,1>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,3>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,1>::n4] + Xm[pos<0,0>::n4]*Xm[pos<2,3>::n4]*Xm[pos<3,1>::n4] + Xm[pos<0,1>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,3>::n4] - Xm[pos<0,0>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<3,1>::n4] = ( Xm[pos<0,1>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,2>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,0>::n4] + Xm[pos<0,2>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,0>::n4]*Xm[pos<2,2>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,1>::n4]*Xm[pos<2,0>::n4]*Xm[pos<3,2>::n4] + Xm[pos<0,0>::n4]*Xm[pos<2,1>::n4]*Xm[pos<3,2>::n4] ) / det_val;
    
    outm[pos<0,2>::n4] = ( Xm[pos<0,2>::n4]*Xm[pos<1,3>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,3>::n4]*Xm[pos<1,2>::n4]*Xm[pos<3,1>::n4] + Xm[pos<0,3>::n4]*Xm[pos<1,1>::n4]*Xm[pos<3,2>::n4] - Xm[pos<0,1>::n4]*Xm[pos<1,3>::n4]*Xm[pos<3,2>::n4] - Xm[pos<0,2>::n4]*Xm[pos<1,1>::n4]*Xm[pos<3,3>::n4] + Xm[pos<0,1>::n4]*Xm[pos<1,2>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<1,2>::n4] = ( Xm[pos<0,3>::n4]*Xm[pos<1,2>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,2>::n4]*Xm[pos<1,3>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,3>::n4]*Xm[pos<1,0>::n4]*Xm[pos<3,2>::n4] + Xm[pos<0,0>::n4]*Xm[pos<1,3>::n4]*Xm[pos<3,2>::n4] + Xm[pos<0,2>::n4]*Xm[pos<1,0>::n4]*Xm[pos<3,3>::n4] - Xm[pos<0,0>::n4]*Xm[pos<1,2>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<2,2>::n4] = ( Xm[pos<0,1>::n4]*Xm[pos<1,3>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,3>::n4]*Xm[pos<1,1>::n4]*Xm[pos<3,0>::n4] + Xm[pos<0,3>::n4]*Xm[pos<1,0>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,0>::n4]*Xm[pos<1,3>::n4]*Xm[pos<3,1>::n4] - Xm[pos<0,1>::n4]*Xm[pos<1,0>::n4]*Xm[pos<3,3>::n4] + Xm[pos<0,0>::n4]*Xm[pos<1,1>::n4]*Xm[pos<3,3>::n4] ) / det_val;
    outm[pos<3,2>::n4] = ( Xm[pos<0,2>::n4]*Xm[pos<1,1>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,1>::n4]*Xm[pos<1,2>::n4]*Xm[pos<3,0>::n4] - Xm[pos<0,2>::n4]*Xm[pos<1,0>::n4]*Xm[pos<3,1>::n4] + Xm[pos<0,0>::n4]*Xm[pos<1,2>::n4]*Xm[pos<3,1>::n4] + Xm[pos<0,1>::n4]*Xm[pos<1,0>::n4]*Xm[pos<3,2>::n4] - Xm[pos<0,0>::n4]*Xm[pos<1,1>::n4]*Xm[pos<3,2>::n4] ) / det_val;
    
    outm[pos<0,3>::n4] = ( Xm[pos<0,3>::n4]*Xm[pos<1,2>::n4]*Xm[pos<2,1>::n4] - Xm[pos<0,2>::n4]*Xm[pos<1,3>::n4]*Xm[pos<2,1>::n4] - Xm[pos<0,3>::n4]*Xm[pos<1,1>::n4]*Xm[pos<2,2>::n4] + Xm[pos<0,1>::n4]*Xm[pos<1,3>::n4]*Xm[pos<2,2>::n4] + Xm[pos<0,2>::n4]*Xm[pos<1,1>::n4]*Xm[pos<2,3>::n4] - Xm[pos<0,1>::n4]*Xm[pos<1,2>::n4]*Xm[pos<2,3>::n4] ) / det_val;
    outm[pos<1,3>::n4] = ( Xm[pos<0,2>::n4]*Xm[pos<1,3>::n4]*Xm[pos<2,0>::n4] - Xm[pos<0,3>::n4]*Xm[pos<1,2>::n4]*Xm[pos<2,0>::n4] + Xm[pos<0,3>::n4]*Xm[pos<1,0>::n4]*Xm[pos<2,2>::n4] - Xm[pos<0,0>::n4]*Xm[pos<1,3>::n4]*Xm[pos<2,2>::n4] - Xm[pos<0,2>::n4]*Xm[pos<1,0>::n4]*Xm[pos<2,3>::n4] + Xm[pos<0,0>::n4]*Xm[pos<1,2>::n4]*Xm[pos<2,3>::n4] ) / det_val;
    outm[pos<2,3>::n4] = ( Xm[pos<0,3>::n4]*Xm[pos<1,1>::n4]*Xm[pos<2,0>::n4] - Xm[pos<0,1>::n4]*Xm[pos<1,3>::n4]*Xm[pos<2,0>::n4] - Xm[pos<0,3>::n4]*Xm[pos<1,0>::n4]*Xm[pos<2,1>::n4] + Xm[pos<0,0>::n4]*Xm[pos<1,3>::n4]*Xm[pos<2,1>::n4] + Xm[pos<0,1>::n4]*Xm[pos<1,0>::n4]*Xm[pos<2,3>::n4] - Xm[pos<0,0>::n4]*Xm[pos<1,1>::n4]*Xm[pos<2,3>::n4] ) / det_val;
    outm[pos<3,3>::n4] = ( Xm[pos<0,1>::n4]*Xm[pos<1,2>::n4]*Xm[pos<2,0>::n4] - Xm[pos<0,2>::n4]*Xm[pos<1,1>::n4]*Xm[pos<2,0>::n4] + Xm[pos<0,2>::n4]*Xm[pos<1,0>::n4]*Xm[pos<2,1>::n4] - Xm[pos<0,0>::n4]*Xm[pos<1,2>::n4]*Xm[pos<2,1>::n4] - Xm[pos<0,1>::n4]*Xm[pos<1,0>::n4]*Xm[pos<2,2>::n4] + Xm[pos<0,0>::n4]*Xm[pos<1,1>::n4]*Xm[pos<2,2>::n4] ) / det_val;
    
    const eT check_val = Xm[pos<0,0>::n4]*outm[pos<0,0>::n4] + Xm[pos<0,1>::n4]*outm[pos<1,0>::n4] + Xm[pos<0,2>::n4]*outm[pos<2,0>::n4] + Xm[pos<0,3>::n4]*outm[pos<3,0>::n4];
    
    const  T max_diff  = (is_float<T>::value) ? T(1e-4) : T(1e-10);  // empirically determined; may need tuning
    
    if(std::abs(T(1) - check_val) >= max_diff)  { return false; }
    
    return true;
    }
  
  return false;
  }



template<typename T1>
inline
void
op_inv_sympd::apply(Mat<typename T1::elem_type>& out, const Op<T1,op_inv_sympd>& X)
  {
  arma_extra_debug_sigprint();
  
  const bool status = op_inv_sympd::apply_direct(out, X.m);
  
  if(status == false)
    {
    out.soft_reset();
    arma_stop_runtime_error("inv_sympd(): matrix is singular or not positive definite");
    }
  }



template<typename T1>
inline
bool
op_inv_sympd::apply_direct(Mat<typename T1::elem_type>& out, const Base<typename T1::elem_type,T1>& expr)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::elem_type eT;
  
  out = expr.get_ref();
  
  arma_debug_check( (out.is_square() == false), "inv_sympd(): given matrix must be square sized" );
  
  if((arma_config::debug) && (auxlib::rudimentary_sym_check(out) == false))
    {
    if(is_cx<eT>::no )  { arma_debug_warn_level(1, "inv_sympd(): given matrix is not symmetric"); }
    if(is_cx<eT>::yes)  { arma_debug_warn_level(1, "inv_sympd(): given matrix is not hermitian"); }
    }
  
  if((out.n_rows <= 4) && is_cx<eT>::no)
    {
    Mat<eT> tmp(out.n_rows, out.n_rows, arma_nozeros_indicator());
    
    const bool status = op_inv::apply_tiny_noalias(tmp, out);
    
    if(status)  { arrayops::copy(out.memptr(), tmp.memptr(), tmp.n_elem); return true; }
    
    // fallthrough if optimisation failed
    }
  
  return auxlib::inv_sympd(out);
  }



//! @}
