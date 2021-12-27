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



//! \addtogroup op_pinv
//! @{



template<typename T1>
inline
void
op_pinv::apply(Mat<typename T1::elem_type>& out, const Op<T1,op_pinv>& in)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::pod_type T;
  
  const T     tol       = access::tmp_real(in.aux);
  const uword method_id = in.aux_uword_a;
  
  const bool status = op_pinv::apply_direct(out, in.m, tol, method_id);
  
  if(status == false)
    {
    out.soft_reset();
    arma_stop_runtime_error("pinv(): svd failed");
    }
  }



template<typename T1>
inline
bool
op_pinv::apply_direct(Mat<typename T1::elem_type>& out, const Base<typename T1::elem_type,T1>& expr, typename T1::pod_type tol, const uword method_id)
  {
  arma_extra_debug_sigprint();
  
  typedef typename T1::elem_type eT;
  typedef typename T1::pod_type   T;
  
  arma_debug_check((tol < T(0)), "pinv(): tolerance must be >= 0");
  
  // method_id = 0 -> default setting
  // method_id = 1 -> use standard algorithm
  // method_id = 2 -> use divide and conquer algorithm
  
  Mat<eT> A(expr.get_ref());
  
  const uword n_rows = A.n_rows;
  const uword n_cols = A.n_cols;
  
  if(A.is_empty())  { out.set_size(n_cols,n_rows); return true; }
  
  #if defined(ARMA_OPTIMISE_SYMPD)
    const bool try_sympd = (auxlib::crippled_lapack(A) == false) && (tol == T(0)) && (method_id == uword(0)) && sympd_helper::guess_sympd_anysize(A);
  #else
    const bool try_sympd = false;
  #endif
  
  if(try_sympd)
    {
    arma_extra_debug_print("op_pinv: attempting sympd optimisation");
    
    out = A;
    
    const T rcond_threshold = T((std::max)(uword(100), uword(A.n_rows))) * std::numeric_limits<T>::epsilon();
    
    const bool status = auxlib::inv_sympd_rcond(out, rcond_threshold);
    
    if(status)  { return true; }
    
    arma_extra_debug_print("op_pinv: sympd optimisation failed");
    // auxlib::inv_sympd_rcond() will fail if A isn't really positive definite or its rcond is below rcond_threshold
    }
  
  // economical SVD decomposition 
  Mat<eT> U;
  Col< T> s;
  Mat<eT> V;
  
  if(n_cols > n_rows)  { A = trans(A); }
  
  const bool status = ((method_id == uword(0)) || (method_id == uword(2))) ? auxlib::svd_dc_econ(U, s, V, A) : auxlib::svd_econ(U, s, V, A, 'b');
  
  if(status == false)  { return false; }
  
  const uword s_n_elem = s.n_elem;
  const T*    s_mem    = s.memptr();
  
  // set tolerance to default if it hasn't been specified
  if( (tol == T(0)) && (s_n_elem > 0) )
    {
    tol = (std::max)(n_rows, n_cols) * s_mem[0] * std::numeric_limits<T>::epsilon();
    }
  
  
  uword count = 0;
  
  for(uword i = 0; i < s_n_elem; ++i)  { count += (s_mem[i] >= tol) ? uword(1) : uword(0); }
  
  if(count == 0)  { out.zeros(n_cols, n_rows); return true; }
  
  Col<T> s2(count, arma_nozeros_indicator());
  
  T* s2_mem = s2.memptr();
  
  uword count2 = 0;
  
  for(uword i=0; i < s_n_elem; ++i)
    {
    const T val = s_mem[i];
    
    if(val >= tol)  { s2_mem[count2] = (val > T(0)) ? T(T(1) / val) : T(0); ++count2; }
    }
  
  
  Mat<eT> tmp;
    
  if(n_rows >= n_cols)
    {
    // out = ( (V.n_cols > count) ? V.cols(0,count-1) : V ) * diagmat(s2) * trans( (U.n_cols > count) ? U.cols(0,count-1) : U );
    
    if(count < V.n_cols)
      {
      tmp = V.cols(0,count-1) * diagmat(s2);
      }
    else
      {
      tmp = V * diagmat(s2);
      }
    
    if(count < U.n_cols)
      {
      out = tmp * trans(U.cols(0,count-1));
      }
    else
      {
      out = tmp * trans(U);
      }
    }
  else
    {
    // out = ( (U.n_cols > count) ? U.cols(0,count-1) : U ) * diagmat(s2) * trans( (V.n_cols > count) ? V.cols(0,count-1) : V );
    
    if(count < U.n_cols)
      {
      tmp = U.cols(0,count-1) * diagmat(s2);
      }
    else
      {
      tmp = U * diagmat(s2);
      }
    
    if(count < V.n_cols)
      {
      out = tmp * trans(V.cols(0,count-1));
      }
    else
      {
      out = tmp * trans(V);
      }
    }
  
  return true;
  }



//! @}
