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


//! \addtogroup fn_rank
//! @{



template<typename T1>
arma_warn_unused
inline
uword
rank
  (
  const Base<typename T1::elem_type,T1>& X,
        typename T1::pod_type            tol = 0.0,
  const typename arma_blas_type_only<typename T1::elem_type>::result* junk = nullptr
  )
  {
  arma_extra_debug_sigprint();
  arma_ignore(junk);
  
  typedef typename T1::elem_type eT;
  typedef typename T1::pod_type   T;
  
  Mat<eT> A(X.get_ref());
  
  Col<T> s;
  
  const bool status = auxlib::svd_dc(s, A);
  
  if(status == false)
    {
    arma_stop_runtime_error("rank(): svd failed");
    
    return uword(0);
    }
  
  const uword s_n_elem = s.n_elem;
  const T*    s_mem    = s.memptr();
  
  // set tolerance to default if it hasn't been specified
  if( (tol == T(0)) && (s_n_elem > 0) )
    {
    tol = (std::max)(A.n_rows, A.n_cols) * s_mem[0] * std::numeric_limits<T>::epsilon();
    }
  
  uword count = 0;
  
  for(uword i=0; i < s_n_elem; ++i)  { count += (s_mem[i] > tol) ? uword(1) : uword(0); }
  
  return count;
  }



//! @}
