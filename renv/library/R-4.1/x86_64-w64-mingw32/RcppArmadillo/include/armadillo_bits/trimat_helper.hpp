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


//! \addtogroup trimat_helper
//! @{


namespace trimat_helper
{



template<typename eT>
inline
bool
is_triu(const Mat<eT>& A)
  {
  arma_extra_debug_sigprint();
  
  // NOTE: assuming that A has a square size
  
  const uword N   = A.n_rows;
  const uword Nm1 = N-1;
  
  if(N < 2)  { return false; }
  
  const eT*   A_mem   = A.memptr();
  const eT    eT_zero = eT(0);
  
  // quickly check bottom-left corner
  const eT* A_col0 = A_mem;
  const eT* A_col1 = A_col0 + N;
  
  if( (A_col0[N-2] != eT_zero) || (A_col0[Nm1] != eT_zero) || (A_col1[Nm1] != eT_zero) )  { return false; }
  
  // if we got to this point, do a thorough check
  
  const eT* A_col = A_mem;
  
  for(uword j=0; j < Nm1; ++j)
    {
    for(uword i=(j+1); i < N; ++i)
      {
      const eT A_ij = A_col[i];
      
      if(A_ij != eT_zero) { return false; }
      }
    
    A_col += N;
    }
  
  return true;
  }



template<typename eT>
inline
bool
is_tril(const Mat<eT>& A)
  {
  arma_extra_debug_sigprint();
  
  // NOTE: assuming that A has a square size
  
  const uword N = A.n_rows;
  
  if(N < 2)  { return false; }
  
  const eT eT_zero = eT(0);
  
  // quickly check top-right corner
  const eT* A_colNm2 = A.colptr(N-2);
  const eT* A_colNm1 = A_colNm2 + N;
  
  if( (A_colNm2[0] != eT_zero) || (A_colNm1[0] != eT_zero) || (A_colNm1[1] != eT_zero) )  { return false; }
  
  // if we got to this point, do a thorough check
  
  const eT* A_col = A.memptr() + N;
  
  for(uword j=1; j < N; ++j)
    {
    for(uword i=0; i < j; ++i)
      {
      const eT A_ij = A_col[i];
      
      if(A_ij != eT_zero) { return false; }
      }
    
    A_col += N;
    }
  
  return true;
  }



}  // end of namespace trimat_helper


//! @}
