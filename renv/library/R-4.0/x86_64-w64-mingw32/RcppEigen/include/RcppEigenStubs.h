// RcppEigenStubs.cpp: Provide access to compiled CHOLMOD functions in
// the Matrix package.
//
// Copyright (C)      2011 Douglas Bates and Martin Maechler
//
// This file is part of RcppEigen.
//
// RcppEigen is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppEigen is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppEigen.  If not, see <http://www.gnu.org/licenses/>.

// Yes, this really is a C++ source file in an include directory.  To
// use the Cholmod support functions in RcppEigen you should create a
// source file, say MyPackage/src/local_stubs.c that contains the
// single line #include "RcppEigenStubs.cpp"

#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef	__cplusplus
extern "C" {
#endif

inline CHM_DN as_cholmod_dense(CHM_DN ans, SEXP x)
{
    static CHM_DN(*fun)(CHM_DN,SEXP) = NULL;
    if(fun == NULL)
	fun = (CHM_DN(*)(CHM_DN,SEXP))
	    R_GetCCallable("Matrix", "as_cholmod_dense");
    return fun(ans, x);
}

inline CHM_FR as_cholmod_factor(CHM_FR ans, SEXP x)
{
    static CHM_FR(*fun)(CHM_FR,SEXP) = NULL;
    if(fun == NULL)
	fun = (CHM_FR(*)(CHM_FR,SEXP))
	    R_GetCCallable("Matrix", "as_cholmod_factor");
    return fun(ans, x);
}

inline CHM_SP as_cholmod_sparse(CHM_SP ans, SEXP x, Rboolean check_Udiag, Rboolean sort_in_place)
{
    static CHM_SP(*fun)(CHM_SP,SEXP,Rboolean,Rboolean)= NULL;
    if(fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,SEXP,Rboolean,Rboolean))
	    R_GetCCallable("Matrix", "as_cholmod_sparse");
    return fun(ans, x, check_Udiag, sort_in_place);
}

inline CHM_TR as_cholmod_triplet(CHM_TR ans, SEXP x, Rboolean check_Udiag)
{
    static CHM_TR(*fun)(CHM_TR,SEXP,Rboolean)= NULL;
    if(fun == NULL)
	fun = (CHM_TR(*)(CHM_TR,SEXP,Rboolean))
	    R_GetCCallable("Matrix", "as_cholmod_triplet");
    return fun(ans, x, check_Udiag);
}

inline SEXP Csparse_diagU2N(SEXP x)
{
    static SEXP(*fun)(SEXP) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(SEXP))
	    R_GetCCallable("Matrix", "Csparse_diagU2N");
    return fun(x);
}

inline SEXP
M_chm_factor_to_SEXP(const_CHM_FR f, int dofree)
{
    static SEXP(*fun)(const_CHM_FR,int) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(const_CHM_FR,int))
	    R_GetCCallable("Matrix", "chm_factor_to_SEXP");
    return fun(f, dofree);
}

inline double
M_chm_factor_ldetL2(const_CHM_FR f)
{
    static double(*fun)(const_CHM_FR) = NULL;
    if(fun == NULL)
	fun = (double(*)(const_CHM_FR))
	    R_GetCCallable("Matrix", "chm_factor_ldetL2");
    return fun(f);
}

inline CHM_FR
M_chm_factor_update(CHM_FR f, const_CHM_SP A, double mult)
{
    static CHM_FR(*fun)(CHM_FR,const_CHM_SP,double) = NULL;
    if(fun == NULL)
	fun = (CHM_FR(*)(CHM_FR,const_CHM_SP,double))
	    R_GetCCallable("Matrix", "chm_factor_update");
    return fun(f, A, mult);
}

inline SEXP
M_chm_sparse_to_SEXP(const_CHM_SP a, int dofree,
		     int uploT, int Rkind, const char *diag, SEXP dn)
{
    static SEXP(*fun)(const_CHM_SP,int,int,int,const char*,SEXP) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(const_CHM_SP,int,int,int,const char*,SEXP))
	    R_GetCCallable("Matrix", "chm_sparse_to_SEXP");
    return fun(a, dofree, uploT, Rkind, diag, dn);
}

inline SEXP
M_chm_triplet_to_SEXP(const CHM_TR a, int dofree,
		      int uploT, int Rkind, const char *diag, SEXP dn)
{
    static SEXP(*fun)(const CHM_TR,int,int,int,const char*,SEXP) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(const CHM_TR,int,int,int,const char*,SEXP))
	    R_GetCCallable("Matrix", "chm_triplet_to_SEXP");
    return fun(a, dofree, uploT, Rkind, diag, dn);
}

inline CHM_SP
cholmod_aat(const_CHM_SP A, int *fset, size_t fsize,
	      int mode, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,int*,size_t,
			int,CHM_CM) = NULL;
    if(fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,int*,size_t,
			 int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_aat");
    return fun(A, fset, fsize, mode, Common);
}

inline int
M_cholmod_band_inplace(CHM_SP A, int k1, int k2, int mode,
		       CHM_CM Common)
{
    static int(*fun)(CHM_SP,int,int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP,int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_band_inplace");
    return fun(A, k1, k2, mode, Common);
}

inline CHM_SP
cholmod_add(const_CHM_SP A, const_CHM_SP B,
	      double alpha[2], double beta[2], int values,
	      int sorted, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,const_CHM_SP,
			double*,double*,int,int,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,const_CHM_SP,
			 double*,double*,int,int,
			 CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_add");
    return fun(A, B, alpha, beta, values, sorted, Common);
}

inline CHM_DN
cholmod_allocate_dense(size_t nrow, size_t ncol, size_t d,
			 int xtype, CHM_CM Common)
{
    static CHM_DN(*fun)(size_t,size_t,size_t,
			int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(size_t,size_t,size_t,
			 int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_allocate_dense");
    return fun(nrow, ncol, d, xtype, Common);
}

inline CHM_SP
cholmod_allocate_sparse(size_t nrow, size_t ncol, size_t nzmax,
			  int sorted, int packed, int stype,
			  int xtype, CHM_CM Common)
{
    static CHM_SP(*fun)(size_t,size_t,size_t,int,int,
			int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)
	       (size_t,size_t,size_t,int,int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_allocate_sparse");
    return fun(nrow,ncol,nzmax,sorted,packed,stype,xtype,Common);
}

inline CHM_TR
cholmod_allocate_triplet(size_t nrow, size_t ncol, size_t nzmax,
			   int stype, int xtype, CHM_CM Common)
{
    static CHM_TR(*fun)(size_t,size_t,size_t, int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_TR(*)(size_t,size_t,size_t,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_allocate_triplet");
    return fun(nrow,ncol,nzmax,stype,xtype,Common);
}

inline CHM_SP
cholmod_triplet_to_sparse(const cholmod_triplet* T, int nzmax,
			    CHM_CM Common)
{
    static CHM_SP(*fun)(const cholmod_triplet*,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const cholmod_triplet*,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_triplet_to_sparse");
    return fun(T, nzmax, Common);
}

inline CHM_TR
cholmod_sparse_to_triplet(const_CHM_SP A, CHM_CM Common)
{
    static CHM_TR(*fun)(const_CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_TR(*)(const_CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_sparse_to_triplet");
    return fun(A, Common);
}

inline CHM_DN
cholmod_sparse_to_dense(const_CHM_SP A, CHM_CM Common)
{
    static CHM_DN(*fun)(const_CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(const_CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_sparse_to_dense");
    return fun(A, Common);
}

inline CHM_FR
cholmod_analyze(const_CHM_SP A, CHM_CM Common)
{
    static CHM_FR(*fun)(const_CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_FR(*)(const_CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_analyze");
    return fun(A, Common);
}

inline CHM_FR
cholmod_analyze_p(const_CHM_SP A, int *Perm, int *fset,
		  size_t fsize, CHM_CM Common) {
    static CHM_FR(*fun)(const_CHM_SP,int*,int*,size_t,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_FR(*)(const_CHM_SP,int*,int*,
			 size_t,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_analyze_p");
    return fun(A, Perm, fset, fsize, Common);
}

inline CHM_SP
cholmod_copy(const_CHM_SP A, int stype,
	       int mode, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_copy");
    return fun(A, stype, mode, Common);
}

inline CHM_DN
cholmod_copy_dense(const_CHM_DN  A, CHM_CM Common)
{
    static CHM_DN(*fun)(const_CHM_DN,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(const_CHM_DN,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_copy_dense");
    return fun(A, Common);
}

inline CHM_FR
cholmod_copy_factor(const_CHM_FR L, CHM_CM Common)
{
    static CHM_FR(*fun)(const_CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_FR(*)(const_CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_copy_factor");
    return fun(L, Common);
}

inline int
cholmod_change_factor(int to_xtype, int to_ll, int to_super, int to_packed,
			int to_monotonic, CHM_FR L, CHM_CM Common)
{
    static int(*fun)(int,int,int,int,int,CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(int,int,int,int,int,CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_change_factor");
    return fun(to_xtype, to_ll, to_super, to_packed, to_monotonic, L, Common);
}

inline CHM_SP
cholmod_copy_sparse(const_CHM_SP A, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_copy_sparse");
    return fun(A, Common);
}

inline CHM_SP
cholmod_factor_to_sparse(const_CHM_FR L, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_factor_to_sparse");
    return fun(L, Common);
}

inline CHM_SP
cholmod_submatrix(const_CHM_SP A, int *rset, int rsize, int *cset,
		    int csize, int values, int sorted, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,int*,int,int*,int,
			int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,int*,int,int*,
			 int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_submatrix");
    return fun(A, rset, rsize, cset, csize, values, sorted, Common);
}

inline CHM_SP
cholmod_dense_to_sparse(const_CHM_DN  X, int values, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_DN,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_DN,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_dense_to_sparse");
    return fun(X, values, Common);
}

inline int
cholmod_factorize(const_CHM_SP A, CHM_FR L, CHM_CM Common)
{
    static int(*fun)(const_CHM_SP,CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(const_CHM_SP,CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_factorize");
    return fun(A, L, Common);
}

inline int
cholmod_factorize_p(const_CHM_SP A, double *beta, int *fset,
		      size_t fsize, CHM_FR L,
		      CHM_CM Common)
{
    static int(*fun)(const_CHM_SP,double*,int*,size_t,
		     CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(const_CHM_SP,double*,int*,size_t,
		      CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_factorize_p");
    return fun(A, beta, fset, fsize, L, Common);
}

inline int
cholmod_finish(CHM_CM Common)
{

    static int(*fun)(CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_finish");
    return fun(Common);
}

inline int
cholmod_sort(CHM_SP A, CHM_CM Common)
{
    static int(*fun)(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_sort");
    return fun(A, Common);
}

inline int
cholmod_free_dense(CHM_DN  *A, CHM_CM Common)
{
    static int(*fun)(CHM_DN*,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_DN*,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_free_dense");
    return fun(A, Common);
}

inline int
cholmod_free_factor(CHM_FR *L, CHM_CM Common)
{
    static int(*fun)(CHM_FR*,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_FR*,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_free_factor");
    return fun(L, Common);
}

inline int
cholmod_free_sparse(CHM_SP *A, CHM_CM Common)
{
    static int(*fun)(CHM_SP*,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP*,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_free_sparse");
    return fun(A, Common);
}

inline int
cholmod_free_triplet(cholmod_triplet **T, CHM_CM Common)
{
    static int(*fun)(cholmod_triplet**,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(cholmod_triplet**,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_free_triplet");
    return fun(T, Common);
}

inline long int
cholmod_nnz(const_CHM_SP A, CHM_CM Common) {
    static long(*fun)(const_CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (long(*)(const_CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_nnz");
    return fun(A, Common);
}

inline int
cholmod_sdmult(const_CHM_SP A, int transpose,
		 const double *alpha, const double *beta,
		 const_CHM_DN X, CHM_DN  Y,
		 CHM_CM Common)
{
    static int(*fun)(const_CHM_SP,int,const double*,
		     const double*,const_CHM_DN,
		     CHM_DN,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(const_CHM_SP,int,const double*,
		      const double*, const_CHM_DN,
		      CHM_DN,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_sdmult");
    return fun(A, transpose, alpha, beta, X, Y, Common);
}

inline CHM_SP
cholmod_ssmult(const_CHM_SP A, const_CHM_SP B, int stype,
	       int values, int sorted, CHM_CM Common) {
    static CHM_SP(*fun)(const_CHM_SP,const_CHM_SP,
			int,int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,const_CHM_SP,
			 int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_ssmult");
    return fun(A, B, stype, values, sorted, Common);
}

inline CHM_DN
cholmod_solve(int sys, const_CHM_FR L, const_CHM_DN B, CHM_CM Common) {
    static CHM_DN(*fun)(int,const_CHM_FR,const_CHM_DN,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(int,const_CHM_FR,const_CHM_DN,
			 CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_solve");
    return fun(sys, L, B, Common);
}

inline CHM_SP
cholmod_speye(size_t nrow, size_t ncol,
		int xtype, CHM_CM Common)
{
    static CHM_SP(*fun)(size_t,size_t,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(size_t,size_t,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_speye");
    return fun(nrow, ncol, xtype, Common);
}

inline CHM_SP
cholmod_spsolve(int sys, const_CHM_FR L,
		  const_CHM_SP B, CHM_CM Common)
{
    static CHM_SP(*fun)(int,const_CHM_FR,
			const_CHM_SP, CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(int,const_CHM_FR,
			 const_CHM_SP, CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_spsolve");
    return fun(sys, L, B, Common);
}

inline int
cholmod_defaults (CHM_CM Common)
{
    static int(*fun)(CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_defaults");
    return fun(Common);
}

/* extern cholmod_common c; */

inline void
R_cholmod_error(int status, const char *file, int line, const char *message)
{
/* NB: keep in sync with R_cholmod_error(), ../../src/chm_common.c */

    if(status < 0) {
/* Note: Matrix itself uses CHM_set_common_env, CHM_store_common 
 *   and CHM_restore_common to preserve settings through error calls.
 *  Consider defining your own error handler, *and* possibly restoring
 *  *your* version of the cholmod_common that *you* use.
 */
	Rf_error("Cholmod error '%s' at file:%s, line %d", message, file, line);
    }
    else
	Rf_warning("Cholmod warning '%s' at file:%s, line %d",
		message, file, line);
}

inline int
cholmod_start(CHM_CM Common)
{
    int val;
    static int(*fun)(CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_start");
    val = fun(Common);
/*-- NB: keep in sync with  R_cholmod_start() --> ../../src/chm_common.c */
    /* do not allow CHOLMOD printing - currently */
    Common->print_function = NULL;/* was  R_cholmod_printf; /.* Rprintf gives warning */
/* Consider using your own error handler: */
    Common->error_handler = R_cholmod_error;
    return val;
}

inline CHM_SP
cholmod_transpose(const_CHM_SP A, int values, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_transpose");
    return fun(A, values, Common);
}

inline CHM_SP
cholmod_vertcat(const_CHM_SP A, const_CHM_SP B, int values, CHM_CM Common)
{
    static CHM_SP(*fun)(const_CHM_SP,const_CHM_SP,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(const_CHM_SP,const_CHM_SP, int, CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_vertcat");
    return fun(A, B, values, Common);
}

inline int
cholmod_scale(const_CHM_DN S, int scale, CHM_SP A,
		CHM_CM Common)
{
    static int(*fun)(const_CHM_DN,int,CHM_SP, CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(const_CHM_DN,int,CHM_SP, CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_scale");
    return fun(S, scale, A, Common);
}

#ifdef	__cplusplus
}
#endif
