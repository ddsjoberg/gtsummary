// Copyright (C) 2015 Hans Skaug
// License: GPL-2

/**
   \file R_inla.hpp
   Makes SPDE methods from INLA R-package available in TMB.
*/

/** \brief SPDE methods from INLA R-package .

Constructs precission matrix Q of the type defined in Lindgren et al. (2011).
Requires RINLA package to build stuff. Q built this way can be used in \ref GMRF_t. 

*/

namespace R_inla {
using namespace Eigen;
using namespace tmbutils;

/** \brief Object containing all elements of an SPDE object, i.e. eqn (10) in Lindgren et al. */	
template<class Type>
struct spde_t{  
  SparseMatrix<Type> M0;	// G0 eqn (10) in Lindgren 
  SparseMatrix<Type> M1;	// G1 eqn (10) in Lindgren 
  SparseMatrix<Type> M2;	// G2 eqn (10) in Lindgren 
  spde_t(SEXP x){  /* x = List passed from R */
  M0 = asSparseMatrix<Type>(getListElement(x,"M0"));
  M1 = asSparseMatrix<Type>(getListElement(x,"M1"));
  M2 = asSparseMatrix<Type>(getListElement(x,"M2"));
}
};

/** Precision matrix eqn (10) in Lindgren et al. (2011) */    
template<class Type>
  SparseMatrix<Type> Q_spde(spde_t<Type> spde, Type kappa){
  Type kappa_pow2 = kappa*kappa;
  Type kappa_pow4 = kappa_pow2*kappa_pow2;
  	
  return kappa_pow4*spde.M0 + Type(2.0)*kappa_pow2*spde.M1 + spde.M2;    // M0=G0, M1=G1, M2=G2
}

/** \brief Object containing all elements of an anisotropic SPDE object, i.e. eqn (20) in Lindgren et al. */	
template<class Type>
struct spde_aniso_t{
  int n_s;
  int n_tri;
  vector<Type> Tri_Area;
  matrix<Type> E0;
  matrix<Type> E1;
  matrix<Type> E2;
  matrix<int>  TV;
  SparseMatrix<Type> G0;
  SparseMatrix<Type> G0_inv;
  spde_aniso_t(SEXP x){  /* x = List passed from R */
  n_s = 	CppAD::Integer(asVector<Type>(getListElement(x,"n_s"))[0]);  
  n_tri = 	CppAD::Integer(asVector<Type>(getListElement(x,"n_tri"))[0]);  
  Tri_Area = asVector<Type>(getListElement(x,"Tri_Area"));
  E0 = asMatrix<Type>(getListElement(x,"E0"));
  E1 = asMatrix<Type>(getListElement(x,"E1"));
  E2 = asMatrix<Type>(getListElement(x,"E2"));  
  TV = asMatrix<int>(getListElement(x,"TV")); 
  G0 = asSparseMatrix<Type>(getListElement(x,"G0"));
  G0_inv = asSparseMatrix<Type>(getListElement(x,"G0_inv"));
  
}
};


/** Precision matrix for the anisotropic case, eqn (20) in Lindgren et al. (2011) */    
template<class Type>
  SparseMatrix<Type> Q_spde(spde_aniso_t<Type> spde, Type kappa, matrix<Type> H){

  int i;
  Type kappa_pow2 = kappa*kappa;
  Type kappa_pow4 = kappa_pow2*kappa_pow2;
  
  int n_s = spde.n_s;
  int n_tri = spde.n_tri;
  vector<Type> Tri_Area = spde.Tri_Area;
  matrix<Type> E0 = spde.E0;
  matrix<Type> E1 = spde.E1;
  matrix<Type> E2 = spde.E2;
  matrix<int> TV = spde.TV;
  SparseMatrix<Type> G0 = spde.G0;
  SparseMatrix<Type> G0_inv = spde.G0_inv;
	  	  
  //Type H_trace = H(0,0)+H(1,1);
  //Type H_det = H(0,0)*H(1,1)-H(0,1)*H(1,0);
  SparseMatrix<Type> G1_aniso(n_s,n_s); 
  SparseMatrix<Type> G2_aniso(n_s,n_s); 
  // Calculate adjugate of H
  matrix<Type> adj_H(2,2);
  adj_H(0,0) = H(1,1);
  adj_H(0,1) = -1 * H(0,1);
  adj_H(1,0) = -1 * H(1,0);
  adj_H(1,1) = H(0,0);
  // Calculate new SPDE matrices

  // Calculate G1 - pt. 1
  array<Type> Gtmp(n_tri,3,3);
  for(i=0; i<n_tri; i++){    
    // 1st line: E0(i,) %*% adjH %*% t(E0(i,)), etc.    
    Gtmp(i,0,0) = (E0(i,0)*(E0(i,0)*adj_H(0,0)+E0(i,1)*adj_H(1,0)) + E0(i,1)*(E0(i,0)*adj_H(0,1)+E0(i,1)*adj_H(1,1))) / (4*Tri_Area(i));  
    Gtmp(i,0,1) = (E1(i,0)*(E0(i,0)*adj_H(0,0)+E0(i,1)*adj_H(1,0)) + E1(i,1)*(E0(i,0)*adj_H(0,1)+E0(i,1)*adj_H(1,1))) / (4*Tri_Area(i));  
    Gtmp(i,0,2) = (E2(i,0)*(E0(i,0)*adj_H(0,0)+E0(i,1)*adj_H(1,0)) + E2(i,1)*(E0(i,0)*adj_H(0,1)+E0(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
    Gtmp(i,1,1) = (E1(i,0)*(E1(i,0)*adj_H(0,0)+E1(i,1)*adj_H(1,0)) + E1(i,1)*(E1(i,0)*adj_H(0,1)+E1(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
    Gtmp(i,1,2) = (E2(i,0)*(E1(i,0)*adj_H(0,0)+E1(i,1)*adj_H(1,0)) + E2(i,1)*(E1(i,0)*adj_H(0,1)+E1(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
    Gtmp(i,2,2) = (E2(i,0)*(E2(i,0)*adj_H(0,0)+E2(i,1)*adj_H(1,0)) + E2(i,1)*(E2(i,0)*adj_H(0,1)+E2(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
  }
  // Calculate G1 - pt. 2
  for(i=0; i<n_tri; i++){
    G1_aniso.coeffRef(TV(i,1),TV(i,0)) = G1_aniso.coeffRef(TV(i,1),TV(i,0)) + (Gtmp(i,0,1));  
    G1_aniso.coeffRef(TV(i,0),TV(i,1)) = G1_aniso.coeffRef(TV(i,0),TV(i,1)) + (Gtmp(i,0,1));  
    G1_aniso.coeffRef(TV(i,2),TV(i,1)) = G1_aniso.coeffRef(TV(i,2),TV(i,1)) + (Gtmp(i,1,2));  
    G1_aniso.coeffRef(TV(i,1),TV(i,2)) = G1_aniso.coeffRef(TV(i,1),TV(i,2)) + (Gtmp(i,1,2));  
    G1_aniso.coeffRef(TV(i,2),TV(i,0)) = G1_aniso.coeffRef(TV(i,2),TV(i,0)) + (Gtmp(i,0,2));  
    G1_aniso.coeffRef(TV(i,0),TV(i,2)) = G1_aniso.coeffRef(TV(i,0),TV(i,2)) + (Gtmp(i,0,2));  
    G1_aniso.coeffRef(TV(i,0),TV(i,0)) = G1_aniso.coeffRef(TV(i,0),TV(i,0)) + (Gtmp(i,0,0));  
    G1_aniso.coeffRef(TV(i,1),TV(i,1)) = G1_aniso.coeffRef(TV(i,1),TV(i,1)) + (Gtmp(i,1,1));  
    G1_aniso.coeffRef(TV(i,2),TV(i,2)) = G1_aniso.coeffRef(TV(i,2),TV(i,2)) + (Gtmp(i,2,2));  
  }
  G2_aniso = G1_aniso * G0_inv * G1_aniso; 

  return kappa_pow4*G0 + Type(2.0)*kappa_pow2*G1_aniso + G2_aniso;
}

} // end namespace R_inla


