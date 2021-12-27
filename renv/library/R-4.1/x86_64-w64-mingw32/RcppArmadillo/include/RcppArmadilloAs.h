// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// RcppArmadilloAs.h: Rcpp/Armadillo glue, support for as
//
// Copyright (C)  2013 - 2015  Dirk Eddelbuettel and Romain Francois
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

#ifndef RcppArmadillo__RcppArmadilloAs__h
#define RcppArmadillo__RcppArmadilloAs__h

namespace Rcpp{
   
namespace traits {

    template <typename T> 
    class Exporter< arma::field<T> > {
    public: 
        Exporter(SEXP x) : data(x){}
        
        inline arma::field<T> get() {
            size_t n = data.size() ;
            arma::field<T> out( n ) ;
            for(size_t i=0; i<n; i++){
                out[i] = as<T>(data[i]) ;       
            }
            return out ;
        }
        
    private:
        List data ;
    }; 
        
    template <typename T> 
    class Exporter< arma::Col<T> > : public IndexingExporter< arma::Col<T>, T > {
    public: 
        Exporter(SEXP x) : IndexingExporter< arma::Col<T>, T >(x){}
    }; 

    template <typename T> 
    class Exporter< arma::Row<T> > : public IndexingExporter< arma::Row<T>, T > {
    public:
        Exporter(SEXP x) : IndexingExporter< arma::Row<T>, T >(x){}
    }; 

    template <typename T> 
    class Exporter< arma::Mat<T> > : public MatrixExporter< arma::Mat<T>, T > {
    public:
        Exporter(SEXP x) : MatrixExporter< arma::Mat<T>, T >(x){}
    }; 
                          
    template <typename T> 
    class Exporter< const arma::Mat<T>& > {
    public:  
        typedef typename Rcpp::Matrix< Rcpp::traits::r_sexptype_traits<T>::rtype > MATRIX ;
                
        Exporter(SEXP x) : mat(x) {}
                
        inline arma::Mat<T>* get(){
            return new arma::Mat<T>( mat.begin(), mat.nrow(), mat.ncol(), false ) ;
        }
                
    private:
        MATRIX mat ;
    };
    
    // 14 June 2017
    // Add support for sparse matrices other than dgCMatrix
    template <typename T>
    class Exporter< arma::SpMat<T> > {
    public:
        Exporter( SEXP x ) {
            is_stm=Rf_inherits(x, "simple_triplet_matrix");
            if (is_stm) {
                li = x;
            }
            else {
                mat = x;
            }
        }
                
        arma::SpMat<T> get(){
            const int  RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype;
            if (is_stm) {
                arma::urowvec ti = as<arma::urowvec>(li["i"]);
                arma::urowvec tj = as<arma::urowvec>(li["j"]);
                arma::Col<T> tx = li["v"];
                arma::SpMat<T> res = arma::sp_mat(true, arma::join_cols(ti, tj)-1, tx, li["nrow"], li["ncol"], true, false);
                return res;
            }
        
            IntegerVector dims = mat.slot("Dim");
            int nrow = dims[0];
            int ncol = dims[1];
            
            // Creating an empty SpMat
            arma::SpMat<T> res(static_cast<unsigned>(nrow), static_cast<unsigned>(ncol));
            
            // Get the type of sparse matrix
            std::string type = Rcpp::as<std::string>(mat.slot("class"));
            if (type == "dgCMatrix" || mat.is("dgCMatrix")) {
                IntegerVector i = mat.slot("i");
                IntegerVector p = mat.slot("p");
                Vector<RTYPE> x = mat.slot("x");
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));

                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
            }
            else if (type == "dtCMatrix" || mat.is("dtCMatrix")) {
                IntegerVector i = mat.slot("i");
                IntegerVector p = mat.slot("p");
                Vector<RTYPE> x = mat.slot("x");
                std::string diag = Rcpp::as<std::string>(mat.slot("diag"));
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
                
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
                
                if (diag == "U") {
                    res.diag().ones();
                }
            }
            else if (type == "dsCMatrix" || mat.is("dsCMatrix")) {
                IntegerVector i = mat.slot("i");
                IntegerVector p = mat.slot("p");
                Vector<RTYPE> x = mat.slot("x");
                std::string uplo = Rcpp::as<std::string>(mat.slot("uplo"));
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
                
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
                
                if (uplo == "U") {
                    res = symmatu(res);
                } else {
                    res = symmatl(res);
                }
            }
            else if (type == "dgTMatrix" || mat.is("dgTMatrix")) {
                arma::urowvec ti = mat.slot("i");
                arma::urowvec tj = mat.slot("j");
                arma::Col<T> tx = mat.slot("x");             
                
                res = arma::sp_mat(true, arma::join_cols(ti, tj), tx, nrow, ncol, true, false);
            }
            else if (type == "dtTMatrix" || mat.is("dtTMatrix")) {
                arma::urowvec ti = mat.slot("i");
                arma::urowvec tj = mat.slot("j");
                arma::Col<T> tx = mat.slot("x");
                
                res = arma::sp_mat(true, arma::join_cols(ti, tj), tx, nrow, ncol, true, false);
                if (Rcpp::as<std::string>(mat.slot("diag")) == "U") {
                    res.diag().ones();
                }
            }
            else if (type == "dsTMatrix" || mat.is("dsTMatrix")) {
                arma::urowvec ti = mat.slot("i");
                arma::urowvec tj = mat.slot("j");
                arma::Col<T> tx = mat.slot("x");

                res = arma::sp_mat(true, arma::join_cols(ti, tj), tx, nrow, ncol, true, false);
                res = Rcpp::as<std::string>(mat.slot("uplo")) == "U" ? symmatu(res) : symmatl(res);
            }
            else if (type == "dgRMatrix" || mat.is("dgRMatrix")) {
                IntegerVector rj = mat.slot("j");
                IntegerVector rp = mat.slot("p");
                Vector<RTYPE> rx = mat.slot("x");
              
                int nnz = rx.size();
                IntegerVector i = IntegerVector(nnz);
                IntegerVector p = IntegerVector(ncol + 1);
                Vector<RTYPE> x = Vector<RTYPE>(nnz);
              
                // Count the nnz in each column
                for(int n = 0; n < nnz; n++){
                    p[rj[n] + 1]++;
                }
                
                // Cumsum p
                for(int col = 0, cumsum = 0; col < ncol + 1; col++){
                    cumsum += p[col];
                    p[col] = cumsum;
                }
                
                // https://github.com/scipy/scipy/blob/master/scipy/sparse/sparsetools/csr.h#L436
                // Calculate i&x
                for(int row = 0; row < nrow; row++){
                    for(int tmp = rp[row]; tmp < rp[row + 1]; tmp++){
                        int col = rj[tmp];
                        int dest = p[col];
                    
                        i[dest] = row;
                        x[dest] = rx[tmp];
                    
                        p[col]++;
                    }
                }
                
                // Fix the p
                for(int col = 0, last = 0; col <= ncol; col++){
                    int tmp  = p[col];
                    p[col] = last;
                    last = tmp;
                }
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
                
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
            }
            else if (type == "dtRMatrix" || mat.is("dtRMatrix")) {
                IntegerVector rj = mat.slot("j");
                IntegerVector rp = mat.slot("p");
                Vector<RTYPE> rx = mat.slot("x");
                std::string diag = Rcpp::as<std::string>(mat.slot("diag"));
                
                int nnz = rx.size();
                IntegerVector i = IntegerVector(nnz);
                IntegerVector p = IntegerVector(ncol + 1);
                Vector<RTYPE> x = Vector<RTYPE>(nnz);
                
                // Count the nnz in each column
                for(int n = 0; n < nnz; n++){
                    p[rj[n] + 1]++;
                }
                
                // Cumsum p
                for(int col = 0, cumsum = 0; col < ncol + 1; col++){
                    cumsum += p[col];
                    p[col] = cumsum;
                }
                
                // https://github.com/scipy/scipy/blob/master/scipy/sparse/sparsetools/csr.h#L436
                // Calculate i&x
                for(int row = 0; row < nrow; row++){
                    for(int tmp = rp[row]; tmp < rp[row + 1]; tmp++){
                        int col = rj[tmp];
                        int dest = p[col];
                    
                        i[dest] = row;
                        x[dest] = rx[tmp];
                    
                        p[col]++;
                    }
                }
                
                // Fix the p
                for(int col = 0, last = 0; col <= ncol; col++){
                    int tmp  = p[col];
                    p[col] = last;
                    last = tmp;
                }
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
                
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
                
                if (diag == "U"){
                    res.diag().ones();
                }
            }
            else if (type == "dsRMatrix" || mat.is("dsRMatrix")) {
                IntegerVector rj = mat.slot("j");
                IntegerVector rp = mat.slot("p");
                Vector<RTYPE> rx = mat.slot("x");
                std::string uplo = Rcpp::as<std::string>(mat.slot("uplo"));
                
                int nnz = rx.size();
                IntegerVector i = IntegerVector(nnz);
                IntegerVector p = IntegerVector(ncol + 1);
                Vector<RTYPE> x = Vector<RTYPE>(nnz);
                
                // Count the nnz in each column
                for(int n = 0; n < nnz; n++){
                    p[rj[n] + 1]++;
                }
                
                // Cumsum p
                for(int col = 0, cumsum = 0; col < ncol + 1; col++){
                    cumsum += p[col];
                    p[col] = cumsum;
                }
                
                // https://github.com/scipy/scipy/blob/master/scipy/sparse/sparsetools/csr.h#L436
                // Calculate i&x
                for(int row = 0; row < nrow; row++){
                    for(int tmp = rp[row]; tmp < rp[row + 1]; tmp++){
                        int col = rj[tmp];
                        int dest = p[col];
                    
                        i[dest] = row;
                        x[dest] = rx[tmp];
                    
                        p[col]++;
                    }
                }
                
                // Fix the p
                for(int col = 0, last = 0; col <= ncol; col++){
                    int tmp  = p[col];
                    p[col] = last;
                    last = tmp;
                }
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
                
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
                
                if (uplo == "U") {
                    res = symmatu(res);
                } else {
                    res = symmatl(res);
                }
            }
            else if (type == "indMatrix" || mat.is("indMatrix")) {
                std::vector<int> i;
                IntegerVector p(ncol + 1);
                IntegerVector x(nrow, 1);
                IntegerVector perm = mat.slot("perm");
                
                typedef std::pair<int, int> Key;
                typedef std::set<Key> Set;
                Set permiSet;
                
                // Sort i;
                int nnz = perm.size();
                for(int tmp = 0; tmp < nnz; tmp++){
                    Key permi(perm[tmp], tmp);
                    permiSet.insert(permi);
                }
                
                for(Set::iterator tmp = permiSet.begin(); tmp != permiSet.end(); tmp++){
                    i.push_back(tmp -> second);
                }
                
                // Count the number of nnz in each column
                for(int idx = 0; idx < nnz; idx++){
                    int col = perm[idx];
                    p[col]++;
                }
                
                // Cumsum p
                for(int col = 0, cumsum = 0; col < ncol + 1; col++){
                    cumsum += p[col];
                    p[col] = cumsum;
                }
                
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
                
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
            }
            else if (type == "pMatrix" || mat.is("pMatrix")) {
                std::vector<int> i;
                IntegerVector p(ncol + 1);
                IntegerVector x(ncol, 1);
                IntegerVector perm = mat.slot("perm");
                
                // Sort the row number by the column number
                typedef std::map <int, int> Map;
                Map colrow;
                for(int tmp = 0; tmp < perm.size(); tmp++){
                    colrow[perm[tmp]] = tmp;
                }
                
                // Calculate i
                for(Map::iterator tmp = colrow.begin(); tmp != colrow.end(); tmp++){
                    i.push_back(tmp -> second);
                }
              
                // Calculate p
                for(int tmp = 0; tmp < p.size(); tmp++){
                    p[tmp] = tmp;
                }
              
                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));
              
                // In order to access the internal arrays of the SpMat class
                res.sync();
                
                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
            }
            else if (type == "ddiMatrix" || mat.is("ddiMatrix")) {
                std::vector<int> i;
                std::vector<int> p;
                std::vector<double> x;
                std::string diag = Rcpp::as<std::string>(mat.slot("diag"));
                
                if (diag == "U") {
                    for(int idx = 0; idx < ncol; idx++){
                        i.push_back(idx);
                        p.push_back(idx);
                        x.push_back(1);
                    }
                    p.push_back(ncol);
                } else {
                    Vector<RTYPE> tmpx = mat.slot("x");
                    int tmpp = 0;
                    for(int idx = 0; idx < ncol; idx++){
                        p.push_back(tmpp);
                        if (tmpx[idx] != 0) {
                            i.push_back(idx);
                            x.push_back(tmpx[idx]);
                            tmpp++;
                        }
                    }
                    p.push_back(tmpp);
                }

                // Making space for the elements
                res.mem_resize(static_cast<unsigned>(x.size()));

                // In order to access the internal arrays of the SpMat class
                res.sync();

                // Copying elements
                std::copy(i.begin(), i.end(), arma::access::rwp(res.row_indices));
                std::copy(p.begin(), p.end(), arma::access::rwp(res.col_ptrs));
                std::copy(x.begin(), x.end(), arma::access::rwp(res.values));
            }
            else {
                Rcpp::stop(type + " is not supported.");
            }
            
            // In order to access the internal arrays of the SpMat class
            res.sync();

            // Setting the sentinel
            arma::access::rw(res.col_ptrs[static_cast<unsigned>(ncol + 1)]) =
              std::numeric_limits<arma::uword>::max();
                        
            return res;
        }
        
        
    private:
        S4 mat ;
        List li;
        bool is_stm;
    } ;
    
    // 30 November 2015
    // default Exporter-Cube specialization:
    // handles cube, icube, and cx_cube
    // fails on fcube, ucube, and cx_fcube
    template <typename T>
    class Exporter< arma::Cube<T> > {
    public:
        typedef arma::Cube<T> cube_t;
        enum { RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype };
        typedef typename Rcpp::traits::storage_type<RTYPE>::type value_t;
        Exporter(SEXP x) : vec(x) {}
      
        cube_t get() {
            Rcpp::Vector<INTSXP> dims = vec.attr("dim");
            if (dims.size() != 3) {
                std::string msg = 
                  "Error converting object to arma::Cube<T>:\n"
                  "Input array must have exactly 3 dimensions.\n";
                Rcpp::stop(msg);
            }
          
            cube_t result(
                reinterpret_cast<T*>(vec.begin()),
                dims[0], dims[1], dims[2], false);
            return result;
        }
      
    private:
        Rcpp::Vector<RTYPE> vec;
    };
    
    // specializations for 3 cube typedefs that fail above
    // first use viable conversion SEXP -> Cube<other_t>
    // then use conv_to<cube_t>::from(other_t other)
    template <>
    class Exporter<arma::fcube> {
    public:
        typedef arma::fcube cube_t;
        
        Exporter(SEXP x)
            : tmp(Exporter<arma::cube>(x).get()) {}
        
        cube_t get() {
            cube_t result = arma::conv_to<cube_t>::from(tmp);
            return result;
        }
      
    private:
        typedef arma::cube other_t;
        other_t tmp;
    };
    
    template <>
    class Exporter<arma::ucube> {
    public:
        typedef arma::ucube cube_t;
      
        Exporter(SEXP x)
            : tmp(Exporter<arma::icube>(x).get()) {}
        
        cube_t get() {
            cube_t result = arma::conv_to<cube_t>::from(tmp);
            return result;
        }
      
    private:
        typedef arma::icube other_t;
        other_t tmp;
    };
    
    template <>
    class Exporter<arma::cx_fcube> {
    public:
        typedef arma::cx_fcube cube_t;
      
        Exporter(SEXP x)
            : tmp(Exporter<arma::cx_cube>(x).get()) {}
      
        cube_t get() {
            cube_t result = arma::conv_to<cube_t>::from(tmp);
            return result;
        }
      
    private:
        typedef arma::cx_cube other_t;
        other_t tmp;
    };

} // end traits
        
    /* Begin Armadillo vector as support classes */
    
    template <typename T, typename MAT, typename REF, 
              typename NEEDS_CAST = typename Rcpp::traits::r_sexptype_needscast<T>::type>
    class ArmaMat_InputParameter;
    
    template <typename T, typename MAT, typename REF>
    class ArmaMat_InputParameter<T, MAT, REF, Rcpp::traits::false_type> {
    public:
        ArmaMat_InputParameter(SEXP x_) : m(x_), mat(reinterpret_cast<T*>(m.begin()), m.nrow(), m.ncol(), false) {} 
                        
        inline operator REF(){
            return mat ;        
        }
                        
    private:
        Rcpp::Matrix< Rcpp::traits::r_sexptype_traits<T>::rtype > m ;
        MAT mat ;
    } ;
    
    template <typename T, typename MAT, typename REF>
    class ArmaMat_InputParameter<T, MAT, REF, Rcpp::traits::true_type> {
    public:
        ArmaMat_InputParameter( SEXP x_ ): m(x_), mat( as<MAT>(m) ) {}
                        
        inline operator REF(){
            return mat ;        
        }
                        
    private:
        Rcpp::Matrix< Rcpp::traits::r_sexptype_traits<T>::rtype > m ;
        MAT mat ;
    } ;

    /* End Armadillo vector as support classes */


    /* Begin Armadillo vector as support classes */
    
    template <typename T, typename VEC, typename REF, 
              typename NEEDS_CAST = typename Rcpp::traits::r_sexptype_needscast<T>::type>
    class ArmaVec_InputParameter;
    
    template <typename T, typename VEC, typename REF>
    class ArmaVec_InputParameter<T, VEC, REF, Rcpp::traits::false_type> {
    public:
        ArmaVec_InputParameter( SEXP x_ ) : v(x_), vec( reinterpret_cast<T*>( v.begin() ), v.size(), false ){}
                        
        inline operator REF(){
            return vec ;        
        }
                        
    private:
        Rcpp::Vector< Rcpp::traits::r_sexptype_traits<T>::rtype > v ;
        VEC vec ;
    } ;
    
    template <typename T, typename VEC, typename REF>
    class ArmaVec_InputParameter<T, VEC, REF, Rcpp::traits::true_type> {
    public:
        ArmaVec_InputParameter( SEXP x_ ): v(x_), vec( as<VEC>(v) ) {}
                        
        inline operator REF(){
            return vec ;        
        }
                        
    private:
        Rcpp::Vector< Rcpp::traits::r_sexptype_traits<T>::rtype > v ;
        VEC vec ;
    } ;
    
    /* End Armadillo vector as support classes */
    
#define MAKE_INPUT_PARAMETER(INPUT_TYPE,TYPE,REF)                       \
    template <typename T>                                               \
    class INPUT_TYPE<TYPE> : public ArmaVec_InputParameter<T, TYPE, REF >{ \
    public:                                                             \
    INPUT_TYPE( SEXP x) : ArmaVec_InputParameter<T, TYPE, REF >(x){}    \
    } ;                                                                                                                  
    
    MAKE_INPUT_PARAMETER(ConstReferenceInputParameter, arma::Col<T>, const arma::Col<T>& )
    MAKE_INPUT_PARAMETER(ReferenceInputParameter     , arma::Col<T>, arma::Col<T>&       )
    MAKE_INPUT_PARAMETER(ConstInputParameter         , arma::Col<T>, const arma::Col<T>  )
    
    MAKE_INPUT_PARAMETER(ConstReferenceInputParameter, arma::Row<T>, const arma::Row<T>& )
    MAKE_INPUT_PARAMETER(ReferenceInputParameter     , arma::Row<T>, arma::Row<T>&       )
    MAKE_INPUT_PARAMETER(ConstInputParameter         , arma::Row<T>, const arma::Row<T>  )
    
#undef MAKE_INPUT_PARAMETER

    
#define MAKE_INPUT_PARAMETER(INPUT_TYPE,TYPE,REF)                       \
    template <typename T>                                               \
    class INPUT_TYPE<TYPE> : public ArmaMat_InputParameter<T, TYPE, REF >{ \
    public:                                                             \
    INPUT_TYPE( SEXP x) : ArmaMat_InputParameter<T, TYPE, REF >(x){} \
    } ;
 
    MAKE_INPUT_PARAMETER(ConstReferenceInputParameter, arma::Mat<T>, const arma::Mat<T>& )
    MAKE_INPUT_PARAMETER(ReferenceInputParameter     , arma::Mat<T>, arma::Mat<T>&       )
    MAKE_INPUT_PARAMETER(ConstInputParameter         , arma::Mat<T>, const arma::Mat<T>  )

#undef MAKE_INPUT_PARAMETER
    
}

#endif

