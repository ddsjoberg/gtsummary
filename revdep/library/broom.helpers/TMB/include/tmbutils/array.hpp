// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/**
   \file vector.hpp
   Array templates using inheritance.
*/

/** \brief Array class used by TMB.

    The TMB array class is implemented as an Eigen Array of
    dynamic length with a dimension attribute. The implementation
    closely follows the way arrays work in R.
    Vectorized operations are inherited from the Eigen library.

    \warning Methods that are not documented here are inherited from
    the Eigen library and applied on the underlying n-by-1 array. In
    general this will yield surprising results for 2D specific
    array methods.
*/
template<class Type>
struct array:Map< Array<Type,Dynamic,1> >{

  typedef Array<Type,Dynamic,1> Base;
  typedef Map< Base > MapBase;

  vector<int> dim;

  /* def: inner product sum(mult*tuple) gives vector index
     example: array a(4,5,6);
     dim=[4,5,6] first dimension runs fastest, last dimension slowest:
     mult=[1,1*4,1*4*5]
  */
  vector<int> mult;

  Base vectorcopy; /* Array data */

  /** \brief Sets dimension attribute and updates internal stride. **Must** be used when e.g. collapsing array dimensions. */
  void setdim(vector<int> dim_){
    dim=dim_;
    mult.resize(dim.size());
    mult[0]=1;
    for(int k=1;k<dim.size();k++){
      mult[k]=mult[k-1]*dim[k-1];
    }
  }
  void initZeroArray(vector<int> dim_){
    vectorcopy.resize(dim_.prod());
    vectorcopy.setZero();
    if (vectorcopy.size() > 0) {
      new (this) MapBase(&vectorcopy[0],vectorcopy.size()); /* Eigen manual: Despite appearances, this does not invoke the memory allocator... */
    }
    setdim(dim_);
  }

  /* Utility function */
  vector<int> c(int n1){
    vector<int> ans(1);
    ans << n1;
    return ans;
  }
  vector<int> c(int n1, int n2){
    vector<int> ans(2);
    ans << n1,n2;
    return ans;
  }
  vector<int> c(int n1, int n2, int n3){
    vector<int> ans(3);
    ans << n1,n2,n3;
    return ans;
  }
  vector<int> c(int n1, int n2, int n3, int n4){
    vector<int> ans(4);
    ans << n1,n2,n3,n4;
    return ans;
  }
  vector<int> c(int n1, int n2, int n3, int n4, int n5){
    vector<int> ans(5);
    ans << n1,n2,n3,n4,n5;
    return ans;
  }
  vector<int> c(int n1, int n2, int n3, int n4, int n5, int n6){
    vector<int> ans(6);
    ans << n1,n2,n3,n4,n5,n6;
    return ans;
  }
  vector<int> c(int n1, int n2, int n3, int n4, int n5, int n6, int n7){
    vector<int> ans(7);
    ans << n1,n2,n3,n4,n5,n6,n7;
    return ans;
  }


  /* Default constructor: e.g. array<double> a; */
  array():MapBase(NULL,0){};

  /** \brief Construct array from dimension vector and fill with zeros. */
  array(vector<int> dim_):MapBase(NULL,0){
    initZeroArray(dim_);
  }
  array(int n1):MapBase(NULL,0){initZeroArray(c(n1));}
  array(int n1, int n2):MapBase(NULL,0){initZeroArray(c(n1,n2));}
  array(int n1, int n2, int n3):MapBase(NULL,0){initZeroArray(c(n1,n2,n3));}
  array(int n1, int n2, int n3, int n4):MapBase(NULL,0){initZeroArray(c(n1,n2,n3,n4));}
  array(int n1, int n2, int n3, int n4, int n5):MapBase(NULL,0){initZeroArray(c(n1,n2,n3,n4,n5));}
  array(int n1, int n2, int n3, int n4, int n5, int n6):MapBase(NULL,0){initZeroArray(c(n1,n2,n3,n4,n5,n6));}
  array(int n1, int n2, int n3, int n4, int n5, int n6, int n7):MapBase(NULL,0){initZeroArray(c(n1,n2,n3,n4,n5,n6,n7));}

  /* Default construction: always deep copy ! */
  template<class T>
  array(T &x, vector<int> dim_):MapBase(NULL,0),vectorcopy(x){
    if(x.size()>0){
      new (this) MapBase(&vectorcopy[0],x.size()); /* Eigen manual: Despite appearances, this does not invoke the memory allocator... */
    }
    setdim(dim_);
  }
  /* Deep copy existing array - as above with dim_=x.dim  */
  template<class T>
  array(array<T> &x):MapBase(NULL,0),vectorcopy(x){
    if(x.size()>0){
      new (this) MapBase(&vectorcopy[0],x.size()); /* Eigen manual: Despite appearances, this does not invoke the memory allocator... */
    }
    setdim(x.dim);
  }

  /* Sometimes we want a reference only... See col() */
  array(Type *p, vector<int> dim_):MapBase(p,dim_.prod()){
    setdim(dim_);
  }

  /* 
     Example:
     array<double> a(3,4);
     array<double> c=a;        // Assign
     a.col(1)=c.col(1);        // Assign
     array<double> b=a.col(1); // Initialize (not this method)
  */
  array & operator= (const array & other)
  {
    if(this->dim.size() == 0){ // Not initialized (default constructed)
      this->initZeroArray(other.dim);
    }
    this->MapBase::operator=(other);
    this->setdim(other.dim);
    return *this;
  }

  /* Assign array from any other object. First convert other object to
     1D object (because we assign to underlying 1D array data).

     Example:
     a = m         (assign from matrix)
     a.col(0) = m; (assign from matrix)
     a = m*m       (assign from unevaluated expression template)
     a = v         (assign from vector)
  */
  template <class T>
  array<Type> operator=(T y){
    Array<Type, Dynamic, Dynamic> a = y;
    a.resize(a.size(),1);
    return array(MapBase::operator=(a), dim);
  }

  void print(){
    Rcout << "Array dim: ";
    for(int i=0;i<dim.size();i++)Rcout << dim[i] << " ";
    Rcout << "\n";
    Rcout << "Array val: ";
    for(int i=0;i<this->MapBase::size();i++)Rcout << this->MapBase::operator[](i) << " ";
    Rcout << "\n";
  };

  /** \brief Number of outer-most dimensions
      \return Last array dimension (Similar to R's ncol in 2D case
      only)
   */
  int cols(){
    return dim[dim.size()-1];
  }

  /** \brief Number of inner-most dimensions
      \return First array dimension (Similar to R's nrow)
   */
  int rows(){
    return dim[0];
  }

  /** \brief Extract sub-array with write access 
      Index i refers to the outer-most (i.e. final) dimension.
   */
  array<Type> col(int i){
    int nslice=this->MapBase::size()/this->cols();
    Type* p=&(this->MapBase::operator()(i*nslice));
    vector<int> newdim;
    if(dim.size()>1){
      newdim=dim.segment(0,dim.size()-1);
    } else {
      newdim.resize(1);
      newdim << 1;
    }
    return array(p,newdim);
  }

  /** \brief Elementwise subsetting 1D array.
      Also allowed in general to access the underlying vector of n-dim array. */
  Type& operator()(int i1){
    return this->MapBase::operator[](i1);
  }
  /** \brief Elementwise subsetting 2D array */
  Type& operator()(int i1, int i2){
    return this->MapBase::operator[](index(c(i1,i2)));
  }
  /** \brief Elementwise subsetting 3D array */
  Type& operator()(int i1, int i2, int i3){
    return this->MapBase::operator[](index(c(i1,i2,i3)));
  }
  /** \brief Elementwise subsetting 4D array */
  Type& operator()(int i1, int i2, int i3, int i4){
    return this->MapBase::operator[](index(c(i1,i2,i3,i4)));
  }
  /** \brief Elementwise subsetting 5D array */
  Type& operator()(int i1, int i2, int i3, int i4, int i5){
    return this->MapBase::operator[](index(c(i1,i2,i3,i4,i5)));
  }
  /** \brief Elementwise subsetting 6D array */
  Type& operator()(int i1, int i2, int i3, int i4, int i5, int i6){
    return this->MapBase::operator[](index(c(i1,i2,i3,i4,i5,i6)));
  }
  /** \brief Elementwise subsetting 7D array */
  Type& operator()(int i1, int i2, int i3, int i4, int i5, int i6, int i7){
    return this->MapBase::operator[](index(c(i1,i2,i3,i4,i5,i6,i7)));
  }


  int index(vector<int> tup){
    eigen_assert( tup.size() == dim.size() );
    eigen_assert( ( (dim*0 <= tup) && (tup < dim) ).all() );
    return (tup*mult).sum();
  }
  vector<int> tuple(int i){
    vector<int> revtup(dim.size());
    vector<int> revmult=mult.reverse();
    for(int k=0;k<dim.size();k++){
      revtup[k]=i/revmult[k];
      i=i-revtup[k]*revmult[k];
    }    
    return revtup.reverse();
  }

  /** \brief Array permutation.
      Permutes array dimensions corresponding to permutation vector p.
   */
  array<Type> perm(vector<int> p){
    vector<Type> x(this->size());
    array<Type> ans(x,dim(p));       /* Create new array with permuted dimension */
    for(int i=0;i<this->size();i++){ /* Loop through values of old array */
      ans[ans.index(tuple(i)(p))]=this->operator[](i);
    }
    return ans;
  }
  /** \brief Array transpose (Special case of array permutation) 

      If e.g. x has dimension [3,4,5,6] then x.transpose() has
      dimension [6,5,4,3].
   */
  array<Type> transpose(){
    vector<int> p(dim.size());
    for(int i=0;i<p.size();i++)p[i]=i;
    return this->perm(p.reverse());
  }

  int mod(int i,int n){return ((i%n)+n)%n;}
  /** \brief Array rotate (Special case of array permutation) 

      Rotates array dimension with n steps where n can be any
      (positive or negative) integer.
      If e.g. x has dimension [3,4,5,6] then x.rotate(1) has
      dimension [6,3,4,5].
   */
  array<Type> rotate(int n){
    vector<int> p(dim.size());
    for(int i=0;i<p.size();i++)p[i]=mod(i-n,p.size());
    return this->perm(p);
  }

#define INHERIT(OP)					\
  template <class T>					\
  array<Type> OP(T y){return array(MapBase::OP(y),dim);}
  INHERIT(operator+)
  INHERIT(operator-)
  INHERIT(operator*)
  INHERIT(operator/)
#undef INHERIT

  /** \brief Convert TMB array to matrix by keeping the first
      dimension and collapsing remaining dimensions.

      E.g. if array dimension is (n1,n2,n3) the resulting matrix
      dimension is (n1, n2*n3). */
  tmbutils::matrix<Type> matrix(){
    tmbutils::matrix<Type> ans = this->MapBase::matrix();
    ans.resize(this->rows(), ans.size() / this->rows() );
    return ans;
  }

  /** \brief Convert TMB array to vector */
  tmbutils::vector<Type> vec() { return *this; }

  /* Methods this class should *not* inherit (generate compile time error if used) */
  private:
  using MapBase::row;
};

