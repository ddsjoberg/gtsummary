// Copyright (C) 2016 Kasper Kristensen
// License: GPL-2

/* Simple vector class that can be used with tiny_ad */
template <class Type>
struct tiny_vec_ref {
  Type *p;
  size_t n;
  tiny_vec_ref(Type *p_, size_t n_) : p(p_), n(n_) {}
  template<class T>
  tiny_vec_ref &operator=(const T &other) {
    for(size_t i = 0; i < n; i++) p[i] = other[i];
    return *this;
  }
};
template <class Type, int n>
struct tiny_vec {
  Type data[n];
  tiny_vec(){}
  tiny_vec(const tiny_vec &other) {
    for(int i=0; i<n; i++) data[i] = other.data[i];
  }
  tiny_vec(const Type &other) {
    for(int i=0; i<n; i++) data[i] = other;
  }
  void resize(size_t length){ /* Ignore - this is fixed size */ }
  int size() const { return n; }
  Type operator[] (size_t i) const { return data[i]; }
  Type &operator[] (size_t i) { return data[i]; }
  void setZero() {for(int i=0; i<n; i++) (*this)[i] = 0;}
  tiny_vec_ref<Type> segment(size_t start, size_t length) {
    tiny_vec_ref<Type> ans(&(data[start]), length);
    return ans;
  }
#define VBINARY_OPERATOR(OP)						\
  tiny_vec operator OP (const tiny_vec &other) const {			\
    tiny_vec ans;							\
    for(int i=0; i<n; i++) ans.data[i] = data[i] OP other.data[i];	\
    return ans;								\
  }									\
  template<class Scalar>						\
  tiny_vec operator OP (const Scalar &other) const {			\
    tiny_vec ans;							\
    for(int i=0; i<n; i++) ans.data[i] = data[i] OP other;		\
    return ans;								\
  }
  VBINARY_OPERATOR(+)
  VBINARY_OPERATOR(-)
  VBINARY_OPERATOR(*)
  VBINARY_OPERATOR(/)
#define VUNARY_OPERATOR(OP)					\
  tiny_vec operator OP () const {				\
    tiny_vec ans;						\
    for(int i=0; i<n; i++) ans.data[i] = OP (*this).data[i];	\
    return ans;							\
  }
  VUNARY_OPERATOR(+)
  VUNARY_OPERATOR(-)
#define COMPOUND_ASSIGNMENT_OPERATOR(OP)		\
  tiny_vec& operator OP (const Type &other) {		\
    for(int i=0; i<n; i++) (*this).data[i] OP other;	\
    return *this;					\
  }							\
  tiny_vec& operator OP (const tiny_vec &other) {	\
    for(int i=0; i<n; i++) (*this).data[i] OP other[i];	\
    return *this;					\
  }
  COMPOUND_ASSIGNMENT_OPERATOR(+=)
  COMPOUND_ASSIGNMENT_OPERATOR(-=)
  COMPOUND_ASSIGNMENT_OPERATOR(*=)
  COMPOUND_ASSIGNMENT_OPERATOR(/=)

  #ifdef EIGEN_CORE_H
  operator vector<Type>(){
    vector<Type> ans(n);
    for(int i=0; i<n; i++) ans[i] = (*this)[i];
    return ans;
  }
  #endif
};

template<class Type, int n>
tiny_vec<Type, n> operator* (const Type &x, const tiny_vec<Type, n> &y) {
  return y.operator* (x);
}

template<class Type, int n>
std::ostream &operator<<(std::ostream &os, tiny_vec<Type, n> const &x) {
  os << "[ ";
  for(int i=0; i < x.size(); i++) os << x[i] << " ";
  os << "]";
  return os;
}
