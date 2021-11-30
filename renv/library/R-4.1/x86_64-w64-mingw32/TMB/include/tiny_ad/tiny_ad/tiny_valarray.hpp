// Copyright (C) 2016 Kasper Kristensen
// License: GPL-2

#include <valarray>
#include <cmath>
template<class Type, int n>
struct tiny_vector : std::valarray<Type> {
  tiny_vector() : std::valarray<Type>(n) {}
  template<class T>
  tiny_vector(T other) : std::valarray<Type>(other) {}
  void setZero() {for(int i=0; i<n; i++) (*this)[i] = 0;}
  std::slice_array<Type> segment(size_t start, size_t length) {
    return (*this)[std::slice(start, length, 1)];
  }
#ifdef EIGEN_CORE_H
  operator vector<Type>(){
    vector<Type> ans(n);
    for(int i=0; i<n; i++) ans[i] = (*this)[i];
    return ans;
  }
#endif
};
template<class Type, int n, class T>
struct tiny_vector<Type, n> operator*(const T &x, const tiny_vector<Type, n> &y) {
  return y * Type(x);
}
template<class Type, int n>
std::ostream &operator<<(std::ostream &os, tiny_vector<Type, n> const &x) {
  os << "[ ";
  for(int i=0; i < x.size(); i++) os << x[i] << " ";
  os << "]";
  return os;
}
