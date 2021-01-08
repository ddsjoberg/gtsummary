// Copyright (C) 2016 Kasper Kristensen
// License: GPL-2

#ifndef TINY_AD_H
#define TINY_AD_H

/* Standalone ? */
#ifndef R_RCONFIG_H
#include <cmath>
#include <iostream>
#define CSKIP(x) x
#endif

/* Select the vector class to use (Default: tiny_vec) */
#if defined(TINY_AD_USE_STD_VALARRAY)
#include "tiny_valarray.hpp"
#define TINY_VECTOR(type,size) tiny_vector<type, size>
#elif defined(TINY_AD_USE_EIGEN_VEC)
#include <Eigen/Dense>
#define TINY_VECTOR(type,size) Eigen::Array<type, size, 1>
#else
#include "tiny_vec.hpp"
#define TINY_VECTOR(type,size) tiny_vec<type, size>
#endif

namespace tiny_ad {
  template<class Type, class Vector>
  struct ad {
    Type value;
    Vector deriv;
    ad(){}
    ad(Type v, Vector d){value = v; deriv = d;}
    ad(double v)        {value = v; deriv.setZero();}
    ad operator+ (const ad &other) const{
      return ad(value + other.value,
		deriv + other.deriv);
    }
    ad operator+ () const{
      return *this;
    }
    ad operator- (const ad &other) const{
      return ad(value - other.value,
		deriv - other.deriv);
    }
    ad operator- () const{
      return ad(-value, -deriv);
    }
    ad operator* (const ad &other) const{
      return ad(value * other.value,
		value * other.deriv +
		deriv * other.value);
    }
    ad operator/ (const ad &other) const{
      Type res = value / other.value;
      return ad(res,
		(deriv - res * other.deriv) /
		other.value );
    }
    /* Comparison operators */
#define COMPARISON_OPERATOR(OP)			\
    template<class other>			\
    bool operator OP (const other &x) const{	\
      return (value OP x);			\
    }
    COMPARISON_OPERATOR(<)
    COMPARISON_OPERATOR(>)
    COMPARISON_OPERATOR(<=)
    COMPARISON_OPERATOR(>=)
    COMPARISON_OPERATOR(==)
    COMPARISON_OPERATOR(!=)
#undef COMPARISON_OPERATOR
    /* Combine ad with other types (constants) */
    ad operator+ (const double &x) const{
      return ad(value + x, deriv);
    }
    ad operator- (const double &x) const{
      return ad(value - x, deriv);
    }
    ad operator* (const double &x) const{
      return ad(value * x, deriv * x);
    }
    ad operator/ (const double &x) const{
      return ad(value / x, deriv / x);
    }
    /* Note: 'this' and 'other' may point to the same object */
    ad& operator+=(const ad &other){
      value += other.value;
      deriv += other.deriv;
      return *this;
    }
    ad& operator-=(const ad &other){
      value -= other.value;
      deriv -= other.deriv;
      return *this;
    }
    ad& operator*=(const ad &other){
      if (this != &other) {
	deriv *= other.value;
	deriv += other.deriv * value;
	value *= other.value;
      } else {
	deriv *= value * 2.;
	value *= value;
      }
      return *this;
    }
    ad& operator/=(const ad &other){
      value /= other.value;
      deriv -= other.deriv * value;
      deriv /= other.value;
      return *this;
    }
  };
  /* Binary operators where a constant is first argument */
  template<class T, class V>
  ad<T, V> operator+ (const double &x, const ad<T, V> &y) {
    return y + x;
  }
  template<class T, class V>
  ad<T, V> operator- (const double &x, const ad<T, V> &y) {
    return -(y - x);
  }
  template<class T, class V>
  ad<T, V> operator* (const double &x, const ad<T, V> &y) {
    return y * x;
  }
  template<class T, class V>
  ad<T, V> operator/ (const double &x, const ad<T, V> &y) {
    T value = x / y.value;
    return ad<T, V>(value, T(-value / y.value) * y.deriv);
  }
  /* Unary operators with trivial derivatives */
#define UNARY_MATH_ZERO_DERIV(F)		\
  template<class T, class V>			\
  double F (const ad<T, V> &x){			\
    return F(x.value);				\
  }
  using ::floor; using ::ceil;
  using ::trunc; using ::round;
  UNARY_MATH_ZERO_DERIV(floor)
  UNARY_MATH_ZERO_DERIV(ceil)
  UNARY_MATH_ZERO_DERIV(trunc)
  UNARY_MATH_ZERO_DERIV(round)
  template<class T>
  double sign(const T &x){return (x > 0) - (x < 0);}
  bool isfinite(const double &x)CSKIP( {return std::isfinite(x);} )
  template<class T, class V>
  bool isfinite(const ad<T, V> &x){return isfinite(x.value);}
#undef UNARY_MATH_ZERO_DERIV
  /* Unary operators with non-trivial derivatives */
#define UNARY_MATH_DERIVATIVE(F,DF)		\
  template<class T, class V>			\
  ad<T, V> F (const ad<T, V> &x){		\
    return ad<T, V>(F (x.value),		\
		    T(DF(x.value)) * x.deriv);	\
  }
  using ::exp;  using ::log;
  using ::sin;  using ::cos;  using ::tan;
  using ::sinh; using ::cosh; using ::tanh;
  using ::sqrt; using ::fabs;
  template<class T> T D_tan(const T &x) {
    T y = cos(x); return 1. / (y * y);
  }
  template<class T> T D_tanh(const T &x) {
    T y = cosh(x); return 1. / (y * y);
  }
  UNARY_MATH_DERIVATIVE(exp, exp)
  UNARY_MATH_DERIVATIVE(log, 1.0/)
  UNARY_MATH_DERIVATIVE(sin, cos)
  UNARY_MATH_DERIVATIVE(cos, -sin)
  UNARY_MATH_DERIVATIVE(tan, D_tan)
  UNARY_MATH_DERIVATIVE(sinh, cosh)
  UNARY_MATH_DERIVATIVE(cosh, sinh)
  UNARY_MATH_DERIVATIVE(tanh, D_tanh)
  UNARY_MATH_DERIVATIVE(sqrt, 0.5/sqrt)
  UNARY_MATH_DERIVATIVE(fabs, sign)
  using ::expm1; using ::log1p;
  UNARY_MATH_DERIVATIVE(expm1, exp)
  template<class T> T D_log1p(const T &x) {return 1. / (x + 1.);}
  UNARY_MATH_DERIVATIVE(log1p, D_log1p)
  /* asin, acos, atan */
  using ::asin; using ::acos; using ::atan;
  template<class T> T D_asin(const T &x) {
    return 1. / sqrt(1. - x * x);
  }
  template<class T> T D_acos(const T &x) {
    return -1. / sqrt(1. - x * x);
  }
  template<class T> T D_atan(const T &x) {
    return 1. / (1. + x * x);
  }
  UNARY_MATH_DERIVATIVE(asin, D_asin)
  UNARY_MATH_DERIVATIVE(acos, D_acos)
  UNARY_MATH_DERIVATIVE(atan, D_atan)
#undef UNARY_MATH_DERIVATIVE
  /* A few more ... */
  template<class T, class V>
  ad<T, V> pow (const ad<T, V> &x, const ad<T, V> &y){
    return exp(y * log(x));
  }
  using ::pow;
  template<class T, class V>
  ad<T, V> pow (const ad<T, V> &x, const double &y){
    return ad<T, V> (pow(x.value, y), // Note: x.value could be 0
		     T( y * pow(x.value, y - 1.) ) * x.deriv);
  }
  /* Comparison operators where a constant is first argument */
#define COMPARISON_OPERATOR_FLIP(OP1, OP2)			\
  template<class T, class V>					\
  bool operator OP1 (const double &x, const ad<T, V> &y) {	\
    return y OP2 x;						\
  }
  COMPARISON_OPERATOR_FLIP(<,>)
  COMPARISON_OPERATOR_FLIP(<=,>=)
  COMPARISON_OPERATOR_FLIP(>,<)
  COMPARISON_OPERATOR_FLIP(>=,<=)
  COMPARISON_OPERATOR_FLIP(==,==)
  COMPARISON_OPERATOR_FLIP(!=,!=)
#undef COMPARISON_OPERATOR_FLIP
  /* Utility: Return the value of a tiny_ad type */
  double asDouble(double x) CSKIP( {return x;} )
  template<class T, class V>
  double asDouble (const ad<T, V> &x){
    return asDouble(x.value);
  }
  /* Utility: Return the max absolute value of all members of a
     tiny_ad type */
  double max_fabs(double x) CSKIP( {return fabs(x);} )
  template<class T, class V>
  double max_fabs (const ad<T, V> &x){
    double ans = max_fabs(x.value);
    for(int i=0; i<x.deriv.size(); i++) {
      double tmp = max_fabs(x.deriv[i]);
      ans = (tmp > ans ? tmp : ans);
    }
    return ans;
  }
  /* R-specific derivatives (rely on Rmath)*/
#ifdef R_RCONFIG_H
  extern "C" {
    /* See 'R-API: entry points to C-code' (Writing R-extensions) */
    double	Rf_lgammafn(double);
    double	Rf_psigamma(double, double);
  }
  template<int deriv>
  double lgamma(const double &x) {
    return Rf_psigamma(x, deriv-1);
  }
  template<>
  double lgamma<0>(const double &x) CSKIP( {return Rf_lgammafn(x);} )
  double lgamma(const double &x) CSKIP( {return lgamma<0>(x);} )
  template<int deriv, class T, class V>
  ad<T, V> lgamma (const ad<T, V> &x){
    return ad<T, V> (lgamma< deriv >(x.value),
		     T(lgamma< deriv + 1 >(x.value)) * x.deriv);
  }
  template<class T, class V>
  ad<T, V> lgamma (const ad<T, V> &x){
    return lgamma<0>(x);
  }
#endif
  /* Print method */
  template<class T, class V>
  std::ostream &operator<<(std::ostream &os, const ad<T, V> &x) {
    os << "{";
    os << " value=" << x.value;
    os << " deriv=" << x.deriv;
    os << "}";
    return os;
  }

  /* Interface to higher order derivatives. Example:

     typedef tiny_ad::variable<3, 2> Float; // Track 3rd order derivs wrt. 2 parameters
     Float a (1.23, 0);                     // Let a = 1.23 have parameter index 0
     Float b (2.34, 1);                     // Let b = 2.34 have parameter index 1
     Float y = sin(a + b);                  // Run the algorithm
     y.getDeriv();                          // Get all 3rd order derivatives
  */
#define VARIABLE(order, nvar, scalartype) variable<order, nvar, scalartype>
  template<int order, int nvar, class Double=double>
  struct variable : ad< VARIABLE(order-1, nvar, Double),
			TINY_VECTOR( VARIABLE(order-1, nvar, Double) , nvar) > {
    typedef ad< VARIABLE(order-1, nvar, Double),
		TINY_VECTOR(VARIABLE(order-1, nvar, Double), nvar) > Base;
    typedef variable<order-1, nvar, Double> Type;
    static const int result_size = nvar * Type::result_size;
    variable() { /* Do not zero-initialize */ }
    variable(Base x) : Base(x) {}
    variable(double x) : Base(x) {}
    variable(double x, int id) : Base(x) {
      setid(id);
    }
    template<class Constant>
    variable(Constant x) {
      Base::value = x; Base::deriv.setZero();
    }
    template<class Constant>
    variable(Constant x, int id) {
      Base::value = x; Base::deriv.setZero();
      setid(id);
    }
    void setid(int i0, int count = 0){
      this->value.setid(i0, count);
      this->deriv[i0].setid(i0, count + 1);
    }
    TINY_VECTOR(Double, result_size) getDeriv(){
      TINY_VECTOR(Double, result_size) ans;
      int stride = result_size / nvar;
      for(int i=0; i<nvar; i++)
	ans.segment(i * stride, stride) = this->deriv[i].getDeriv();
      return ans;
    }
  };
#undef VARIABLE
  template<int nvar, class Double>
  struct variable<1, nvar, Double> : ad<Double, TINY_VECTOR(Double,nvar) >{
    typedef ad<Double, TINY_VECTOR(Double,nvar) > Base;
    static const int result_size = nvar;
    variable<1, nvar, Double>() { /* Do not zero-initialize */ }
    variable<1, nvar, Double>(Base x) : Base(x) {}
    variable<1, nvar, Double>(double x) : Base(x) {}
    variable<1, nvar, Double>(double x, int id) : Base(x) {
      setid(id);
    }
    template<class Constant>
    variable<1, nvar, Double>(Constant x) {
      Base::value = x; Base::deriv.setZero();
    }
    template<class Constant>
    variable<1, nvar, Double>(Constant x, int id) {
      Base::value = x; Base::deriv.setZero();
      setid(id);
    }
    void setid(int i0, int count = 0){
      if(count == 0)
	this->deriv[i0] = 1.0;
      if(count == 1)
	this->value = 1.0;
    }
    TINY_VECTOR(Double, nvar) getDeriv(){
      return this->deriv;
    }
  };
#undef TINY_VECTOR
} // End namespace tiny_ad

#endif
