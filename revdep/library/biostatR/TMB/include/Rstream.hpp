/*
  Borrowed from Rcpp package.
  Copyright (C) 2011 - 2013  Dirk Eddelbuettel, Romain Francois and Jelmer Ypma
  License: GPL-2

  After including this file we can replace "std::cout" by "Rcout", and it 
  will be possible to correctly capture the text output from R.
*/

#include <iostream>
#include <cstdio>
#include <streambuf>

template <bool OUTPUT>
class Rstreambuf : public std::streambuf {
public:
  Rstreambuf(){}

protected:
  virtual std::streamsize xsputn(const char *s, std::streamsize n );
  virtual int overflow(int c = EOF );
  virtual int sync();
};

template <bool OUTPUT>
class Rostream : public std::ostream {
  typedef Rstreambuf<OUTPUT> Buffer ; 
  Buffer* buf ;
public:
  Rostream() : 
    std::ostream( new Buffer ), 
    buf( static_cast<Buffer*>( rdbuf() ) )
  {}
  ~Rostream() { 
    if (buf != NULL) {
      delete buf; 
      buf = NULL;
    }
  }
};

template <> inline std::streamsize Rstreambuf<true>::xsputn(const char *s, std::streamsize num ) {
  Rprintf( "%.*s", num, s ) ;
  return num ;
}
template <> inline std::streamsize Rstreambuf<false>::xsputn(const char *s, std::streamsize num ) {
  REprintf( "%.*s", num, s ) ; 
  return num ;
}
template <> inline int Rstreambuf<true>::overflow(int c ) {
  if (c != EOF) Rprintf( "%.1s", &c ) ;
  return c ;
}
template <> inline int Rstreambuf<false>::overflow(int c ) {
  if (c != EOF) REprintf( "%.1s", &c ) ;
  return c ;
}
template <> inline int Rstreambuf<true>::sync(){
  //::R_FlushConsole() ;
  return 0 ;
}
template <> inline int Rstreambuf<false>::sync(){
  //::R_FlushConsole() ;
  return 0 ;
}
TMB_EXTERN Rostream<true>  Rcout;
TMB_EXTERN Rostream<false> Rcerr;
