#ifndef __GDTOOLS_TYPES__
#define __GDTOOLS_TYPES__

#include <RcppCommon.h>

class FontMetric {
public:
  double height, width, ascent, descent;

  FontMetric();
  FontMetric(SEXP);
  operator SEXP() const ;
};

#include <Rcpp.h>
using namespace Rcpp;

inline FontMetric::FontMetric() {}

inline FontMetric::FontMetric(SEXP x_) {
  Rcpp::NumericVector x(x_);
  if (x.size() != 4)
    Rcpp::stop("Invalid size");

  width = x[0];
  height = x[1];
  ascent = x[2];
  descent = x[3];
}

inline FontMetric::operator SEXP() const {
  return Rcpp::NumericVector::create(width, height, ascent, descent);
}

typedef struct _cairo_font_face cairo_font_face_t;

class CairoContext {
  struct CairoContext_;
  CairoContext_* cairo_;
  typedef std::map<std::string, cairo_font_face_t*> fontCache;

public:
  CairoContext();
  ~CairoContext();

  void cacheFont(fontCache& cache, std::string& key, std::string fontfile, int fontindex);
  void cacheSystemFont(std::string& key, std::string& fontname, bool bold, bool italic);
  void setFont(std::string fontname = "sans", double fontsize = 12,
               bool bold = false, bool italic = false,
               std::string fontfile = "");
  void setUserFont(std::string& fontname, double fontsize,
                   bool bold, bool italic, std::string& fontfile);
  void setSystemFont(std::string& fontname, double fontsize,
                     bool bold, bool italic);

  FontMetric getExtents(std::string x);
  bool validateGlyphs(std::string x);

};

typedef Rcpp::XPtr<CairoContext> XPtrCairoContext;


#endif
