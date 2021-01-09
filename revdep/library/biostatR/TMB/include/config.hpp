// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file
 * \brief Configuration of user template.
 */

/** \brief Configuration variables of a user template.

    Configuration variables can either be set from R with config(...,
    DLL="mymodel") or from the user template after the include
    statement:
    \code
    #include <TMB.hpp>
    config.trace.parallel     = true;
    config.trace.optimize     = true;
    config.trace.atomic       = true;
    config.optimize.instantly = true;
    config.optimize.parallel  = false;
    config.tape.parallel      = true;
    \endcode
*/
struct config_struct{
  /* Configuration variables.
     - Default values _must_be_ specified with SET(var,value) 
     - Can be either bool or integer.
  */
  struct {
    bool parallel;   /**< \brief Trace info from parallel for loops */
    bool optimize;   /**< \brief Trace tape optimization */
    bool atomic;     /**< \brief Trace construction of atomic functions */
  } trace;
  struct {
    bool instantly;  /**< \brief Always optimize just after tape creation */
    bool parallel;   /**< \brief Allow optimize in parallel (memory consuming) */
  } optimize;
  struct {
    bool parallel;   /**< \brief Enable parallel tape creation */
  } tape;
  struct {
    bool getListElement;
  } debug;

  int cmd;
  SEXP envir; /* PROTECTed because function argument - see
                 'TMBconfig' */
  void set(const char* name, bool &var, bool default_value) CSKIP(
  {
    // cmd=0: set defaults in this struct.
    // cmd=1: copy from this struct to R.
    // cmd=2: copy from R to this struct.
    SEXP name_symbol = Rf_install(name);
    if (cmd==0) var = default_value;
    if (cmd==1) Rf_defineVar(name_symbol, asSEXP(var), envir);
    if (cmd==2) var = INTEGER(Rf_findVar(name_symbol, envir))[0];
  })
#define SET(name,value)set(#name,name,value);
  void set() CSKIP(
  {
    SET(trace.parallel,true);
    SET(trace.optimize,true);
    SET(trace.atomic,true);
    SET(debug.getListElement,false);
    SET(optimize.instantly,true);
    SET(optimize.parallel,false);
    SET(tape.parallel,true);
  })
#undef SET
  config_struct() CSKIP(
  {
    cmd=0;
    set();
  })
};
TMB_EXTERN config_struct config;

extern "C"
{
  SEXP TMBconfig(SEXP envir, SEXP cmd) CSKIP(
  {
    config.cmd=INTEGER(cmd)[0];
    config.envir=envir;
    config.set();
    return R_NilValue;
  })
}
