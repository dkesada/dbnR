#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _dbnR_calc_mu_cpp(SEXP, SEXP);
extern SEXP _dbnR_calc_sigma_cpp(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_dbnR_calc_mu_cpp",    (DL_FUNC) &_dbnR_calc_mu_cpp,    2},
  {"_dbnR_calc_sigma_cpp", (DL_FUNC) &_dbnR_calc_sigma_cpp, 2},
  {NULL, NULL, 0}
};

void R_init_dbnR(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}