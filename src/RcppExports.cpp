// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calc_mu
std::map<std::string, float> calc_mu(Rcpp::List fit, std::vector<std::string> order);
RcppExport SEXP _dbnR_calc_mu(SEXP fitSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type fit(fitSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_mu(fit, order));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dbnR_calc_mu", (DL_FUNC) &_dbnR_calc_mu, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dbnR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
