#ifndef Rcpp_head
#define Rcpp_head
#include <Rcpp.h>
using namespace Rcpp;
#endif

#include <map>

#ifndef gausst_op
#define gausst_op
std::map<std::string, float> calc_mu_cpp(Rcpp::List &fit, Rcpp::StringVector &order);
Rcpp::NumericMatrix calc_sigma_cpp(Rcpp::List &fit, Rcpp::StringVector &order);
#endif
