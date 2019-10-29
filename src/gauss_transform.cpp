
#include <Rcpp.h>
using namespace Rcpp;

//' Test function
//' @param x a vector of numbers
//' @export
// [[Rcpp::export]]
int sum_vec(NumericVector x) {
  int acc = 0;
  for(int i = 0; i < x.length(); i++)
    acc += x[i];

  return acc;
}
