#include <Rcpp.h>
using namespace Rcpp;
#include <map>

//' Calculate the mu vector of means of a Gaussian linear network
//' 
//' @param fit a bn.fit object
//' @param order a topological ordering of the nodes
//' @return the numeric named vector of means
//' @export
// [[Rcpp::export]]
std::map<std::string, float> calc_mu(Rcpp::List fit, std::vector<std::string> order){
  std::map<std::string, float> mu;
  Rcpp::List node;
  std::vector<std::string> parents;
  std::vector<float> coefs;
  
  for(unsigned int i = 0; i < order.size(); i++){
    // Extract the relevant elements from the lists
    node = fit[order[i]];
    parents = node["parents"];
    coefs = node["coefficients"];
    
    mu[order[i]] = coefs[0];
    for(unsigned int j = 1; j < coefs.size(); j++){
      mu[order[i]] += coefs[j] * mu[parents[j-1]];
    }
  }
  
  return mu;
}
