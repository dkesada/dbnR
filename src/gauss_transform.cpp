#include <Rcpp.h>
using namespace Rcpp;
#include "include/gauss_transform.h"

//' Calculate the mu vector of means of a Gaussian linear network. This is 
//' the C++ backend of the function.
//' 
//' @param fit a bn.fit object as a Rcpp::List
//' @param order a topological ordering of the nodes as a vector of strings
//' @return the map with the nodes and their mu. Returns as a named numeric vector
// [[Rcpp::export]]
std::map<std::string, float> calc_mu_cpp(Rcpp::List &fit, std::vector<std::string> &order){
  std::map<std::string, float> mu;
  Rcpp::List node;
  std::vector<std::string> parents;
  std::vector<float> coefs;
  
  for(unsigned int i = 0; i < order.size(); i++){
    // Extract the relevant elements from the lists
    node = fit[order[i]]; 
    parents = as<std::vector<std::string> >(node["parents"]);
    coefs = as<std::vector<float> >(node["coefficients"]);

    mu[order[i]] = coefs[0];
    for(unsigned int j = 1; j < coefs.size(); j++){
      mu[order[i]] += coefs[j] * mu[parents[j-1]];
    }
  }

  return mu;
}

//' Calculate the sigma covariance matrix of a Gaussian linear network. 
//' This is the C++ backend of the function.
//' 
//' @param fit a bn.fit object as a Rcpp::List
//' @param order a topological ordering of the nodes as a vector of strings
//' @return the covariance matrix
// [[Rcpp::export]]
Rcpp::NumericMatrix calc_sigma_cpp(Rcpp::List &fit, std::vector<std::string> &order){
  Rcpp::NumericMatrix sigma(order.size(), order.size());
  Rcpp::List node;
  std::vector<std::string> parents;
  std::vector<float> coefs;
  float sd;
  std::map<std::string, float> idx;
  
  for(unsigned int i = 0; i < order.size(); i++){
    idx[order[i]] = i;
  }

  // Calculate variances diagonal
  for(unsigned int i = 0; i < order.size(); i++){
    // Extract the relevant elements from the lists
    node = fit[order[i]];
    sd = node["sd"];
    parents =  as<std::vector<std::string> >(node["parents"]);
    coefs = as<std::vector<float> >(node["coefficients"]);

    sigma(i,i) = sd * sd;
    for(unsigned int j = 1; j < coefs.size(); j++){
      sigma(i,i) += coefs[j] * sigma(idx[parents[j-1]], idx[parents[j-1]]) * coefs[j];
    }
  }
  
  // Calculate covariances
  
  for(unsigned int i = 0; i < order.size(); i++){
    for(unsigned int j = i + 1; j < order.size(); j++){
      // Extract the relevant elements from the lists
      node = fit[order[j]];
      coefs = as<std::vector<float> >(node["coefficients"]);
      parents =  as<std::vector<std::string> >(node["parents"]);
      
      for(unsigned int k = 1; k < coefs.size(); k++){
        sigma(i,j) += coefs[k] * sigma(i,idx[parents[k-1]]);  
      }
      
      sigma(j,i) = sigma(i,j);
    }
  }
  
  return sigma;
}





