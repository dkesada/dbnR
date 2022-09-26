#include "include/gauss_transform.h"

//' Calculate the mu vector of means of a Gaussian linear network. This is 
//' the C++ backend of the function.
//' 
//' @param fit a bn.fit object as a Rcpp::List
//' @param order a topological ordering of the nodes as a vector of strings
//' @return the map with the nodes and their mu. Returns as a named numeric vector
//' @keywords internal
// [[Rcpp::export]]
std::map<std::string, float> calc_mu_cpp(Rcpp::List &fit, Rcpp::StringVector &order){
  std::map<std::string, float> mu;
  Rcpp::List node;
  Rcpp::StringVector parents;
  Rcpp::NumericVector coefs;
  std::string node_name;
  std::string parent_name;
  
  for(unsigned int i = 0; i < order.size(); i++){
    // Extract the relevant elements from the lists
    node_name = order[i];
    node = fit[node_name]; 
    parents = node["parents"];
    coefs = node["coefficients"];

    mu[node_name] = coefs[0];
    for(unsigned int j = 1; j < coefs.size(); j++){
      parent_name = parents[j-1];
      mu[node_name] += coefs[j] * mu[parent_name];
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
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix calc_sigma_cpp(Rcpp::List &fit, Rcpp::StringVector &order){
  Rcpp::NumericMatrix sigma(order.size(), order.size());
  Rcpp::List node;
  Rcpp::StringVector parents;
  Rcpp::NumericVector coefs;
  float sd;
  std::map<std::string, float> idx;
  std::string node_name;
  std::string parent_name;
  
  for(unsigned int i = 0; i < order.size(); i++){
    node_name = order[i];
    idx[node_name] = i;
  }

  // Calculate variances diagonal
  for(unsigned int i = 0; i < order.size(); i++){
    // Extract the relevant elements from the lists
    node_name = order[i];
    node = fit[node_name];
    sd = node["sd"];
    parents = node["parents"];
    coefs = node["coefficients"];

    sigma(i,i) = sd * sd;
    for(unsigned int j = 1; j < coefs.size(); j++){
      parent_name = parents[j-1];
      sigma(i,i) += coefs[j] * sigma(idx[parent_name], idx[parent_name]) * coefs[j];
    }
  }
  
  // Calculate covariances
  
  for(unsigned int i = 0; i < order.size(); i++){
    for(unsigned int j = i + 1; j < order.size(); j++){
      // Extract the relevant elements from the lists
      node_name = order[j];
      node = fit[node_name];
      coefs = node["coefficients"];
      parents =  node["parents"];
      
      for(unsigned int k = 1; k < coefs.size(); k++){
        parent_name = parents[k-1];
        sigma(i,j) += coefs[k] * sigma(i,idx[parent_name]);  
      }
      
      sigma(j,i) = sigma(i,j);
    }
  }
  
  return sigma;
}





