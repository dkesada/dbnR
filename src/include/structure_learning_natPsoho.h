#ifndef Rcpp_head
#define Rcpp_head
#include <Rcpp.h>
using namespace Rcpp;
#endif


#ifndef natPsoho_op
#define natPsoho_op

#include <regex>
#include <random>
#include <vector>
#include <string>

void insert_node_natcl(Rcpp::NumericVector &cl, const StringVector &ordering, std::string node, unsigned int i)
Rcpp::NumericVector create_natcauslist_cpp(Rcpp::NumericVector &cl, Rcpp::List &net, StringVector &ordering);
Rcpp::CharacterMatrix cl_to_arc_matrix_cpp(const Rcpp::NumericVector &cl, Rcpp::CharacterVector &ordering, unsigned int rows);
int nat_pos_plus_vel_cpp(Rcpp::NumericVector &cl, const Rcpp::NumericVector &vl, const Rcpp::NumericVector &vl_neg, int n_arcs);
Rcpp::List randomize_vl_cpp(Rcpp::List &vl, NumericVector &probs, int seed);
int nat_pos_minus_pos_cpp(const Rcpp::NumericVector &ps1, const Rcpp::NumericVector &ps2, 
                          Rcpp::NumericVector &vl, Rcpp::NumericVector &vl_neg);
int nat_vel_plus_vel_cpp(Rcpp::NumericVector &vl1, Rcpp::NumericVector &vl1_neg, 
                         const Rcpp::NumericVector &vl2, const Rcpp::NumericVector &vl2_neg, 
                         int abs_op1, int abs_op2);
void add_nat_vel(int &num1, int num2, int &abs_op);
int nat_cte_times_vel_cpp(float k, Rcpp::NumericVector &vl, Rcpp::NumericVector &vl_neg, int abs_op, int max_size);
int one_hot_cpp(int nat);
int bitcount(unsigned x);
Rcpp::StringVector find_name_and_index(std::string node);
void include_arc(Rcpp::StringMatrix &res, const Rcpp::StringVector &ordering, int i, int j, int &k);
int find_index(const Rcpp::StringVector &ordering, std::string node);
std::vector<int> find_open_positions(const Rcpp::NumericVector &cl, const Rcpp::NumericVector &cl_neg, int max_int);
Rcpp::NumericVector find_open_bits(int x, bool remove, int max_int);
int bitwise_sub(int x1, int x2);
Rcpp::NumericVector init_cl_cpp(int n_nodes);
Rcpp::StringVector crop_names_cpp(Rcpp::StringVector names);

#endif