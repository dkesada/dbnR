#ifndef Rcpp_head
#define Rcpp_head
#include <Rcpp.h>
using namespace Rcpp;
#endif

#include <regex>
#include <random>

#ifndef psoho_op
#define psoho_op
Rcpp::List initialize_cl_cpp(StringVector &ordering, unsigned int size);
void insert_node_cl(Rcpp::List &cl, std::string node, unsigned int i);
Rcpp::List create_causlist_cpp(Rcpp::List &cl, Rcpp::List &net, unsigned int size, StringVector &ordering);
Rcpp::CharacterMatrix cl_to_arc_matrix_cpp(Rcpp::List &cl, Rcpp::CharacterVector &ordering, unsigned int rows);
Rcpp::List pos_plus_vel_cpp(Rcpp::List &cl, Rcpp::List &vl, int n_arcs);
Rcpp::List init_list_cpp(Rcpp::StringVector nodes, unsigned int size, unsigned int n_inds);
Rcpp::List randomize_vl_cpp(Rcpp::List &vl, NumericVector &probs, int seed);
Rcpp::List pos_minus_pos_cpp(Rcpp::List &cl, Rcpp::List &ps, Rcpp::List &vl);
Rcpp::List vel_plus_vel_cpp(Rcpp::List &vl1, Rcpp::List &vl2, int abs_op);
Rcpp::List cte_times_vel_cpp(float k, Rcpp::List vl, unsigned int abs_op, unsigned int max_op);
Rcpp::StringVector rename_nodes_cpp(Rcpp::StringVector &nodes, unsigned int size);
int find_index(std::string node);
Rcpp::StringVector rename_slices(const Rcpp::StringVector &nodes, unsigned int slice);
Rcpp::List random_directions(const Rcpp::NumericVector &probs, unsigned int size);
int add_dirs(int d1, int d2, int &n_arcs);
Rcpp::NumericVector add_dirs_vec(const NumericVector &d1, const NumericVector &d2, int &n_arcs);
int subtract_dirs(int d1, int d2, int &n_abs);
Rcpp::NumericVector subtract_dirs_vec(const NumericVector &d1, const NumericVector &d2, int &n_abs);
int add_vel_dirs(int d1, int d2, int &abs_op);
Rcpp::NumericVector add_vel_dirs_vec(const NumericVector &d1, const NumericVector &d2, int &abs_op);
void locate_directions(Rcpp::List &vl, Rcpp::List &pool, int cmp, bool invert);
void modify_directions(Rcpp::List &vl, Rcpp::List &n_pool, int cmp);
#endif