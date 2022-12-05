#include "include/structure_learning_psoho.h"

// All the C++ code of the psoho algorithm has also been condensed into one
// source file and one header.


// ----------------------------------------------------------------------------
// CausalityList
// ----------------------------------------------------------------------------

//' Create a causality list and initialize it
//' 
//' @param ordering a list with the order of the variables in t_0
//' @param size the size of the DBN
//' @return a causality list
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List initialize_cl_cpp(StringVector &ordering, unsigned int size) {
  Rcpp::List res (size - 1);
  Rcpp::StringVector new_names;
  
  // Initialization of the causality list
  for(unsigned int i = 0; i < size - 1; i++){
    Rcpp::List vel_list(ordering.size());
    new_names = rename_slices(ordering, i + 1);
    for(unsigned int j = 0; j < ordering.size(); j++){
      Rcpp::List pair (2);
      Rcpp::NumericVector velocity (ordering.size());
      
      pair[0] = new_names;
      pair[1] = velocity;
      vel_list[j] = pair;
    }
    res[i] = vel_list;
  }
  
  return res;
}

// Insert a node in the correspondent causal unit. Keeps record of inserted
// elements in each causal unit
// 
// @param cl a causality list
// @param node the node to insert
// @param i the causal unit in which to insert.
void insert_node_cl(Rcpp::List &cl, std::string node, unsigned int i){
  int idx = find_index(node);
  Rcpp::List slice = cl[idx-1];
  Rcpp::List cu = slice[i];
  int pos = 0;
  Rcpp::StringVector names = cu[0];
  std::string str;
  Rcpp::NumericVector arcs = cu[1];
  
  str = names[0];
  while(node.compare(str) != 0 && pos < names.size()){
    pos++;
    str = names[pos];
  }
  
  arcs[pos] = 1;
}


// ----------------------------------------------------------------------------
// Position
// ----------------------------------------------------------------------------

//' Create a causal list from a DBN. This is the C++ backend of the function.
//' 
//' @param cl an initialized causality list
//' @param net a dbn object treated as a list of lists
//' @param size the size of the DBN
//' @param ordering a list with the order of the variables in t_0
//' @return a list with a CharacterVector and a NumericVector
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List create_causlist_cpp(Rcpp::List &cl, Rcpp::List &net, unsigned int size, StringVector &ordering) {
  Rcpp::List aux;
  Rcpp::StringVector caus_unit, parents;
  std::string node;
  
  // Translation into causal list
  for(unsigned int i = 0; i < ordering.size(); i++){
    node = ordering[i];
    aux = net[node];
    parents = aux["parents"];
    
    for(unsigned int j = 0; j < parents.size(); j++){
      node = parents[j];
      insert_node_cl(cl, node, i);
    }
  }
  
  return cl;
}

//' Create a matrix with the arcs defined in a causlist object
//' 
//' @param cl a causal list
//' @param ordering a list with the order of the variables in t_0
//' @param rows number of arcs in the network
//' @return a list with a CharacterVector and a NumericVector
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterMatrix cl_to_arc_matrix_cpp(Rcpp::List &cl, Rcpp::CharacterVector &ordering,
                                           unsigned int rows){
  Rcpp::StringMatrix res (rows, 2);
  unsigned int res_row = 0;
  Rcpp::List slice, cu;
  Rcpp::StringVector nodes;
  Rcpp::NumericVector arcs;
  
  for(unsigned int i = 0; i < cl.size(); i++){
    slice = cl[i];
    for(unsigned int j = 0; j < ordering.size(); j++){
      cu = slice[j];
      nodes = cu[0];
      arcs = cu[1];
      for(unsigned int k = 0; k < nodes.size(); k++){
        if(arcs[k] == 1){
          res(res_row, 0) = nodes[k];
          res(res_row, 1) = ordering[j];
          res_row += 1;
        }
      }
    }
  }
  
  return res;
}

//' Add a velocity to a position
//' 
//' @param cl the position's causal list
//' @param vl the velocity's causal list
//' @param n_arcs number of arcs present in the position
//' @return a list with the modified position and the new number of arcs
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List pos_plus_vel_cpp(Rcpp::List &cl, Rcpp::List &vl, int n_arcs){
  Rcpp::List slice_cl, slice_vl, cu_cl, cu_vl, pair_cl, pair_vl;
  Rcpp::NumericVector dirs_cl, dirs_vl;
  Rcpp::List res (2);
  
  for(unsigned int i = 0; i < cl.size(); i++){
    slice_cl = cl[i];
    slice_vl = vl[i];
    
    for(unsigned int j = 0; j < slice_cl.size(); j++){
      pair_cl = slice_cl[j];
      pair_vl = slice_vl[j];
      dirs_cl = pair_cl[1];
      dirs_vl = pair_vl[1];
      dirs_cl = add_dirs_vec(dirs_cl, dirs_vl, n_arcs);
      
      pair_cl[1] = dirs_cl;
      slice_cl[j] = pair_cl;
    }
    
    cl[i] = slice_cl;
  }
  
  res[0] = cl;
  res[1] = n_arcs;
  
  return res;
}

//' Initialize the particles
//' 
//' @param nodes the names of the nodes
//' @param size the size of the DBN
//' @param n_inds the number of particles
//' @return a list with the randomly initialized particles
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List init_list_cpp(Rcpp::StringVector nodes, unsigned int size, unsigned int n_inds){
  Rcpp::List res (n_inds);
  Environment psoho("package:psoho");
  Environment env = psoho["Position"];
  Function new_ps = env["new"];
  
  for(unsigned int i = 0; i < n_inds; i++){
    Environment ps;
    ps = new_ps(NULL, size, nodes);
    res[i] = ps;
  }
  
  return res;
}


// ----------------------------------------------------------------------------
// Velocity
// ----------------------------------------------------------------------------

//' Randomize a velocity with the given probabilities
//' 
//' @param vl a velocity list
//' @param probs the probabilities of each value in the set {-1,0,1}
//' @return a velocity list with randomized values
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List randomize_vl_cpp(Rcpp::List &vl, NumericVector &probs) {
  Rcpp::List slice, velocity, directions, cu, pair;
  unsigned int abs_op = 0, dir_tmp;
  Rcpp::List res (2);
  
  // Initialization of the velocity
  for(unsigned int i = 0; i < vl.size(); i++){
    slice = vl[i];
    for(unsigned int j = 0; j < slice.size(); j++){
      pair = slice[j];
      directions = random_directions(probs, slice.size());
      pair[1] = directions[0];
      dir_tmp = directions[1]; // Error on some systems with abs_op += directions[1];
      abs_op += dir_tmp;
    }
  }
  
  res[0] = vl;
  res[1] = abs_op;
  
  return res;
}

//' Subtracts two Positions to obtain the Velocity that transforms one into the other
//' 
//' @param cl the first position's causal list
//' @param ps the second position's causal list
//' @param vl the Velocity's causal list
//' @return a list with the Velocity's causal list and the number of operations
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List pos_minus_pos_cpp(Rcpp::List &cl, Rcpp::List &ps, Rcpp::List &vl){
  Rcpp::List slice_cl, slice_ps, slice_vl, cu_cl, cu_ps, cu_vl, pair_cl, pair_ps, pair_vl;
  Rcpp::NumericVector dirs_cl, dirs_ps, dirs_vl;
  int n_abs = 0;
  Rcpp::List res (2);
  
  for(unsigned int i = 0; i < cl.size(); i++){
    slice_cl = cl[i];
    slice_ps = ps[i];
    slice_vl = vl[i];
    
    for(unsigned int j = 0; j < slice_cl.size(); j++){
      pair_cl = slice_cl[j];
      pair_ps = slice_ps[j];
      pair_vl = slice_vl[j];
      dirs_cl = pair_cl[1];
      dirs_ps = pair_ps[1];
      dirs_vl = subtract_dirs_vec(dirs_cl, dirs_ps, n_abs);
      
      pair_vl[1] = dirs_vl;
      slice_vl[j] = pair_vl;
    }
    
    vl[i] = slice_vl;
  }
  
  res[0] = vl;
  res[1] = n_abs;
  
  return res;
}

//' Add two Velocities 
//' 
//' @param vl1 the first Velocity's causal list
//' @param vl2 the second Velocity's causal list
//' @param abs_op the final number of {1,-1} operations
//' @return a list with the Velocity's causal list and the number of operations
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List vel_plus_vel_cpp(Rcpp::List &vl1, Rcpp::List &vl2, int abs_op){
  Rcpp::List slice_vl1, slice_vl2, cu_vl1, cu_vl2, pair_vl1, pair_vl2;
  Rcpp::NumericVector dirs_vl1, dirs_vl2;
  Rcpp::List res (2);
  
  for(unsigned int i = 0; i < vl1.size(); i++){
    slice_vl1 = vl1[i];
    slice_vl2 = vl2[i];
    
    for(unsigned int j = 0; j < slice_vl1.size(); j++){
      pair_vl1 = slice_vl1[j];
      pair_vl2 = slice_vl2[j];
      dirs_vl1 = pair_vl1[1];
      dirs_vl2 = pair_vl2[1];
      dirs_vl1 = add_vel_dirs_vec(dirs_vl1, dirs_vl2, abs_op);
      
      pair_vl1[1] = dirs_vl1;
      slice_vl1[j] = pair_vl1;
    }
    
    vl1[i] = slice_vl1;
  }
  
  res[0] = vl1;
  res[1] = abs_op;
  
  return res;
}

//' Multiply a Velocity by a constant real number
//' 
//' @param k the constant real number
//' @param vl the Velocity's causal list
//' @param abs_op the final number of {1,-1} operations
//' @param max_op the maximum number of directions in the causal list
//' @return a list with the Velocity's new causal list and number of operations
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List cte_times_vel_cpp(float k, Rcpp::List &vl, unsigned int abs_op, int max_op){
  Rcpp::List res (2);
  int n_op, idx, cmp;
  Rcpp::List pool, n_pool;
  int l_pool = abs_op;
  Rcpp::NumericVector pos;
  bool invert = false;
  NumericVector tmp;
  
  // Process the k and max_op
  if(k < 0){
    k = fabs(k);
    invert = true;
  }
  
  n_op = floor(k * abs_op);
  res[1] = n_op;
  if(n_op < -max_op){
    n_op = -max_op;
    res[1] = max_op;
  }
  
  else if(n_op > max_op){
    n_op = max_op;
    res[1] = max_op;
  }
  
  n_op = abs_op - n_op;
  
  if(n_op < 0){ // Convert {0} into {1,-1}
    l_pool = max_op - abs_op; // Number of 0's remaining
    n_op = std::abs(n_op);
    pool = Rcpp::List(l_pool);
    cmp = 0;
  } 
  
  else{ // Convert {1,-1} into {0}
    n_op = std::abs(n_op);
    pool = Rcpp::List(l_pool);
    cmp = 1;
  }
  
  if(n_op > 0){
    // Loop through the cl to store the position and sign invert the 0's or the 1's depending on k greater or lesser than 0
    locate_directions(vl, pool, cmp, invert);
    
    // Sample the position vector to position 0's or 1's in some or all of those positions
    pos = seq(0, (pool.size() - 1));
    pos = sample(pos, n_op, false);
    n_pool = Rcpp::List(n_op);
    for(unsigned int i = 0; i < pos.size(); i++){
      idx = pos[i];
      tmp = pool[idx];
      n_pool[i] = tmp;
    }
    
    // Operate the selected directions
    modify_directions(vl, n_pool, cmp);
  }
  
  res[0] = vl;
  
  return res;
}


// ----------------------------------------------------------------------------
// Utils
// ----------------------------------------------------------------------------

//' Return a list of nodes with the time slice appended up to the desired size
//' of the network
//' 
//' @param nodes a list with the names of the nodes in the network
//' @param size the size of the DBN
//' @return a list with the renamed nodes in each timeslice
//' @keywords internal
// [[Rcpp::export]]
Rcpp::StringVector rename_nodes_cpp(const Rcpp::StringVector &nodes, unsigned int size){
  Rcpp::StringVector res (nodes.size() * size);
  std::string new_name;
  
  for(unsigned int i = 0; i < size; i++){
    for(unsigned int j = 0; j < nodes.size(); j++){
      new_name = nodes[j];
      res[i*nodes.size()+j] = new_name + "_t_" + std::to_string(size-1-i); // Random network generation works better with t_0 at the end 
    }
  }
  
  return res;
}

// Return the time slice of a node
// 
// @param node a string with the name of the node
// @return an integer with the time slice that the node belongs to
int find_index(std::string node){
  int res;
  std::string str_slice;
  
  StringVector crop_number = R_SUB("^.+_t_", "", node);
  str_slice = crop_number[0];
  res = std::stoi(str_slice);
  
  return res;
}

// Modify the names of the nodes to the desired timeslice
// 
// @param nodes a string vector with the names of the nodes
// @param slice the new slice of the nodes
// @return an integer with the time slice that the node belongs to
Rcpp::StringVector rename_slices(const Rcpp::StringVector &nodes, unsigned int slice){
  std::string new_name;
  Rcpp::StringVector res (nodes.size()); 
  Rcpp::StringVector crop_name;
  
  for(unsigned int i = 0; i < nodes.size(); i++){
    new_name = nodes[i];
    crop_name = R_SUB("_[0-9]+$", "_", new_name);  // Using R regex motor instead of C++11 regex due to unstable behaviour
    new_name = crop_name[0];
    new_name = new_name + std::to_string(slice);
    res[i] = new_name;
  }
  
  return res;
}

// Generate a random vector of n {-1,0,1} directions
// 
// @param probs the weights of each value in the random generation
// @param size the number of random directions to generate
// @return a NumericVector with the random directions
Rcpp::List random_directions(const Rcpp::NumericVector &probs, unsigned int size){
  Rcpp::NumericVector res_n (size);
  NumericVector base = {-1,0,1};
  unsigned int abs_op = 0;
  Rcpp::List res (2);
  
  for(unsigned int i = 0; i < size; i++){
    NumericVector dir = sample(base, 1, true, probs);
    int dir_val = dir[0];
    res_n[i] = dir_val;
    abs_op += std::abs(dir_val);
  }
  
  res[0] = res_n;
  res[1] = abs_op;
  
  return res;
}

// Add two directions whose value has to be in the set {-1,0,1}
// 
// @param d1 first direction
// @param d2 second direction
// @param n_arcs the number of arcs present in the resulting causal list
// @return the result of adding them
int add_dirs(int d1, int d2, int &n_arcs){
  int res = d1 + d2;
  
  if(res < 0)
    res = 0;
  else if(res > 1)
    res = 1;
  
  if(res > d1)
    n_arcs++;
  else if(res < d1)
    n_arcs--;
  
  return res;
}

// Add two directions vectors whose value has to be in the set {-1,0,1}
// 
// @param d1 first NumericVector direction
// @param d2 second NumericVector direction
// @param n_arcs the number of arcs present in the resulting causal list
// @return the result of adding them
Rcpp::NumericVector add_dirs_vec(const NumericVector &d1, const NumericVector &d2, int &n_arcs){
  Rcpp::NumericVector res (d1.size());
  
  for(unsigned int i = 0; i < d1.size(); i++){
    res[i] = add_dirs(d1[i], d2[i], n_arcs);
  }
  
  return res;
}

// Subtract two directions whose value has to be in the set {-1,0,1}
// 
// @param d1 first direction
// @param d2 second direction
// @param n_arcs the number of arcs operations present in the resulting Velocity
// @return the result of subtracting them
int subtract_dirs(int d1, int d2, int &n_abs){
  int res = d1 - d2;
  
  if(d1 != d2)
    n_abs++;
  
  return res;
}

// Subtract two directions vectors whose value has to be in the set {-1,0,1}
// 
// @param d1 first NumericVector direction
// @param d2 second NumericVector direction
// @param n_arcs the number of arcs operations present in the resulting Velocity
// @return the result of subtracting them
Rcpp::NumericVector subtract_dirs_vec(const NumericVector &d1, const NumericVector &d2, int &n_abs){
  Rcpp::NumericVector res (d1.size());
  
  for(unsigned int i = 0; i < d1.size(); i++){
    res[i] = subtract_dirs(d1[i], d2[i], n_abs);
  }
  
  return res;
}

// Add two velocity directions whose value has to be in the set {-1,0,1}
// 
// @param d1 first direction
// @param d2 second direction
// @param abs_op the number of {1,-1} operations present in the resulting Velocity
// @return the result of adding them
int add_vel_dirs(int d1, int d2, int &abs_op){
  int res = d1 + d2;
  
  if(res < -1)
    res = -1;
  else if(res > 1)
    res = 1;
  
  if(res > d1 && res == 1)
    abs_op++;
  else if(res > d1 && res == 0)
    abs_op--;
  else if(res < d1 && res == 0)
    abs_op--;
  else if(res < d1 && res == -1)
    abs_op++;
  
  return res;
}

// Subtract two directions vectors whose value has to be in the set {-1,0,1}
// 
// @param d1 first NumericVector direction
// @param d2 second NumericVector direction
// @param abs_op the number of {1,-1} operations present in the resulting Velocity
// @return the result of adding them
Rcpp::NumericVector add_vel_dirs_vec(const NumericVector &d1, const NumericVector &d2, int &abs_op){
  Rcpp::NumericVector res (d1.size());
  
  for(unsigned int i = 0; i < d1.size(); i++){
    res[i] = add_vel_dirs(d1[i], d2[i], abs_op);
  }
  
  return res;
}

// Find the position of 0's or 1's in a Velocity's causality list
// 
// @param vl the Velocity's causality list
// @param pool the list with the positions
// @param cmp the direction to be searched, either 0 or 1 
// @return a list with the Velocity's new causal list and number of operations
void locate_directions(Rcpp::List &vl, Rcpp::List &pool, int cmp, bool invert){
  Rcpp::List slice, cu, pair;
  Rcpp::NumericVector dirs;
  unsigned int pool_i = 0;
  
  for(unsigned int i = 0; i < vl.size(); i++){
    slice = vl[i];
    
    for(unsigned int j = 0; j < slice.size(); j++){
      pair = slice[j];
      dirs = pair[1];
      
      for(unsigned int k = 0; k < dirs.size(); k++){
        if(invert)
          dirs[k] = -dirs[k];
        if(std::abs(dirs[k]) == cmp){
          Rcpp::NumericVector pool_res (3);
          pool_res[0] = i;
          pool_res[1] = j;
          pool_res[2] = k;
          pool[pool_i++] = pool_res;
        }
      }
    }
  }
}

// Modify the 0's or 1's in the given positons of a Velocity's causality list
// 
// @param vl the Velocity's causality list
// @param n_pool the list with the positions
// @param cmp the direction to be searched, either 0 or 1 
// @return a list with the Velocity's new causal list and number of operations
void modify_directions(Rcpp::List &vl, Rcpp::List &n_pool, int cmp){
  Rcpp::List slice, pair;
  Rcpp::NumericVector tuple, dirs;
  unsigned int idx;
  NumericVector base = {-1,1};
  NumericVector rand (1);
  
  for(unsigned int i = 0; i < n_pool.size(); i++){
    tuple = n_pool[i];
    idx = tuple[0];
    slice = vl[idx];
    idx = tuple[1];
    pair = slice[idx];
    dirs = pair[1];
    idx = tuple[2];
    
    if(cmp == 0){
      rand = sample(base, 1, false);
      dirs(idx) = rand[0];
    }
    
    else
      dirs(idx) = 0.0;
  }
}