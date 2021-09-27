#include "include/structure_learning_natPsoho.h"

// All the C++ code of the natPsoho algorithm has also been condensed into one
// source file and one header.


// ----------------------------------------------------------------------------
// CausalityList
// ----------------------------------------------------------------------------

// Insert an arc in the correspondent temporal family. 
// 
// @param cl a causality list
// @param ordering a list with the order of the variables in t_0
// @param node the node to insert
// @param i the causal unit in which to insert.
void insert_node_natcl(Rcpp::NumericVector &cl, const StringVector &ordering, std::string node, unsigned int i){
  Rcpp::StringVector tuple = find_name_and_index(node);
  std::string tmp;
  tmp = tuple[1];
  int idx = std::stoi(tmp);
  tmp = tuple[0];
  int ordering_idx = find_index(ordering, tmp);
  int arcs = cl[i * 3 + ordering_idx];
  
  idx = one_hot_cpp(idx);
  arcs = arcs | idx;
  cl[i * 3 + ordering_idx] = arcs;
}


// ----------------------------------------------------------------------------
// Position
// ----------------------------------------------------------------------------

//' Create a natural causal list from a DBN. This is the C++ backend of the function.
//' 
//' @param cl an initialized causality list
//' @param net a dbn object treated as a list of lists
//' @param ordering a vector with the names of the variables in order
//' @return the natCauslist equivalent to the DBN
// [[Rcpp::export]]
Rcpp::NumericVector create_natcauslist_cpp(Rcpp::NumericVector &cl, Rcpp::List &net, StringVector &ordering) {
  Rcpp::List aux;
  Rcpp::StringVector parents;
  std::string node;
  
  // Translation into natural causal list
  for(int i = 0; i < ordering.size(); i++){
    node = ordering[i];
    aux = net[node];
    parents = aux["parents"];
    
    for(int j = 0; j < parents.size(); j++){
      node = parents[j];
      insert_node_natcl(cl, ordering, node, i);
    }
  }
  
  return cl;
}

//' Create a matrix with the arcs defined in a causlist object
//' 
//' @param cl a causal list
//' @param ordering a list with the order of the variables in t_0
//' @param rows number of arcs in the network
//' @return a StringMatrix with the parent nodes and the children nodes
// [[Rcpp::export]]
Rcpp::CharacterMatrix natcl_to_arc_matrix_cpp(const Rcpp::NumericVector &cl, Rcpp::CharacterVector &ordering,
                                           unsigned int rows){
  Rcpp::StringMatrix res (rows, 2);
  int slice, j, k;
  k = 0;
  
  for(int i = 0; i < cl.size(); i++){
    slice = cl[i];
    j = 1;
    
    while(slice > 0){
      if(slice % 2 == 1){
        include_arc(res, ordering, i, j, k);
      }
      
      slice = slice >> 1;
      j++;
    }
  }
  
  return res;
}

//' Add a velocity to a position
//' 
//' @param cl the position's causal list
//' @param vl the velocity's positive causal list
//' @param vl_neg velocity's negative causal list
//' @param n_arcs number of arcs present in the position. Remainder: can't return integers by reference, they get casted to 1 sized vectors
//' @return the new position by reference and the new number of arcs by return
// [[Rcpp::export]]
int nat_pos_plus_vel_cpp(Rcpp::NumericVector &cl, const Rcpp::NumericVector &vl, const Rcpp::NumericVector &vl_neg, int n_arcs){
  int pos, new_pos, vl_i, vl_neg_i, n_prev, n_post;
  
  for(int i = 0; i < cl.size(); i++){
    pos = cl[i];
    vl_i = vl[i];
    vl_neg_i = vl_neg[i];
    new_pos = pos | vl_i;
    new_pos = bitwise_sub(new_pos, vl_neg_i);
    n_prev = bitcount(pos);
    n_post = bitcount(new_pos);
    
    cl[i] = new_pos;
    n_arcs += n_post - n_prev;
  }
  
  return n_arcs;
}


// ----------------------------------------------------------------------------
// Velocity
// ----------------------------------------------------------------------------

//' Substracts two natPositions to obtain the natVelocity that transforms ps1 into ps2
//' 
//' @param ps1 the first position's causal list
//' @param ps2 the second position's causal list
//' @param vl the natVelocity's positive causal list
//' @param vl_neg the natVelocity's negative causal list
//' @return the velocity's causal lists by reference and the number of operations by return
// [[Rcpp::export]]
int nat_pos_minus_pos_cpp(const Rcpp::NumericVector &ps1, const Rcpp::NumericVector &ps2, Rcpp::NumericVector &vl, Rcpp::NumericVector &vl_neg){
  int ps1_i, ps2_i, vl_i, vl_neg_i;
  int n_abs = 0;
  
  for(int i = 0; i < ps1.size(); i++){
    ps1_i = ps1[i];
    ps2_i = ps2[i];
    
    vl_i = bitwise_sub(ps2_i, ps1_i);
    vl_neg_i = bitwise_sub(ps1_i, ps2_i);
    
    vl[i] = vl_i;
    vl_neg[i] = vl_neg_i;
    n_abs += bitcount(vl_i) + bitcount(vl_neg_i);
  }
  
  return n_abs;
}

//' Adds two natVelocities 
//' 
//' Adds two natVelocities represented as two numeric vectors: one with the
//' positive part and one with the negative part. Adding them is a process that
//' does a bitwise 'or' with both the positive and negative parts of the two
//' velocities, adjusts the new abs_op, removes duplicated arcs in the final
//' velocity by using a bitwise 'xor' with both parts and adjusts the final abs_op.
//' The results are returned via modifying the original vl1 and vl1_neg by
//' reference and returning the final abs_op normally. I can't have an integer
//' edited by reference because it automatically gets casted and cannot be used
//' to return values. 
//' 
//' @param vl1 the first Velocity's positive part 
//' @param vl1_neg the first Velocity's negative part 
//' @param vl2 the second Velocity's positive part
//' @param vl2_neg the first Velocity's negative part 
//' @param abs_op1 the number of {1,-1} operations in the first velocity
//' @param abs_op2 the number of {1,-1} operations in the second velocity
//' @return the total number of resulting operations
// [[Rcpp::export]]
int nat_vel_plus_vel_cpp(Rcpp::NumericVector &vl1, Rcpp::NumericVector &vl1_neg,
                         const Rcpp::NumericVector &vl2, const Rcpp::NumericVector &vl2_neg, 
                         int abs_op1, int abs_op2){
  int pos1, pos2, neg1, neg2, mask, res;
  
  res = abs_op1 + abs_op2;
  for(int i = 0; i < vl1.size(); i++){
    pos1 = vl1[i];
    pos2 = vl2[i];
    neg1 = vl1_neg[i];
    neg2 = vl2_neg[i];
    
    add_nat_vel(pos1, pos2, res);
    add_nat_vel(neg1, neg2, res);
    mask = pos1 & neg1;
    
    if(mask){
      pos1 ^= mask;
      neg1 ^= mask;
      res -= 2 * bitcount(mask);
    }
    
    vl1[i] = pos1;
    vl1_neg[i] = neg1;
  }
  
  return res;
}

// Auxiliary function to make the main 'for' less verbose. It adds a natural
// number in two velocities by using an 'or' and removes duplicated operations 
// from abs_op.
void add_nat_vel(int &num1, int num2, int &abs_op){
  int mask = num1 & num2;
  if(mask)
    abs_op -= bitcount(mask);
  num1 |= num2;
}

//' Multiply a Velocity by a constant real number
//' 
//' @param k the constant real number
//' @param vl the Velocity's positive causal list
//' @param vl_neg the Velocity's negative causal list
//' @param abs_op the final number of {1,-1} operations
//' @param max_size the maximum size of the network
//' @return the new total number of operations 
// [[Rcpp::export]]
int nat_cte_times_vel_cpp(float k, Rcpp::NumericVector &vl, Rcpp::NumericVector &vl_neg, int abs_op, int max_size){
  int res, max_op, n_op, pos, pos_neg, pos_mix, pool_idx, pos_idx, bit_idx, bit_dest, max_int;
  bool remove;
  std::vector<int> pool;
  Rcpp::NumericVector pool_samp, bit_pool, bit_samp;
  
  max_int = one_hot_cpp(max_size) - 1;
  max_op = (max_size - 1) * vl.size();
  
  n_op = floor(k * abs_op);
  if(n_op > max_op)
    n_op = max_op;
  res = n_op;
  
  n_op = abs_op - n_op;
  remove = n_op > 0; // Whether to add or remove arcs
  n_op = std::abs(n_op);
  
  // Find a pool of possible integers in the cl and cl_neg to operate
  if(remove)
    pool = find_open_positions(vl, vl_neg, 0);
  else
    pool = find_open_positions(vl, vl_neg, max_int);
  
  for(int i = 0; i < n_op; i++){
    // Sample a position from the pool
    pool_samp = seq(0, pool.size() - 1);
    pool_samp = sample(pool_samp, 1, false);
    pool_idx = pool_samp[0];
    pos_idx = pool[pool_idx];
    
    // Find the pool of bits in that position
    pos = vl[pos_idx];
    pos_neg = vl_neg[pos_idx];
    pos_mix = pos | pos_neg;
    bit_pool = find_open_bits(pos_mix, remove, max_int);
    
    // Sample a bit and add it or remove it
    bit_samp = seq(0, bit_pool.size() - 1); // Sample the selected bit
    bit_samp = sample(bit_samp, 1, false);
    bit_idx = bit_samp[0];
    bit_idx = bit_pool[bit_idx];
    
    if(remove){
      if(pos & one_hot_cpp(bit_idx))
        pos ^= one_hot_cpp(bit_idx);
      else
        pos_neg ^= one_hot_cpp(bit_idx);
      pos_mix = pos | pos_neg;
      if(pos_mix == 0)
        pool.erase(pool.begin() + pool_idx);
    }
    
    else{
      bit_samp = seq(0, 1); // Sample whether to add the bit in the positive or negative cl
      bit_samp = sample(bit_samp, 1, false);
      bit_dest = bit_samp[0];
      if(bit_dest)
        pos_neg |= one_hot_cpp(bit_idx);
      else
        pos |= one_hot_cpp(bit_idx);
      pos_mix = pos | pos_neg;
      if(pos_mix == max_int)
        pool.erase(pool.begin() + pool_idx);
    }
    
    // Save the modification of the velocity
    vl[pos_idx] = pos;
    vl_neg[pos_idx] = pos_neg;
  }
  
  return res;
}


// ----------------------------------------------------------------------------
// Utils
// ----------------------------------------------------------------------------

//' One-hot encoder for natural numbers without the 0
//' 
//' Given a natural number, return the natural number equivalent to its
//' one-hot encoding. Instead of pow, the '<<' operator will be used.
//' Examples: 3 -> 100 -> 4, 5 -> 10000 -> 16
//' @param nat the natural number to convert
//' @return the converted number
// [[Rcpp::export]]
int one_hot_cpp(int nat){
  return(1 << (nat - 1));
}

// Bitcount implementation from the book 'Hacker's Delight'
// Basically, a divide and conquer algorithm that sums the number of bits
// in two halves. It is not done recursively because the size of integers is
// fixed to 2^5, and so only 5 "mask and add" steps are needed.
// A less efficient but more readable version would be:
//
// int bitcount(unsigned int x)
// {
//   x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
//   x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
//   x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
//   x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
//   x = (x & 0x0000FFFF) + ((x >> 16)& 0x0000FFFF);
//   return x;
// }
// [[Rcpp::export]]
int bitcount(unsigned x){
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F;
  x = x + (x >> 8);
  x = x + (x >> 16);
  return x & 0x0000003F;
}

// Return the name of the node and its time slice
// 
// @param node a string with the name of the node
// @return a list with the name of the node and an integer with the time slice that the node belongs to
Rcpp::StringVector find_name_and_index(std::string node){
  Rcpp::StringVector res (2);
  std::string delim = "_t_";
  size_t pos;
  
  pos = node.find(delim);
  res[0] = node.substr(0, pos);
  res[1] = node.substr(pos + delim.length());
  
  return res;
}

void include_arc(Rcpp::StringMatrix &res, const Rcpp::StringVector &ordering, int i, int j, int &k){
  std::string from, to;
  int from_idx, to_idx;
  
  to_idx = i / ordering.size();
  to = ordering[to_idx];
  to += "_t_0";
  from_idx = i % ordering.size();
  from = ordering[from_idx];
  from += "_t_" + std::to_string(j);
  
  res(k, 0) = from;
  res(k, 1) = to;
  k++;
}

// Find the position of the node in the ordering. The node should be findable
// in the ordering. If not, an out of bounds index is returned
// 
// @param ordering a list with the order of the variables in t_0
// @param node the desired node
// @return the position of the node in the ordering
int find_index(const Rcpp::StringVector &ordering, std::string node){
  int i = 0;
  bool found = false;
  std::string name;
  
  while(i < ordering.size() && !found){
    name = ordering[i];
    if(name.find(node) != std::string::npos)
      found = true;
    else
      i++;
  }
  
  return i;
}

// Find the positions that are available to operate in a velocity
// 
// When adding or removing arcs via cte * vel, this finds positions where
// bits can be added or removed
//
// @param cl the positive causal list
// @param cl_neg the negative causal list
// @param max_int if 0 will search for integers greater than 0. Will search for < max_int otherwise.
// @return a vector with the open positions
std::vector<int> find_open_positions(const Rcpp::NumericVector &cl, const Rcpp::NumericVector &cl_neg, int max_int){
  std::vector<int> res(cl.size());
  bool add = max_int;
  int pos, pos_neg, j = 0;
  
  for(int i = 0; i < cl.size(); i++){
    pos = cl[i];
    pos_neg = cl_neg[i];
    pos |= pos_neg; // Remainder: arcs are shared between the positive and negative cl, so the number is obtained as a combination of both
    
    if((add && pos < max_int) || (!add && pos > 0)){
      res[j] = i;
      j++;
    }
  }
  
  res.resize(j);
  
  return res;
}

// Find the bits that are set to 0 or 1 in an integer
// 
// This can also be done recursively by masking, but in most cases the size
// of the network shouldn't exceed size 8 or so, which means only 8 iterations
// at worst. By using divide and conquer, I could get there in O(log(n)). The
// implementation I did is slower than this, probably because of appending
// the vectors of results in the recursion and because it ends up doing more
// operations by masking and shifting.
//
// @param x the integer to process
// @param remove if true, will search for bits set to 1. Will search for 0s otherwise.
// @param max_int the maximum integer allowed, i.e., the one with all possible bits set to 1
// @return a NumericVector with the open bits
Rcpp::NumericVector find_open_bits(int x, bool remove, int max_int){
  if(!remove)
    x ^= max_int;
  Rcpp::NumericVector res(bitcount(x));
  int i = 0, pos = 1;
  
  while(x != 0){
    if(x % 2){
      res[i] = pos;
      i++;
    }
    x >>= 1;
    pos++;
  }
  
  return res;
}

// Binary bitwise operator that returns 1 only when the first bit is 1 and the
// second one is 0. Kinda like subtracting 1 bit only when there is a 1 in the 
// other bit. It helps removing 1s in the position when a 1 is in that same bit
// in the velocity, or finding the velocity that takes you from a position to another.
// The truth table is:
//        x2
//       0 1
//  x1 0 0 0
//     1 1 0
int bitwise_sub(int x1, int x2){
  return x1 & (~x2);
}

//' Initialize the nodes vector
//' 
//' Initialize the vector in C++
//' @param n_nodes number of receiving nodes
//' @return a list with the randomly initialized particles
// [[Rcpp::export]]
Rcpp::NumericVector init_cl_cpp(int n_nodes){
  Rcpp::NumericVector res (n_nodes);
  
  return res;
}

//' If the names of the nodes have "_t_0" appended at the end, remove it
//' @param names a vector with the names of the nodes in t_0
//' @return the vector with the names cropped
// [[Rcpp::export]]
Rcpp::StringVector crop_names_cpp(const Rcpp::StringVector &names){
  StringVector res = Rcpp::clone(names);
  std::string tmp;
  std::size_t pos;
  
  for(int i = 0; i < res.size(); i++){
    tmp = res[i];
    pos = tmp.find("_");
    tmp = tmp.substr(0, pos);
    res[i] = tmp;
  }
  
  return(res);
}