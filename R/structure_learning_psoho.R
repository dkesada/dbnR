#' This file contains all the classes needed for the PSOHO structure learning 
#' algorithm. It was implemented as an independent package in
#' https://github.com/dkesada/PSOHO and then merged into dbnR. All the original
#' source files are merged into one to avoid bloating the R/ folder of the
#' package.
#' 
#' The classes are now not exported because the whole algorithm is 
#' encapsulated inside the package and only the resulting dbn structure is
#' wanted. As a result, many security checks have been omitted.

# -----------------------------------------------------------------------------

#' R6 class that defines causal lists in the PSO
#' 
#' The causal lists will be the base of the positions and the velocities
#' in the pso part of the algorithm.
#' @importFrom R6 "R6Class"
Causlist <- R6::R6Class("Causlist",
  public = list(
    #' @description 
    #' Constructor of the 'Causlist' class
    #' @param ordering a vector with the names of the nodes in t_0
    #' @param size number of timeslices of the DBN
    #' @return A new 'causlist' object
    initialize = function(ordering, size){
      private$size <- size
      private$ordering <- ordering
      private$cl <- initialize_cl_cpp(ordering, size)
    },
    
    get_cl = function(){return(private$cl)},
    
    get_ordering = function(){return(private$ordering)},
    
    get_size = function(){return(private$size)}
    
  ),
  private = list(
    #' @field cl List of causal units
    cl = NULL,
    #' @field size Size of the DBN
    size = NULL,
    #' @field ordering String vector defining the order of the nodes in a timeslice
    ordering = NULL
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines velocities affecting causality lists in the PSO
#' 
#' The velocities will be defined as a causality list where each element in
#' a causal unit is a pair (v, node) with v being either 0, 1 or -1. 0 means 
#' that arc remained the same, 1 means that arc was added and -1 means that arc 
#' was deleted.
Velocity <- R6::R6Class("Velocity",
  inherit = Causlist,
  public = list(
    #' @description 
    #' Getter of the abs_op attribute.
    #' 
    #' return the number of operations that the velocity performs
    get_abs_op = function(){return(private$abs_op)},
    
    #' @description 
    #' Setter of the abs_op attribute. Intended for inside use only. 
    #' This should be a 'protected' function in Java-like OOP, but there's no 
    #' such thing in R6. This function should not be used from outside the
    #' package.
    #' 
    #' @param n the new number of operations that the velocity performs
    set_abs_op = function(n){private$abs_op = n},
    
    #' @description 
    #' Randomizes the Velocity's directions. If the seed provided is NULL, no
    #' seed will be used.
    #' 
    #' @param probs the weight of each value {-1,0,1}. They define the probability that each of them will be picked 
    #' @param seed the seed provided to the random number generation
    randomize_velocity = function(probs = c(10, 65, 25)){
      numeric_prob_vector_check(probs, 3)
      directions <- randomize_vl_cpp(private$cl, probs)
      private$cl <- directions[[1]]
      private$abs_op <- directions[[2]]
    },
    
    #' @description 
    #' Given a position, returns the velocity that gets this position to the
    #' other.
    #' 
    #' @param ps a Position object
    #' return the Velocity that gets this position to the new one
    subtract_positions = function(ps1, ps2){
      res <- pos_minus_pos_cpp(ps1$get_cl(), ps2$get_cl(), private$cl)
      
      private$cl <- res[[1]]
      private$abs_op <- res[[2]]
    },
    
    #' @description 
    #' Add both velocities directions
    #' 
    #' @param vl a Velocity object
    add_velocity = function(vl){
      res <- vel_plus_vel_cpp(private$cl, vl$get_cl(), private$abs_op)
      
      private$cl <- res[[1]]
      private$abs_op <- res[[2]]
    },
    
    #' @description 
    #' Multiply the Velocity by a constant real number
    #' 
    #' This function multiplies the Velocity by a constant real number. 
    #' It is non deterministic by definition. When calculating k*|V|, the 
    #' result will be floored and bounded to the set [-max_op, max_op], where max_op
    #' is the maximum number of arcs that can be present in the network.
    #' 
    #' @param k a real number
    cte_times_velocity = function(k){
      # initial_numeric_check(k) --ICO-Merge
      
      if(k == 0){
        private$cl <- initialize_cl_cpp(private$ordering, private$size)
        private$abs_op <- 0
      }
      
      else{
        max_op <- (private$size - 1) * length(private$ordering) * length(private$ordering)
        res = cte_times_vel_cpp(k, private$cl, private$abs_op, max_op)
        
        private$cl = res[[1]]
        private$abs_op = res[[2]]
      }
    }
  ),
  private = list(
    #' @field abs_op Total number of operations 1 or -1 in the velocity
    abs_op = NULL
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines DBNs as causality lists
#' 
#' A causality list has a list with causal units, a size representing the
#' Markovian order of the network and a specific node ordering.
Position <- R6::R6Class("Position", 
  inherit = Causlist,
  public = list(
    #' @description 
    #' Constructor of the 'causlist' class
    #' @param net dbn or dbn.fit object defining the network
    #' @param size Number of timeslices of the DBN
    #' @param nodes A list with the names of the nodes in the network
    #' If its not null, a random causlist will be generated for those nodes
    #' @return A new 'causlist' object
    initialize = function(net, size, nodes = NULL){
      #initial_size_check(size) --ICO-Merge
      
      if(!is.null(nodes)){
        #initial_nodes_check(nodes)
        net <- private$generate_random_network(nodes, size)
      }
      else{
        #initial_dbn_check(net) --ICO-Merge
        initial_dbn_to_causlist_check(net)
      }
      
      super$initialize(private$dbn_ordering(net), size)
      private$nodes <- names(net$nodes)
      private$n_arcs <- dim(net$arcs)[1]
      private$cl_translate(net)
    },
    
    get_n_arcs = function(){return(private$n_arcs)},
    
    get_nodes = function(){return(private$nodes)},
    
    #' @description 
    #' Translate the causality list into a DBN network
    #' 
    #' Uses this object private causality list and transforms it into a DBN.
    #' @return a dbn object
    bn_translate = function(){
      arc_mat <- cl_to_arc_matrix_cpp(private$cl, private$ordering, private$n_arcs)
      
      net <- bnlearn::empty.graph(private$nodes)
      bnlearn::arcs(net) <- arc_mat
      
      return(net)
    },
    
    #' @description 
    #' Add a velocity to the position
    #' 
    #' Given a Velocity object, add it to the current position.
    #' @param vl a Velocity object
    add_velocity = function(vl){
      res = pos_plus_vel_cpp(private$cl, vl$get_cl(), private$n_arcs)
      private$cl = res[[1]]
      private$n_arcs = res[[2]]
    },
    
    #' @description 
    #' Given another position, returns the velocity that gets this position to the
    #' other.
    #' 
    #' @param ps a Position object
    #' return the Velocity that gets this position to the new one
    subtract_position = function(ps){
      res <- Velocity$new(private$ordering, private$size)
      res$subtract_positions(self, ps)
      
      return(res)
    }
  ),
  
  private = list(
    #' @field n_arcs Number of arcs in the network
    n_arcs = NULL,
    #' @field nodes Names of the nodes in the network
    nodes = NULL,
    
    #' @description 
    #' Return the static node ordering
    #' 
    #' This function takes as input a dbn and return the node ordering of the
    #' variables inside a timeslice. This ordering is needed to understand a
    #' causal list.
    #' @param net a dbn or dbn.fit object
    #' @return the ordering of the nodes in t_0
    dbn_ordering = function(net){
      return(grep("t_0", names(net$nodes), value = TRUE))
    },
    
    #' @description 
    #' Translate a DBN into a causality list
    #' 
    #' This function takes as input a network from a DBN and transforms the 
    #' structure into a causality list if it is a valid DBN. Valid DBNs have only
    #' inter-timeslice edges and only allow variables in t_0 to have parents.
    #' @param net a dbn object
    #' @return a causlist object
    cl_translate = function(net){
      private$cl <- create_causlist_cpp(private$cl, net$nodes, private$size, private$ordering)
    },
    
    #' @description 
    #' Generates a random DBN valid for causality list translation
    #' 
    #' This function takes as input a list with the names of the nodes and the
    #' desired size of the network and returns a random DBN structure.
    #' @param nodes a character vector with the names of the nodes in the net
    #' @param size the desired size of the DBN
    #' @return a random dbn structure
    generate_random_network = function(nodes, size){
      idx <- grep("t_0", nodes)
      
      if(length(idx) == 0){
        nodes_t_0 <- unlist(lapply(nodes, function(x){paste0(x, "_t_0")}))
        new_nodes <- rename_nodes_cpp(nodes, size)
      }
      else{
        nodes_t_0 <- names(dt)[idx]
        new_nodes <- c(names(dt)[-idx], nodes_t_0)
      }
      
      net <- bnlearn::random.graph(new_nodes)
      net <- private$prune_invalid_arcs(net, nodes_t_0)
      
      return(net)
    },
    
    #' @description 
    #' Fixes a DBN structure to make it suitable for causality list translation
    #' 
    #' This function takes as input a DBN structure and removes the 
    #' intra-timeslice arcs and the arcs that end in a node not in t_0.
    #' @param net the DBN structure
    #' @param nodes_t_0 a vector with the names of the nodes in t_0
    #' @return the fixed network
    prune_invalid_arcs = function(net, nodes_t_0){
      keep_rows <- !(net$arcs[,1] %in% nodes_t_0)
      keep_rows <- keep_rows & (net$arcs[,2] %in% nodes_t_0)
      keep_rows <- net$arcs[keep_rows,]
      bnlearn::arcs(net) <- keep_rows
      
      return(net)
    }
    
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines a Particle in the PSO algorithm
#' 
#' A particle has a Position, a Velocity and a local best
Particle <- R6::R6Class("Particle",
  public = list(
    #' @description 
    #' Constructor of the 'Particle' class
    #' @param ordering a vector with the names of the nodes in t_0
    #' @param size number of timeslices of the DBN
    #' @return A new 'Particle' object
    initialize = function(ordering, size, v_probs){
      private$ps <- Position$new(NULL, size, ordering)
      private$vl <- Velocity$new(private$ps$get_ordering(), size)
      private$vl$randomize_velocity(v_probs)
      private$lb <- -Inf
    },
    
    #' @description 
    #' Evaluate the score of the particle's position
    #' 
    #' Evaluate the score of the particle's position.
    #' Updates the local best if the new one is better.
    #' @param dt dataset to evaluate the fitness of the particle
    #' @return The score of the current position
    eval_ps = function(dt){
      struct <- private$ps$bn_translate()
      score <- bnlearn::score(struct, dt, type = "bge") # For now, unoptimized bge. Any Gaussian score could be used
      if(score > private$lb){
        private$lb <- score 
        private$lb_ps <- private$ps
      }
      
      return(score)
    },
    
    #' @description 
    #' Update the position of the particle with the velocity
    #' 
    #' Update the position of the particle given the constants after calculating
    #' the new velocity
    #' @param in_cte parameter that varies the effect of the inertia
    #' @param gb_cte parameter that varies the effect of the global best
    #' @param gb_ps position of the global best
    #' @param lb_cte parameter that varies the effect of the local best
    #' @param r_probs vector that defines the range of random variation of gb_cte and lb_cte
    update_state = function(in_cte, gb_cte, gb_ps, lb_cte, r_probs){ # max_vl = 20
      # 1.- Inertia of previous velocity
      private$vl$cte_times_velocity(in_cte)
      # 2.- Velocity from global best
      op1 <- gb_cte * runif(1, r_probs[1], r_probs[2])
      vl1 <- gb_ps$subtract_position(private$ps)
      vl1$cte_times_velocity(op1)
      # 3.- Velocity from local best
      op2 <- lb_cte * runif(1, r_probs[1], r_probs[2])
      vl2 <- private$lb_ps$subtract_position(private$ps)
      vl2$cte_times_velocity(op2)
      # 4.- New velocity
      private$vl$add_velocity(vl1)
      private$vl$add_velocity(vl2)
      # 5.- Reduce velocity if higher than maximum. Awful results when the limit is low, so dropped for now.
      # if(private$vl$get_abs_op() > max_vl)
      #    private$vl$cte_times_velocity(max_vl / private$vl$get_abs_op())
      # 6.- New position
      private$ps$add_velocity(private$vl)
      # 7.- If a node has more parents than the maximum, reduce them (TODO)
    },
    
    get_ps = function(){return(private$ps)},
    
    get_vl = function(){return(private$vl)},
    
    get_lb = function(){return(private$lb)},
    
    get_lb_ps = function(){return(private$lb_ps)}
  ),
  
  private = list(
    #' @field ps position of the particle
    ps = NULL,
    #' @field cl velocity of the particle
    vl = NULL,
    #' @field lb local best score obtained
    lb = NULL,
    #' @field lb_ps local best position found
    lb_ps = NULL
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines the PSO controller
#' 
#' The controller will encapsulate the particles and run the algorithm
PsoCtrl <- R6::R6Class("PsoCtrl",
  public = list(
   #' @description 
   #' Constructor of the 'PsoCtrl' class
   #' @param ordering a vector with the names of the nodes in t_0
   #' @param size number of timeslices of the DBN
   #' @param n_inds number of particles that the algorithm will simultaneously process
   #' @param n_it maximum number of iterations of the pso algorithm
   #' @param in_cte parameter that varies the effect of the inertia
   #' @param gb_cte parameter that varies the effect of the global best
   #' @param lb_cte parameter that varies the effect of the local best
   #' @param v_probs vector that defines the random velocity initialization probabilities
   #' @param r_probs vector that defines the range of random variation of gb_cte and lb_cte
   #' @return A new 'PsoCtrl' object
   initialize = function(ordering, size, n_inds, n_it, in_cte, gb_cte, lb_cte,
                         v_probs, r_probs){
     private$initialize_particles(ordering, size, n_inds, v_probs)
     private$gb_scr <- -Inf
     private$n_it <- n_it
     private$in_cte <- in_cte
     private$gb_cte <- gb_cte
     private$lb_cte <- lb_cte
     private$r_probs <- r_probs
   },
   
   #' @description 
   #' Getter of the cluster attribute
   #' @return the cluster attribute
   get_cl = function(){return(private$cl)},
   
   #' @description 
   #' Transforms the best position found into a bn structure and returns it
   #' @return the size attribute
   get_best_network = function(){return(private$gb_ps$bn_translate())},
   
   #' @description 
   #' Main function of the pso algorithm.
   #' @param dt the dataset from which the structure will be learned
   run = function(dt){
     # Missing security checks --ICO-Merge
     private$evaluate_particles(dt)
     pb <- utils::txtProgressBar(min = 0, max = private$n_it, style = 3)
     # Main loop of the algorithm.
     for(i in 1:private$n_it){
       # Inside loop. Update each particle
       for(p in private$parts)
         p$update_state(private$in_cte, private$gb_cte, private$gb_ps, private$lb_cte, private$r_probs)
       
       private$evaluate_particles(dt)
       utils::setTxtProgressBar(pb, i)
     }
     close(pb)
   }
  ),
  private = list(
   #' @field parts list with all the particles in the algorithm
   parts = NULL,
   #' @field cl cluster for the parallel computations
   cl = NULL,
   #' @field n_it maximum number of iterations of the pso algorithm
   n_it = NULL,
   #' @field in_cte parameter that varies the effect of the inertia
   in_cte = NULL,
   #' @field gb_cte parameter that varies the effect of the global best
   gb_cte = NULL,
   #' @field lb_cte parameter that varies the effect of the local best
   lb_cte = NULL,
   #' @field b_ps global best position found
   gb_ps = NULL,
   #' @field b_scr global best score obtained
   gb_scr = NULL,
   #' @field r_probs vector that defines the range of random variation of gb_cte and lb_cte
   r_probs = NULL,
   
   #' @description 
   #' Initialize the particles for the algorithm to random positions and velocities.
   #' @param ordering a vector with the names of the nodes in t_0
   #' @param size number of timeslices of the DBN
   #' @param n_inds number of particles that the algorithm will simultaneously process
   #' @param v_probs vector that defines the random velocity initialization probabilities
   initialize_particles = function(ordering, size, n_inds, v_probs){
     #private$parts <- parallel::parLapply(private$cl,1:n_inds, function(i){Particle$new(ordering, size)})
     private$parts <- vector(mode = "list", length = n_inds)
     for(i in 1:n_inds)
       private$parts[[i]] <- Particle$new(ordering, size, v_probs)
   },
   
   #' @description 
   #' Evaluate the particles and update the global best
   #' @param dt the dataset used to evaluate the position
   evaluate_particles = function(dt){
     for(p in private$parts){
       scr <- p$eval_ps(dt)
       if(scr > private$gb_scr){
         private$gb_scr <- scr
         private$gb_ps <- p$get_ps()
       }
     }
   }
  )
)

# -----------------------------------------------------------------------------

#' Learn a DBN structure with a PSO approach
#' 
#' Given a dataset and the desired Markovian order, this function returns a DBN
#' structure ready to be fitted. It requires a folded dataset.
#' Original algorithm at https://doi.org/10.1109/BRC.2014.6880957
#' @param dt a data.table with the data of the network to be trained
#' @param size Number of timeslices of the DBN. Markovian order 1 equals size 2, and so on.
#' @param n_inds Number of particles used in the algorithm.
#' @param n_it Maximum number of iterations that the algorithm can perform.
#' @param in_cte parameter that varies the effect of the inertia
#' @param gb_cte parameter that varies the effect of the global best
#' @param lb_cte parameter that varies the effect of the local best
#' @param v_probs vector that defines the random velocity initialization probabilities
#' @param r_probs vector that defines the range of random variation of gb_cte and lb_cte
#' @param f_dt previously folded dataset, in case some specific rows have to be removed after the folding
#' @return A 'bn' object with the structure of the best network found
psoho <- function(dt, size, f_dt = NULL, n_inds = 50, n_it = 50,
                                    in_cte = 1, gb_cte = 0.5, lb_cte = 0.5,
                                    v_probs = c(10, 65, 25), 
                                    r_probs = c(-0.5, 1.5)){
  numeric_arg_check(n_inds, n_it, in_cte, gb_cte, lb_cte)
  numeric_prob_vector_check(v_probs, 3)
  numeric_prob_vector_check(r_probs, 2)
  
  if(is.null(f_dt)){
    ordering <- names(dt)
    dt <- time_rename(dt)
    f_dt <- fold_dt_rec(dt, names(dt), size)
  }
  
  else
    ordering <- gsub("_t_0", "", grep("_t_0", names(f_dt), value = T))
  
  ctrl <- PsoCtrl$new(ordering, size, n_inds, n_it, in_cte, gb_cte, lb_cte,
                      v_probs, r_probs)
  ctrl$run(f_dt)
  
  net <- ctrl$get_best_network()
  class(net) <- c("dbn", class(net))
  
  return(net)
}

