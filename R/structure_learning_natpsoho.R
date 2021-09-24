#' This file contains all the classes needed for the natPSOHO structure learning 
#' algorithm. It was implemented as an independent package in
#' https://github.com/dkesada/natPSOHO and then merged into dbnR. All the original
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
#' in the pso part of the algorithm. They will not have the same structure
#' as their binary counterparts, but their class skeleton will serve as a
#' base.
natCauslist <- R6::R6Class("natCauslist",
  public = list(
   #' @description 
   #' Constructor of the 'natCauslist' class
   #' @param ordering a vector with the names of the nodes in t_0
   #' @param ordering_raw a vector with the names of the nodes without the appended "_t_0"
   #' @return A new 'natCauslist' object
   initialize = function(ordering, ordering_raw){
     private$ordering <- ordering
     private$ordering_raw <- ordering_raw
     private$cl <- init_cl_cpp(length(ordering) * length(ordering))
   },
   
   get_cl = function(){return(private$cl)},
   
   get_ordering = function(){return(private$ordering)}
   
  ),
  private = list(
   #' @field cl List of causal units
   cl = NULL,
   #' @field ordering String vector defining the order of the nodes in t_0
   ordering = NULL,
   #' @field ordering String vector defining the order of the nodes without the appended "_t_0"
   ordering_raw = NULL
   
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines velocities in the PSO
#' 
#' The velocities will be defined as two natural vectors where each element in
#' them represents the arcs from a temporal family of nodes to a receiving
#' node. 1-bits in the binary representation of this number mean arc 
#' additions/deletions 
natVelocity <- R6::R6Class("natVelocity",
  inherit = natCauslist,
  public = list(
   #' @description 
   #' Constructor of the 'natVelocity' class. Only difference with the
   #' natCauslist one is that it has a negative cl attribute.
   #' @param ordering a vector with the names of the nodes in t_0
   #' @param ordering_raw a vector with the names of the nodes without the appended "_t_0"
   #' @param max_size maximum number of timeslices of the DBN
   #' @return A new 'natVelocity' object
   initialize = function(ordering, ordering_raw, max_size){
     super$initialize(ordering, ordering_raw)
     private$abs_op <- 0
     private$max_size <- max_size
     private$cl_neg <- init_cl_cpp(length(private$cl))
   },
   
   get_cl_neg = function(){return(private$cl_neg)},
   
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
   #' Randomizes the Velocity's directions.
   #' 
   #' @param probs the weight of each value {-1,0,1}. They define the probability that each of them will be picked 
   #' @param p the parameter of the geometric distribution
   randomize_velocity = function(probs = c(10, 65, 25), p = 0.06){
     numeric_prob_vector_check(probs)
     
     for(i in 1:length(private$cl)){
       op <- rmultinom(n = 1, size = 1, prob = probs)
       if(op[3] == 1){
         if(p <= 0)
           private$cl[i] <- floor(runif(1, 0, private$max_size))
         else
           private$cl[i] <- trunc_geom(p, 2^(private$max_size - 1))
         private$abs_op <- private$abs_op + bitcount(private$cl[i])
       }
       else if (op[1] == 1){
         if(p <= 0)
           private$cl_neg[i] <- floor(runif(1, 0, private$max_size))
         else
           private$cl_neg[i] <- trunc_geom(p, 2^(private$max_size - 1))
         private$abs_op <- private$abs_op + bitcount(private$cl_neg[i])
       }
     }
   },
   
   #' @description 
   #' Given two positions, returns the velocity that gets the first position to the
   #' other one.
   #' 
   #' @param ps1 the origin natPosition object
   #' @param ps2 the objective natPosition object
   #' @return the natVelocity that gets the ps1 to ps2
   subtract_positions = function(ps1, ps2){
     private$abs_op <- nat_pos_minus_pos_cpp(ps1$get_cl(), ps2$get_cl(), private$cl, private$cl_neg)
   },
   
   #' @description 
   #' Add both velocities directions
   #' 
   #' @param vl a Velocity object
   add_velocity = function(vl){
     private$abs_op <- nat_vel_plus_vel_cpp(private$cl, private$cl_neg, vl$get_cl(), vl$get_cl_neg(), private$abs_op, vl$get_abs_op())
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
     # If k < 0, invert the cl and the cl_neg
     if(k < 0){ 
       tmp <- private$cl
       private$cl <- private$cl_neg
       private$cl_neg <- tmp
       k <- abs(k)
     }
     
     if(k == 0){
       private$cl <- init_cl_cpp(length(private$cl))
       private$cl_neg <- init_cl_cpp(length(private$cl_neg))
       private$abs_op <- 0
     }
     
     else
       private$abs_op = nat_cte_times_vel_cpp(k, private$cl, private$cl_neg, private$abs_op, private$max_size)
   }
  ),
  private = list(
   #' @field abs_op Total number of operations 1 or -1 in the velocity
   abs_op = NULL,
   #' @field max_size Maximum number of timeslices of the DBN
   max_size = NULL,
   #' @field cl_neg Negative part of the velocity
   cl_neg = NULL
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines DBNs as vectors of natural numbers
#' 
#' A natPosition represents a single HO-DBN structure with a vector. Its function
#' is to encode the solutions in the PSO framework. Each particle will have a 
#' position.
natPosition <- R6::R6Class("natPosition", 
  inherit = natCauslist,
  public = list(
   #' @description 
   #' Constructor of the 'natPosition' class
   #' @param nodes a vector with the names of the nodes
   #' @param ordering a vector with the names of the nodes in t_0
   #' @param ordering_raw a vector with the names of the nodes without the appended "_t_0"
   #' @param max_size Maximum number of timeslices of the DBN
   #' @param p the parameter of the sampling truncated geometric distribution
   #' If lesser or equal to 0, a uniform distribution will be used instead. 
   #' @return A new 'natPosition' object
   #' @importFrom dbnR fold_dt
   initialize = function(nodes, ordering, ordering_raw, max_size, p = 0.06){
     super$initialize(ordering, ordering_raw)
     private$nodes <- nodes
     private$cl <- private$generate_random_position(length(ordering), max_size, p)
     private$n_arcs <- private$recount_arcs()
     private$max_size <- max_size
     private$p <- p
   },
   
   get_n_arcs = function(){return(private$n_arcs)},
   
   #' @description 
   #' Translate the vector into a DBN network
   #' 
   #' Uses this object private cl and transforms it into a DBN.
   #' @return a dbn object
   bn_translate = function(){
     arc_mat <- cl_to_arc_matrix_cpp(private$cl, private$ordering_raw, private$n_arcs)
     
     net <- bnlearn::empty.graph(private$nodes)
     bnlearn::arcs(net, check.cycles = FALSE, check.illegal = FALSE) <- arc_mat
     
     return(net)
   },
   
   #' @description 
   #' Add a velocity to the position
   #' 
   #' Given a natVelocity object, add it to the current position.
   #' @param vl a natVelocity object
   add_velocity = function(vl){
     private$n_arcs <- nat_pos_plus_vel_cpp(private$cl, vl$get_cl(), vl$get_cl_neg(), private$n_arcs)
   },
   
   #' @description 
   #' Given another position, returns the velocity that gets it to this
   #' position.
   #'  
   #' @param ps a natPosition object
   #' return the natVelocity that gets the other position to this one
   subtract_position = function(ps){
     res <- natVelocity$new(private$ordering, private$ordering_raw, private$max_size)
     res$subtract_positions(ps, self)
     
     return(res)
   }
  ),
  
  private = list(
   #' @field n_arcs Number of arcs in the network
   n_arcs = NULL,
   #' @field max_size Maximum number of timeslices of the DBN
   max_size = NULL,
   #' @field p Parameter of the sampling truncated geometric distribution
   p = NULL,
   #' @field nodes Names of the nodes in the network
   nodes = NULL,
   
   #' @description 
   #' Return the static node ordering
   #' 
   #' This function takes as input a dbn and return the node ordering of the
   #' variables inside a timeslice. This ordering is needed to understand a
   #' position vector.
   #' @param net a dbn or dbn.fit object
   #' @return the ordering of the nodes in t_0
   dbn_ordering = function(net){
     return(grep("t_0", names(net$nodes), value = TRUE))
   },
   
   #' @description 
   #' Translate a DBN into a position vector
   #' 
   #' This function takes as input a network from a DBN and transforms the 
   #' structure into a vector of natural numbers if it is a valid DBN. Valid 
   #' DBNs have only inter-timeslice edges and only allow variables in t_0 to 
   #' have parents.
   #' @param net a dbn object
   cl_translate = function(net){
     private$cl <- create_natcauslist_cpp(private$cl, net$nodes, private$ordering)
   },
   
   #' @description 
   #' Generates a random position
   #' 
   #' This function takes as input the number of variables, the maximum size
   #' and the parameter p and returns a random position with arcs 
   #' sampled either from the uniform distribution or from a truncated 
   #' geometric distribution. Much faster than the binary implementation with
   #' lists of lists and random bn generation into translation.
   #' @param n_vars the number of variables in t_0
   #' @param max_size the maximum size of the DBN
   #' @param p the parameter of the truncated geometric sampler. If lesser or
   #' equal to 0, a uniform distribution will be used instead.
   #' @return a random position
   generate_random_position = function(n_vars, max_size, p){
     res <- init_cl_cpp(n_vars * n_vars)
     
     if(p <= 0){
       res <- floor(runif(n_vars * n_vars, 0, max_size))
     }
     
     else{
       for(i in 1:length(res))
         res[i] <- trunc_geom(p, max_size)
     }
     
     return(res)
   },
   
   #' @description 
   #' Recount the number of arcs in the cl
   #' @return the number of arcs
   recount_arcs = function(){
     private$n_arcs <- 0
     for(i in 1:length(private$cl))
       private$n_arcs <- private$n_arcs + bitcount(private$cl[i])
     
     return(private$n_arcs)
   }
   
  )
)

# -----------------------------------------------------------------------------

#' R6 class that defines a Particle in the PSO algorithm
#' 
#' A particle has a Position, a Velocity and a local best
natParticle <- R6::R6Class("natParticle",
  public = list(
   #' @description 
   #' Constructor of the 'natParticle' class
   #' @param nodes a vector with the names of the nodes
   #' @param ordering a vector with the names of the nodes in t_0
   #' @param ordering_raw a vector with the names of the nodes without the appended "_t_0"
   #' @param max_size maximum number of timeslices of the DBN
   #' @param v_probs vector of probabilities for the velocity sampling
   #' @param p parameter of the truncated geometric distribution 
   #' @return A new 'natParticle' object
   initialize = function(nodes, ordering, ordering_raw, max_size, v_probs, p){
     private$ps <- natPosition$new(nodes, ordering, ordering_raw, max_size, p)
     private$vl <- natVelocity$new(ordering, ordering_raw, max_size)
     private$vl$randomize_velocity(v_probs, p)
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
     struct <- private$ps$bn_translate() # --ICO-Improve a custom bge score could avoid translating and could perform the score over the nat vector
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
natPsoCtrl <- R6::R6Class("natPsoCtrl",
   public = list(
     #' @description 
     #' Constructor of the 'natPsoCtrl' class
     #' @param nodes a vector with the names of the nodes
     #' @param max_size maximum number of timeslices of the DBN
     #' @param n_inds number of particles that the algorithm will simultaneously process
     #' @param n_it maximum number of iterations of the pso algorithm
     #' @param in_cte parameter that varies the effect of the inertia
     #' @param gb_cte parameter that varies the effect of the global best
     #' @param lb_cte parameter that varies the effect of the local best
     #' @param v_probs vector that defines the random velocity initialization probabilities
     #' @param p parameter of the truncated geometric distribution for sampling edges
     #' @param r_probs vector that defines the range of random variation of gb_cte and lb_cte
     #' @param cte boolean that defines whether the parameters remain constant or vary as the execution progresses
     #' @return A new 'natPsoCtrl' object
     initialize = function(nodes, max_size, n_inds, n_it, in_cte, gb_cte, lb_cte,
                           v_probs, p, r_probs, cte){
        ordering <- grep("_t_0", nodes, value = TRUE) 
        private$initialize_particles(nodes, ordering, max_size, n_inds, v_probs, p)
        private$gb_scr <- -Inf
        private$n_it <- n_it
        private$in_cte <- in_cte
        private$gb_cte <- gb_cte
        private$lb_cte <- lb_cte
        private$r_probs <- r_probs
        private$cte <- cte
        if(!cte){
           private$in_var <- in_cte / n_it # Decrease inertia
           private$gb_var <- (1-gb_cte) / n_it # Increase gb
           private$lb_var <- lb_cte / n_it # Decrease gb
        }
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
           
           if(!private$cte)
              private$adjust_pso_parameters()
           
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
     #' @field cte boolean that defines whether the parameters remain constant or vary as the execution progresses
     cte = NULL,
     #' @field in_var decrement of the inertia each iteration
     in_var = NULL,
     #' @field gb_var increment of the global best parameter each iteration
     gb_var = NULL,
     #' @field lb_var increment of the local best parameter each iteration
     lb_var = NULL,
     
     #' @description 
     #' If the names of the nodes have "_t_0" appended at the end, remove it
     #' @param ordering a vector with the names of the nodes in t_0
     #' @return the ordering with the names cropped
     crop_names = function(ordering){
        sapply(ordering, function(x){gsub("_t_0", "", x)}, USE.NAMES = F)
     },
     
     #' @description 
     #' Initialize the particles for the algorithm to random positions and velocities.
     #' @param nodes a vector with the names of the nodes
     #' @param ordering a vector with the names of the nodes in t_0
     #' @param max_size maximum number of timeslices of the DBN
     #' @param n_inds number of particles that the algorithm will simultaneously process
     #' @param v_probs vector that defines the random velocity initialization probabilities
     #' @param p parameter of the truncated geometric distribution for sampling edges
     initialize_particles = function(nodes, ordering, max_size, n_inds, v_probs, p){
        #private$parts <- parallel::parLapply(private$cl,1:n_inds, function(i){Particle$new(ordering, size)})
        private$parts <- vector(mode = "list", length = n_inds)
        ordering_raw <- private$crop_names(ordering)
        for(i in 1:n_inds)
           private$parts[[i]] <- natParticle$new(nodes, ordering, ordering_raw, max_size, v_probs, p)
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
     },
     
     #' @description 
     #' Modify the PSO parameters after each iteration
     adjust_pso_parameters = function(){
        private$in_cte <- private$in_cte - private$in_var
        private$gb_cte <- private$gb_cte + private$gb_var
        private$lb_cte <- private$lb_cte - private$lb_var
     }
   )
)

