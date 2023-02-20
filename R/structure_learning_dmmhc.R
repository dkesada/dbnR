#' Creates the blacklist of arcs from a folded data.table
#'
#' This will create the blacklist of arcs that are not
#' to be learned in the second phase of the dmmhc. This includes arcs
#' backwards in time or inside time-slices.
#' @param name the names of the first time slice, ended in _t_0
#' @param size the number of time slices of the net. Markovian 1 would be size 2
#' @param acc accumulator of the results in the recursion
#' @param slice current time slice that is being processed
#' @return the two column matrix with the blacklisted arcs
#' @keywords internal
create_blacklist <- function(name, size, acc = NULL, slice = 1){
  # Create the blacklist so that there are no arcs from t to t-1 and within t
  if(size >= slice){
    n <- grep(paste0("t_", (slice-1), "$"), name)
    len <- length(n)
    from <- name[n]
    to = name[-n]
    
    if((size - slice) > 0)
      fromArc <- as.vector(sapply(from, rep, times = (size - slice) * len, simplify=T))
    else
      fromArc = NULL
    
    toArc <- rep(to, times = len)
    
    withinTo <- rep(from, len)
    withinFrom <- as.vector(sapply(from, rep, times = len, simplify = T))
    
    local_blacklist <- cbind(c(withinFrom, fromArc), c(withinTo, toArc))
    
    acc <- create_blacklist(to, size, rbind(acc, local_blacklist), slice + 1)
  }
  
  return(acc)
}

#' Merges and replicates the arcs in the static BN into all the time-slices
#' in the DBN
#'
#' This will join the static net and the state transition
#' net by replicating the arcs in the static net in all the time slices.
#' @param net0 the structure of the static net
#' @param netCP1 the state transition net
#' @param size the number of time slices of the net. Markovian 1 would be size 2
#' @param acc accumulator of the results in the recursion
#' @param slice current time slice that is being processed
#' @return the merged nets
#' @keywords internal
merge_nets <- function(net0, netCP1, size, acc = NULL, slice = 1){
  if(size > slice){
    within_t = bnlearn::arcs(net0)
    within_t <- apply(within_t, 2, sub, pattern = "_t_0",
                      replacement = paste0("_t_",slice))
    ret <- merge_nets(net0, netCP1, size, rbind(acc,within_t), slice = slice + 1)
  }
  
  else
    ret <- rbind(bnlearn::arcs(net0), acc, bnlearn::arcs(netCP1))
  
  return(ret)
}

#' Learns the structure of a markovian n DBN model from data
#'
#' Learns a gaussian dynamic Bayesian network from a
#' dataset. It allows the creation of markovian n nets rather than only markov
#' 1.
#' @param dt the data.frame or data.table to be used
#' @param size number of time slices of the net. Markovian 1 would be size 2
#' @param f_dt previously folded dataset, in case some specific rows have to be removed after the folding
#' @param blacklist an optional matrix indicating forbidden arcs between nodes
#' @param intra if TRUE, the intra-slice arcs of the network will be learnt. If FALSE, they will be ignored
#' @param blacklist_tr an optional matrix indicating forbidden intra-slice arcs between nodes 
#' @param white an optional matrix indicating obligatory arcs between nodes
#' @param whitelist_tr an optional matrix indicating obligatory intra-slice arcs between nodes 
#' @param ... additional parameters for \code{\link{rsmax2}} function
#' @return the structure of the net
#' @import data.table
#' @keywords internal
dmmhc <- function(dt, size = 2, f_dt = NULL, blacklist = NULL, intra = TRUE,
                  blacklist_tr = NULL, whitelist = NULL, whitelist_tr = NULL, ...){
  dt_null_check(dt, intra)
  
  if(!is.null(dt)){
    dt <- time_rename(dt)
    if(intra){
      dt_copy <- data.table::copy(dt)
      net0 <- bnlearn::rsmax2(x = dt_copy, blacklist = blacklist, whitelist = whitelist, ...) # Static network
    }
  }
  
  if(is.null(f_dt))
    f_dt <- fold_dt_rec(dt, names(dt), size)
  
  blacklist <- create_blacklist(names(f_dt), size)
  blacklist <- rbind(blacklist, blacklist_tr)
  
  net <- bnlearn::rsmax2(x = f_dt, blacklist = blacklist, whitelist = whitelist_tr, ...) # Transition network
  check_empty_net(net)
  
  if(intra && !warn_empty_net(net0))
    bnlearn::arcs(net) <- merge_nets(net0, net, size)
  class(net) <- c("dbn", class(net))
  
  return(net)
}
