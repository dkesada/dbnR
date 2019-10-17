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
create_blacklist <- function(name, size, acc = NULL, slice = 1){
  # Create the blacklist so that there are no arcs from t to t-1 and within t
  if(size >= slice){
    n <- grep(paste0("t_", (slice-1)), name)
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
#' Learns a gaussian dynamic bayesian network from a
#' dataset. It allows the creation of markovian n nets rather than only markov
#' 1.
#' @param dt the data.frame or data.table to be used
#' @param size number of time slices of the net. Markovian 1 would be size 2
#' @return the folded data.table and the structure of the net
#' @import data.table
#' @export
learn_dbn_struc <- function(dt, size = 2){
  initial_size_check(size)
  initial_df_check(dt)
  if(!is.data.table(dt))
    dt <- as.data.table(dt)

  dt <- time_rename(dt)

  dt_copy <- data.table::copy(dt)

  net0 <- bnlearn::rsmax2(x = dt_copy, restrict="mmpc", maximize = "hc",
                          restrict.args = list(test = "cor"),
                          maximize.args = list(score = "bic-g")) # Static network. hc(..., maxp = 3)

  f_dt <- fold_dt(dt, names(dt), size)
  blacklist <- create_blacklist(names(f_dt), size) #TODO: bug when size = 20

  net <- bnlearn::rsmax2(x = f_dt, restrict="mmpc", maximize = "hc",
                         restrict.args = list(test = "cor"),
                         maximize.args = list(score = "bic-g"),
                         blacklist = blacklist) # kTBN

  bnlearn::arcs(net) <- merge_nets(net0, net, size)
  class(net) <- c("dbn", class(net))

  return(list(f_dt = f_dt, net = net))
}

#' Fits a markovian n DBN model
#'
#' Fits the parameters of the DBN via MLE or BGE.
#' @param f_dt a folded data.table
#' @param net the structure of the DBN
#' @return the fitted net
#' @export
fit_dbn_params <- function(net, f_dt){
  initial_folded_dt_check(f_dt)
  initial_dbn_check(net)

  fit <- bnlearn::bn.fit(net, f_dt)
  class(fit)[grep("dbn", class(fit))] <- "dbn.fit"

  return(fit)
}
