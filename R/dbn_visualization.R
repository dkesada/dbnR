#' Defines a level for every node in the net
#'
#' Calculates the levels in which the nodes will be distributed when plotting
#' the structure. This level is defined by their parent nodes: a node with no
#' parents will always be in the level 0. Subsequently, the level of a node
#' will be one more of the maximum level of his parents.
#' @param net the structure of the network.
#' @param order a topological order of the nodes, with the orphan nodes
#' in the first place. See \code{\link[bnlearn]{node.ordering}}
#' @param lvl current level being processed
#' @param acc accumulator of the nodes already processed
#' @return a matrix with the names of the nodes in the first row and their
#' level on the second
node_levels <- function(net, order, lvl = 1, acc = NULL){
  ret <- acc
  if(length(order) > 0){
    pa <- which(acc[1,] %in% bnlearn::parents(net, order[1]))
    if(length(pa) > 0 && max(as.numeric(cbind(c("_","0"),acc[,pa])[2,])) == lvl)
      lvl <- lvl + 1
    ret <- node_levels(net, order[-1], lvl, cbind(acc, c(order[1], lvl)))
  }

  return(ret)
}

#' Plots a Bayesian networks in a hierarchical way
#'
#' Calculates the levels of each node and then plots them in a hierarchical
#' layout in visNetwork.
#' @param net the structure of the network.
#' @param size the number of time slices of the net. Markovian 1 would be size 2
#' @param acc accumulator of the results in the recursion
#' @param slice current time slice that is being processed
#' @return the visualzation of the net
#' @importFrom magrittr "%>%"
#' @export
plot_network <- function(structure){
  check_opt_pkgs_available()
  initial_bn_check(structure)
  if(is_dbn_or_dbnfit(structure))
    warning("this function is for static networks. For DBNs, 'plot_dynamic_network' is the correct one")

  nodes_uniq <- bnlearn::node.ordering(structure)
  nodes <- data.frame(id = nodes_uniq,
                      label = nodes_uniq,
                      level = node_levels(structure, nodes_uniq)[2,],
                      color.background = rgb(red = 0.6, blue = 0.33,
                                             green = 0.200, alpha = 0.5),
                      color.border = "black",
                      borderWidth = 2,
                      shadow = FALSE,
                      physics = FALSE)

  edges <- data.frame(from = bnlearn::arcs(structure)[,1],
                      to = bnlearn::arcs(structure)[,2],
                      arrows = "to",
                      #smooth = TRUE,
                      shadow = FALSE,
                      color = "black")

  ret <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visHierarchicalLayout(levelSeparation = 100) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = F),
               nodesIdSelection = T)

  eval(ret)
}

#' Returns a vector with the number of consecutive nodes in each level
#'
#' This method processes the vector of node levels to get the position of
#' each node inside the level.
#' @param nodes a vetor with the level of each node
#' @param res the accumulative results of the sub succesions
#' @param prev the level of the previous node processed
#' @param acc the accumulator of the index in the current sub succesion
#' @return the vector of sub succesions in each level
#' @examples
#' levels <- c(1,1,1,2,2,3,4,4,5,5)
#' levels <- acc_succesion(levels)
#' levels # equals to c(1,2,3,1,2,1,1,2,1,2)
acc_succesion <- function(nodes, res = NULL, prev = 0, acc = 0){
  if(length(nodes) == 0)
    return(res)
  else if(prev == nodes[1])
    return(acc_succesion(nodes[-1], c(res, acc+1), prev, acc+1))
  else
    return(acc_succesion(nodes[-1], c(res, 1), nodes[1], 1))
}

#' Gets the ordering of a single time slice in a DBN
#'
#' This method gets the structure of a DBN, isolates the nodes of a single
#' time slice and then gives a topological ordering of them.
#' @param structure the structure of the network.
#' @return the ordered nodes of t_0
dynamic_ordering <- function(structure){

  nodes_0 <- grep("t_0", bnlearn::node.ordering(structure), value = T)
  nodes_pa <- lapply(nodes_0, grep, pattern="t_0", value=T)
  nodes_pa <- lapply(nodes_pa, bnlearn::parents, x = structure)
  nodes_pa <- lapply(nodes_pa, grep, pattern = "t_0", value = T)
  orphans <- which(unlist(lapply(nodes_pa, length)) == 0)
  # Put the nodes without parents in t_0 in the front to solve ordering problems
  nodes_0 <- c(nodes_0[orphans], nodes_0[-orphans])

  return(nodes_0)
}

#' Extends the names of the nodes in t_0 to t_(max-1)
#'
#' This method extends the names of the nodes to the given maximum and
#' mantains the order of the nodes in each slice, so as to plotting
#' the nodes in all slices relative to their homonyms in the first slice.
#' @param name the names of the nodes in the t_0 slice
#' @param acc accumulator of the resulting names in the recursion
#' @param max number of time slices in the net
#' @param i current slice being processed
#' @return the extended names
expand_time_nodes <- function(name, acc, max, i){
  if(i == max)
    return(acc)

  return(expand_time_nodes(name, c(sapply(name,sub, pattern = "_t_0",
                                       replacement = paste0("_t_",i),
                                       USE.NAMES = F),acc), max, i+1))
}

#' Plots a dynamic Bayesian network in a hierarchical way
#'
#' To plot the DBN, this method first computes a hierarchical structure
#' for a time slice and replicates it for each slice. Then, it calculates the
#' relative position of each node with respect to his equivalent in the first
#' slice. The result is a net where each time slice is ordered and separated
#' from one another, where the leftmost slice is the oldest and the rightmost
#' represents the present time.
#' @param structure the structure of the network.
#' @param offset the blank space between time slices
#' @return the visualization of the DBN
#' @importFrom magrittr "%>%"
#' @export
plot_dynamic_network <- function(structure, offset = 200){
  check_opt_pkgs_available()
  initial_dbn_check(structure)
  numeric_arg_check(offset)

  # Static net positioning
  nodes_uniq <- dynamic_ordering(structure)
  levels <- node_levels(structure, nodes_uniq)
  positions <- acc_succesion(as.numeric(levels[2,]))
  n_nodes_slice <- length(nodes_uniq)

  max_consec <- max(positions)
  ord <- bnlearn::nnodes(structure) / n_nodes_slice

  # Relative position of the nodes
  nodes_uniq <- expand_time_nodes(nodes_uniq, nodes_uniq, ord, 1)
  all_pos <- NULL

  for(i in (0:(ord-1)))
    all_pos <- c(all_pos, (positions * 100 + i * 100 * max_consec + i * offset))

  color <- grDevices::col2rgb(grDevices::hcl.colors(ord, palette = "RdYlBu")) / 255
  color <- apply(color, 2, function(x){do.call(grDevices::rgb,as.list(x))})

  all_colors <- NULL
  for(c in color)
    all_colors <- c(all_colors, rep(c, n_nodes_slice)) # TODO: Check for an equivalent with sapply

  nodes <- data.frame(id = nodes_uniq,
                      label = nodes_uniq,
                      x = all_pos,
                      y = as.numeric(levels[2,]) * 100,
                      #level = node_levels(structure, nodes_uniq)[2,],
                      color.background = all_colors,
                      color.border = "black",
                      borderWidth = 2,
                      #shape = "star",
                      shadow = FALSE,
                      physics = FALSE)

  edges <- data.frame(from = bnlearn::arcs(structure)[,1],
                      to = bnlearn::arcs(structure)[,2],
                      arrows = "to",
                      #color = "orange",
                      #smooth = TRUE,
                      shadow = FALSE,
                      color = "black")

  ret <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T),
                           nodesIdSelection = T)

  eval(ret)
}

nodes.dbn <- function(dbn){
  return(names(dbn$nodes))
}
