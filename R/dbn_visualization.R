#' Defines a level for every node in the net
#'
#' Calculates the levels in which the nodes will be distributed when plotting
#' the structure. This level is defined by their parent nodes: a node with no
#' parents will always be in the level 0. Subsequently, the level of a node
#' will be one more of the maximum level of his parents.
#' @param net the structure of the network.
#' @param size the number of time slices of the net. Markovian 1 would be size 2
#' @param acc accumulator of the results in the recursion
#' @param slice current time slice that is being processed
#' @return the two column matrix with the blacklisted arcs
node_levels <- function(net, order, lvl = 1, proc = NULL){
  ret <- proc
  if(length(order) > 0){
    pa <- which(proc[1,] %in% parents(net, order[1]))
    if(length(pa) > 0 && max(as.numeric(cbind(c("_","0"),proc[,pa])[2,])) == lvl)
      lvl <- lvl + 1
    ret <- node_levels(net, order[-1], lvl, cbind(proc, c(order[1], lvl)))
  }

  return(ret)
}

plot_network <- function(structure){
  nodes_uniq <- node.ordering(structure)
  nodes <- data.frame(id = nodes_uniq,
                      label = nodes_uniq,
                      level = node_levels(structure, nodes_uniq)[2,],
                      color.background = rgb(red = 0.6, blue = 0.33,
                                             green = 0.200, alpha = 0.5),
                      color.border = "black",
                      borderWidth = 2,
                      shadow = FALSE,
                      physics = FALSE)

  edges <- data.frame(from = arcs(structure)[,1],
                      to = arcs(structure)[,2],
                      arrows = "to",
                      #smooth = TRUE,
                      shadow = FALSE,
                      color = "black")

  ret <- visNetwork(nodes, edges) %>% # TODO: import the pipe from magrittr
    visHierarchicalLayout(levelSeparation = 100) %>%
    visOptions(highlightNearest = list(enabled = T, hover = F),
               nodesIdSelection = T)

  return(ret)
}

# Recursive function
acc_succesion_rec <- function(nodes, res, prev, acc){
  if(length(nodes) == 0)
    return(res)
  else if(prev == nodes[1])
    return(acc_succesion_rec(nodes[-1], c(res, acc+1), prev, acc+1))
  else
    return(acc_succesion_rec(nodes[-1], c(res, 1), nodes[1], 1))
}

# Recursive handler
acc_succesion <- function(levels){
  return(acc_succesion_rec(as.numeric(levels[2,]), NULL, 0, 0))
}

dynamic_ordering <- function(structure){
  # Put the nodes without parents in t_0 in the front
  nodes_0 <- grep("t_0",node.ordering(structure), value = T)
  nodes_pa <- lapply(nodes_0, grep, pattern="t_0", value=T)
  nodes_pa <- lapply(nodes_pa, parents, x = structure)
  nodes_pa <- lapply(nodes_pa, grep, pattern = "t_0", value = T)
  orphans <- which(unlist(lapply(nodes_pa, length)) == 0)

  nodes_0 <- c(nodes_0[orphans], nodes_0[-orphans])

  return(nodes_0)
}

expand_time_nodes <- function(n, acc, max, i){
  if(i == max)
    return(acc)

  return(expand_time_nodes(n, c(sapply(n,sub, pattern = "_t_0",
                                       replacement = paste0("_t_",i),
                                       USE.NAMES = F),acc), max, i+1))
}

plot_dynamic_network <- function(structure, offset = 200){
  nodes_uniq <- dynamic_ordering(structure)
  levels <- node_levels(structure, nodes_uniq)
  positions <- acc_succesion(levels)
  n_nodes_slice <- length(nodes_uniq)

  max_consec <- max(positions)
  ord <- length(nodes(structure)) / n_nodes_slice

  nodes_uniq <- expand_time_nodes(nodes_uniq, nodes_uniq, ord, 1)
  all_pos <- NULL

  for(i in (0:(ord-1)))
    all_pos <- c(all_pos, (positions * 100 + i * 100 * max_consec + i * offset))

  #colors <- colorRampPalette(c("blue", "darkgreen"))(ord)
  colors <- sample(colors(distinct = T)[grep('gr(a|e)y|*black|*white',
                                             colors(distinct = T),
                                             invert = T)], ord)
  colors <- col2rgb(colors)/255
  colors <- apply(colors, 2, function(x){do.call(rgb,as.list(x))})

  all_colors <- NULL
  for(c in colors)
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

  edges <- data.frame(from = arcs(structure)[,1],
                      to = arcs(structure)[,2],
                      arrows = "to",
                      #color = "orange",
                      #smooth = TRUE,
                      shadow = FALSE,
                      color = "black")



  ret <- visNetwork(nodes, edges) %>%
    #visHierarchicalLayout(levelSeparation = 100) %>%
    visOptions(highlightNearest = list(enabled = T, hover = T),
               nodesIdSelection = T, ) %>%
    visPhysics(solver = "barnesHut")

  return(ret)
}
