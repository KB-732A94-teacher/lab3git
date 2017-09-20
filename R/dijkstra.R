# hyungyum kim
# hyuki392

#1.1.2

#' Applying Dijkstra algorithm to a given graph and initial node
#' 
#' @param graph A data.frame that contains 3 vectors named v1, v2, w
#' @param init_node Numeric representation of initial node
#' @return Solution of the Dijkstra algorithm from the given initial node
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' @export dijkstra

dijkstra <- function(graph, init_node){
  
  stopifnot(is.data.frame(graph) & is.numeric(init_node))
  
  dist_mat <- matrix(Inf, max(graph$v1), max(graph$v1))
  
  num_of_nodes <- max(graph$v1)
  num_of_edges <- length(graph$v1)
  
  dest <- c(rep(0,num_of_nodes))
  mark <- c(rep(0,num_of_nodes))
  
  #making a distance matrix by the given data.frame
  for(i in 1:length(graph$v1)){
    dist_mat[graph$v1[i],graph$v2[i]] = graph$w[i]
  }
  
  #making a distance vector from the initial node to other nodes
  for (i in 1:num_of_nodes){
    dest[i] = dist_mat[init_node,i]
  }
  
  count=1
  
  #maximum iteration is the number of edges
  while(count <= num_of_edges){
    stand_num = Inf
    
    for(w in 1:num_of_nodes){
      #calculation to direct nodes
      if(dest[w] < stand_num && mark[w]==0){
        stand_num=dest[w]
        u=w
      }
    }
    
    mark[u] = 1 #mark as visited
    count = count+1
    
    #calculation to indirect nodes and updates
    for(w in 1:num_of_nodes){
      if((dest[u]+dist_mat[u,w] < dest[w]) && mark[w]==0){
        dest[w]=dest[u]+dist_mat[u,w]
      }
    }
  }
  
  #initial node to initial node is zero, otherwise it goes to nearest node and comeback
  dest[init_node] = 0
  
  return(dest)
}

