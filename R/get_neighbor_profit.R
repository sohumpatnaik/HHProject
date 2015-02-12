# Gets the profit values of all neighboring points
get_neighbor_profit <- function(input, neighbors, clin, csq, ccr){

  prdf <- NULL
  for (i in 1:length(neighbors)){
    # Finds the profit value of a neighbor to a given point
    pr <- find_profit(neighbors[,i], input, clin, csq, ccr)
    # Binds the found profit value to the rest of the profit values of the neighbors
    prdf <-cbind(prdf, pr)
  }

  return(prdf)

}
