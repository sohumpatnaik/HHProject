# Finds the nieghboring points of a given point on a given landscape
find_neighbors <- function(ln, input, connection=1){

  # Stores the original given point in the first column of the data frame
  a <- as.data.frame(ln)
  names(a)[1] <- "original"


  for(concount in 1:connection){
    # Neighbors are found by subtracting 1 from a particular value and adding 1 to another value.
    # The connection determines how many points a particular value in a landscape can add
    # its subtracted value to.
    for (i in 1:input){
      b <- a$original
      if(b[i]>0){
        b[i] <- b[i]-1
        nindex <- (i+concount)%%input
        if(nindex==0){
          nindex <- input
        }
        b[nindex] <- b[nindex]+1
        a <- cbind(a, b)
        names(a)[length(a)] <- i
      }
    }
  }
  # Returns a data frame with all neighboring points to ln
  return(a)
}
