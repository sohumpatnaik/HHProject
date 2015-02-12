# The main function that climbs a landscape based off given conditions and returns the
# final profit level reached and a count of how many steps it took to reach that
# profit level based off the strategy it used.
climbHill <- function(budget=50, input=20, type="max", connection=1, cycles=1,
                      clinl=-1, clinu=1, csql=-1, csqu=1, ccrl=-1, ccru=1){

  options(warn=-1)
  results <- NULL

  # Repeats the program for the specified amount of cycles
  for(i in 1:cycles){

    # Generates the coefficients for linear terms, squared terms, and cross products
    clin <- runif(input, clinl, clinu)
    csq <- runif(input, csql, csqu)
    ccr <- runif(sum(1:(input-1)), ccrl, ccru)

    # Generates a random landscape based off the budget and input
    ln <- rand_int(budget, input)
    lnoriginal <- ln
    neighbpr <- NULL
    count=0
    p1 <- find_profit(ln, input, clin, csq, ccr)

    # Strategy that moves to the neighbor with the highest profit
    if (type=="max"){
      while (p1!=max(neighbpr)){
        # Finds the neighbors of the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Changes the current input vector to the neighbor with the highest profit
        maxneighbindex <- which(neighbpr==max(neighbpr))
        ln <- neighb[,maxneighbindex]
        count <- count + 1
      }
      count <- count - 1
    }

    # Strategy that moves to the neighbor with the lowest profit that is still higher than
    # the current input vector's profit
    minpr <- 0
    if (type=="min"){
      while (p1!=minpr){
        # Finds the neighbors of the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Changes the current input vector to the neighbor with the lowest profit of the neighbors
        # with higher profits than the current input vector
        if (TRUE %in% (neighbpr>p1)){
          minpr <- min(neighbpr[which(neighbpr>p1)])
          minneighbindex <- which(neighbpr==minpr)
          ln <- neighb[,minneighbindex]
        } else {
          minpr <- p1
        }
        count <- count + 1
      }
      count <- count - 1
    }

    # Strategy that moves to the neighbor whose profits are the median of the neighbors
    # with profits higher than the current input vector
    medpr <- 0
    if (type=="med"){
      while (p1!=medpr){
        # Finds the neighbors to the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Changes the current input vector to the neighbor with whose profit is the median of the neighbors
        # with higher profits than the current profit

        # If there's a tie or the median profit is the average of two different profits, it takes
        # the first median input vector found
        if (TRUE %in% (neighbpr>p1)){
          medpr <- median(neighbpr[which(neighbpr>p1)])
          tiedindex <- which(abs(neighbpr-medpr)==min(abs(neighbpr-medpr)))
          ln <- neighb[,tiedindex[1]]
        } else {
          medpr <- p1
        }
        count <- count + 1
      }
      count <- count - 1
    }

    # Strategy that carries out the Stochastic Hill Climbing strategy where a input vector moves to a
    # randomly chosen neighbor whose profit is higher than the current input vector's profit
    randpr <- 0
    if (type=="rand"){
      while (p1!=randpr){
        # Finds the neighbors to the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Finds which neighbors have higher profits than the current input vector and randomly
        # chooses one of those input vectors to move to
        if (TRUE %in% (neighbpr>p1)){
          imprindex <- which(neighbpr>p1)
          randind <- imprindex[sample(1:length(imprindex),1)]
          randpr <- neighbpr[randind]
          ln <- neighb[,randind]
        } else {
          randpr <- p1
        }
        count <- count + 1
      }
      count <- count - 1
    }

    # Strategy that moves to the neighboring input vectors with the highest profits until a single
    # point of the input vector has all of budget or until the process is repeated one thousand times
    if (type=="fiftymax"){
      repeat{
        # Finds the neighbors to the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Moves to the neighboring input vectors with the highest profit level
        maxneighbindex <- which(neighbpr==max(neighbpr[2:length(neighbpr)]))
        ln <- neighb[,maxneighbindex]
        count=count+1

        # Repeats this process until one of the points has all of the budget or until
        # the process is repeated 1000 times
        if((budget %in% ln)||(count==1000)){
          break
        }
      }
      p1 <- find_profit(ln, input, clin, csq, ccr)
    }


    # Strategy that moves to a neighboring input vector with the lowest profit that is still
    # higher than the profit of the current input vector until a single point of the input
    # vector has all of budget, until the process is repeated one thousand times

    # If a local maximum is reached and no single point has all of the budget, then the strategy
    # moves to the neighboring input vector with the highest profit value
    if (type=="fiftymin"){
      repeat{
        # Finds the neighbors to the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        if (TRUE %in% (neighbpr>p1)){
          # Moves to the neighbor with the lowest profit level which is still higher than the current
          # input vector
          minpr <- min(neighbpr[which(neighbpr>p1)])
          minneighbindex <- which(neighbpr==minpr)
          ln <- neighb[,minneighbindex]
        } else {
          # If a local maximum is reached and no single point has all of the budget, the strategy
          # moves to the neighboring input vector with the highest profit level
          maxneighbindex <- which(neighbpr==max(neighbpr[2:length(neighbpr)]))
          ln <- neighb[,maxneighbindex]
        }
        count=count+1

        # The process is repeated until all of the budget is in a single point or until
        # the process is repeated one thousand times
        if((budget %in% ln)||(count==1000)){
          break
        }
      }
      p1 <- find_profit(ln, input, clin, csq, ccr)
    }

    # Strategy that moves to a neighboring input vector which has a median profit level of the
    # neighboring input levels that have higher profit levels than the current input vector

    # If a local maximum is reached and no single point has all of the budget, then the strategy
    # moves to the neighboring input vector with the highest profit value
    if (type=="fiftymed"){
      repeat{
        # Finds the neighbors to the current input vector and their profits
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        if (TRUE %in% (neighbpr>p1)){
          # Moves to the neighbor with the median profit level of the neighboring input vectors
          # with profit levels higher than the current input vector's profit level
          medpr <- median(neighbpr[which(neighbpr>p1)])
          tiedindex <- which(abs(neighbpr-medpr)==min(abs(neighbpr-medpr)))
          ln <- neighb[,tiedindex[1]]
        } else {
          # If a local maximum is reached and no single point has all of the budget, the strategy
          # moves to the neighboring input vector with the highest profit level
          maxneighbindex <- which(neighbpr==max(neighbpr[2:length(neighbpr)]))
          ln <- neighb[,maxneighbindex]
        }
        count=count+1

        # The process is repeated until all of the budget is in a single point or until
        # the process is repeated one thousand times
        if((budget %in% ln)||(count==1000)){
          break
        }
      }
      p1 <- find_profit(ln, input, clin, csq, ccr)
    }

    # Strategy that classifies all neighbors of neighbors of a given input vector as one of its
    # own neighbors and then moves to the neighbor with the highest profit level
    if (type=="maxnn"){
      repeat{
        # Finds the neighbors to the current input vector and their neighbors
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)

        a <- 1:input
        for(i in 2:length(neighb)){
          b <- as.data.frame(neighb[,i])
          b <- find_neighbors(neighb[,i], input, connection)
          a <- cbind(a,b)
        }
        neighb <- a[,2:length(a)]

        # Gets the profits of the neighbors and the neighbors' neighbors
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Finds maximum profit value of the neighbors and their neighbors
        maxneighbindex <- which(neighbpr==max(neighbpr))
        ln <- neighb[,maxneighbindex]

        if(class(ln)=="data.frame"){
          ln <- ln[1]
          ln <- as.numeric(ln[,1])
        }
        count = count+1

        # Process is repeated until all of the budget is in a particular point,
        # until the process is repeated five hundred times, or until a local maximum is reached
        if((budget %in% ln)||(count==500)||(p1==max(neighbpr))){
          break
        }
      }
      p1 <- find_profit(ln, input, clin, csq, ccr)
    }

    # Strategy that classifies all neighbors of neighbors of a given input vector as one of its
    # own neighbors and then moves to the neighbor with the lowest profit level that is still
    # higher than the profit level of the current input vector
    if (type=="minnn"){
      repeat{
        # Finds the neighbors to the current input vector and their neighbors
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)

        a <- 1:input
        for(i in 2:length(neighb)){

          b <- as.data.frame(neighb[,i])

          b <- find_neighbors(neighb[,i], input, connection)
          a <- cbind(a,b)
        }
        neighb <- a[,2:length(a)]

        # Gets the profits of the neighbors and the neighbors' neighbors
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Finds min value of the profits of the neighbors and their neighbors which is still
        # higher than the profit value of the current input vector
        if (TRUE %in% (neighbpr>p1)){
          minpr <- min(neighbpr[which(neighbpr>p1)])
          minneighbindex <- which(neighbpr==minpr)
          ln <- neighb[,minneighbindex]
        } else {
          maxneighbindex <- which(neighbpr==max(neighbpr[2:length(neighbpr)]))
          ln <- neighb[,maxneighbindex]
        }

        if(class(ln)=="data.frame"){
          ln <- ln[1]
          ln <- as.numeric(ln[,1])
        }
        count = count+1

        # Process is repeated until all of the budget is in a particular point,
        # until the process is repeated five hundred times, or until a local maximum is reached
        if((budget %in% ln)||(count==500)||(p1==minpr)){
          break
        }
      }
      p1 <- find_profit(ln, input, clin, csq, ccr)
    }

    # Strategy that classifies all neighbors of neighbors of a given input vector as one of its
    # own neighbors and then moves to the neighbor with the median profit level of the neighbors
    # with profit levels higher than the profit level of the current input
    if (type=="mednn"){
      repeat{
        # Finds the neighbors to the current input vector and their neighbors
        p1 <- find_profit(ln, input, clin, csq, ccr)
        neighb <- find_neighbors(ln, input, connection)

        a <- 1:input
        for(i in 2:length(neighb)){

          b <- as.data.frame(neighb[,i])

          b <- find_neighbors(neighb[,i], input, connection)
          a <- cbind(a,b)
        }
        neighb <- a[,2:length(a)]

        # Gets the profits of the neighbors and the neighbors' neighbors
        neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)

        # Finds median profit value of the neighbors' profits which are still higher than
        # the current input vector's profit level, and then moves to that input vector
        if (TRUE %in% (neighbpr>p1)){
          medpr <- median(neighbpr[which(neighbpr>p1)])
          tiedindex <- which(abs(neighbpr-medpr)==min(abs(neighbpr-medpr)))
          ln <- neighb[,tiedindex[1]]
        } else {
          maxneighbindex <- which(neighbpr==max(neighbpr[2:length(neighbpr)]))
          ln <- neighb[,maxneighbindex]
        }

        if(class(ln)=="data.frame"){
          ln <- ln[1]
          ln <- as.numeric(ln[,1])
        }
        count = count+1

        # Process is repeated until all of the budget is in a particular point,
        # until the process is repeated five hundred times, or until a local maximum is reached
        if((budget %in% ln)||(count==500)||(p1==medpr)){
          break
        }
      }
      p1 <- find_profit(ln, input, clin, csq, ccr)
    }


    # A Simulated Annealing Variant Strategy that allows for an input vector to move in a direction
    # which would lower profit levels in hopes that this could ultimately come closer to finding
    # the global maximum of a landcape with a rugged terrain
    temp = 1.0
    temp_min = 0.001
    alpha = 0.9

    if (type=="simann"){
      while(temp>temp_min){

        aprob <- 0
        randnum <- 0
        inc <- 0

        repeat{
          # Finds the neighbors to the given input vector and picks a random one
          neighb <- find_neighbors(ln, input, connection)
          neighbpr <- get_neighbor_profit(input, neighb, clin, csq, ccr)
          randneighbprind <- sample(2:length(neighbpr), 1)
          randneighbpr <- neighbpr[randneighbprind]

          # Calculates the acceptance probability
          calcdiff <- randneighbpr - p1
          aprob <- exp(1)^(calcdiff/temp)
          randnum <- runif(1, 0, 1)
          inc=inc+1

          # If the calculated acceptance probability is greater than the random number generated
          # or if the process has repeated more than fifty times,then the strategy moves to
          # the random neighbor that was chosen
          if(inc>=100){
            randneighbpr <- max(neighbpr)
            randneighbprind <- which(neighbpr==randneighbpr)
          }
          if((aprob>randnum)||(inc>=50)){
            break
          }
        }

        ln <- neighb[,randneighbprind]
        p1 <- find_profit(ln, input, clin, csq, ccr)
        count=count+1

        temp = temp*alpha
      }
    }

    # Normalizes the profit value
    normp1 <- p1/(budget + (budget^2))

    prc <- c(normp1, count)
    results <- rbind(results,prc)
  }
  # Returns the profit value reached and the count
  return(results)
}
