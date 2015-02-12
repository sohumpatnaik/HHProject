# Finds the profit of a value on a given landscape
find_profit <- function(ln, input, clin, csq, ccr){

  # Finds the sum of the linear terms
  slin <- sum(clin*ln)
  lnsq <- ln^2
  ssq <- sum(csq*lnsq)

  # Finds the sum of the squared terms
  t <- c()
  for(f in 1:(input-1)){
    for (s in (f+1):input){
      t<-c(t,(ln[f]*ln[s]))
    }
  }
  scr <- sum(ccr*t)

  # Finds the sum of the cross products
  finsum <- sum(slin,ssq,scr)

  return(finsum)
}
