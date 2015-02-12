# Creates a random landscape of length input that sums up to the given budget
rand_int <- function(budget, input){
  a <- sample(0:budget, (input-1), replace=TRUE)
  b <- c(0, sort(a), budget)
  c <- diff(b)
  # Returns the differences between every consecutive term, which sum up to the budget
  return(c)
}
