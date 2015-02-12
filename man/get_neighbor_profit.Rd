\name{get_neighbor_profit}
\alias{get_neighbor_profit}
\title{
Get Neighbors' Profits
}
\description{
Gets the profit levels of all neighboring points to a given input vector
}
\usage{
get_neighbor_profit(input, neighbors, clin, csq, ccr)
}

\arguments{
  \item{input}{
This determines the length of the numeric vector returned. It represents the number of inputs a firm
has.
}
  \item{neighbors}{
The neighbors of a given input vector.
}
  \item{clin}{
Coefficients for the the linear terms of the profit function.
}
  \item{csq}{
Coefficients for the the squared terms of the profit function.
}
  \item{ccr}{
Coefficients for the the cross products of the profit function.
}
}

\value{
Returns a matrix of the profit levels of the neighbors of a given input vector.
}

\author{
Sohum Patnaik
}

\examples{
budget = 50
input = 20
clin <- runif(input, -1, 1)
csq <- runif(input, -1, 1)
ccr <- runif(sum(1:(input-1)), -1, 1)
ln <- rand_int(budget, input)
neighb <- find_neighbors(ln, input, connection = 1)
neighbpr <- get_neighbor_profit(input, neighbors = neighb, clin, csq, ccr)
}

\keyword{ profits }

