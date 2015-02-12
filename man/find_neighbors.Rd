\name{find_neighbors}
\alias{find_neighbors}
\title{
Find Neighbors
}
\description{
Finds the neighbors of a given input vector.
}
\usage{
find_neighbors(ln, input, connection = 1)
}

\arguments{
  \item{ln}{
The current input vector that find_neighbors finds the neighbors of.
}
  \item{input}{
This determines the length of the numeric vector returned. It represents the number of inputs a firm
has.
}
  \item{connection}{
This determines how many neighboring input vectors a particular input vector has.
}
}

\value{
A data frame is returned which contains all neighboring input vectors to the current input vector.
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
}

\keyword{ neighbors }
