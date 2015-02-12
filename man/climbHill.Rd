\name{climbHill}
\alias{climbHill}

\title{
Climb Hill
}
\description{
Climbs the hill of a rugged economic landscape.
}
\usage{
climbHill(budget = 50, input = 20, type = "max", connection = 1, cycles = 1, clinl = -1, clinu = 1, csql = -1, csqu = 1, ccrl = -1, ccru = 1)
}

\arguments{
  \item{budget}{
This represents the budget of a firm. The sum of all the values in the input vector is equal to the budget.
}
  \item{input}{
This represents the number of inputs a firm has. This determines the length of the input vector.
}
  \item{type}{
The strategy that climbHill uses.

"max": Strategy that moves to the neighbor with the highest profit and continues to do so until a local maximum is reached

"med": Strategy that moves to the neighbor whose profits are the median of the neighbors with profits higher than the current input vector and continues to do so until a local maximum is reached

"min": Strategy that moves to the neighbor with the lowest profit that is still higher than the current input vector's profit and continues to do so until a local maximum is reached

"fiftymax": Strategy that moves to the neighboring input vectors with the highest profits until a single point of the input vector has all of the budget or until the process is repeated one thousand times

"fiftymed": Strategy that moves to a neighboring input vector which has a median profit level of the neighboring input levels that have higher profit levels than the current input vector until a single point of the input vector has all of the budget or until the process is repeated one thousand times

"fiftymin": Strategy that moves to a neighboring input vector with the lowest profit that is still higher than the profit of the current input vector until a single point of the input vector has all of the budget or until the process is repeated one thousand times

"rand": Strategy that carries out the Stochastic Hill Climbing strategy where a input vector moves to a randomly chosen neighbor whose profit is higher than the current input vector's profit

"simann": A Simulated Annealing Variant Strategy that allows for an input vector to move in a direction which would lower profit levels in hopes that this could ultimately come closer to finding the global maximum of a landcape with a rugged terrain

"maxnn": Strategy that classifies all neighbors of neighbors of a given input vector as one of its own neighbors and then moves to the neighbor with the highest profit level

"mednn": Strategy that classifies all neighbors of neighbors of a given input vector as one of its own neighbors and then moves to the neighbor with the median profit level of the neighbors with profit levels higher than the profit level of the current input

"minnn": Strategy that classifies all neighbors of neighbors of a given input vector as one of its own neighbors and then moves to the neighbor with the lowest profit level that is still higher than the profit level of the current input vector
}
  \item{connection}{
This determines how many neighboring input vectors a particular input vector has.
}
  \item{cycles}{
The number of times this function is run.
}
  \item{clinl}{
The lower value that coefficients for the the linear terms of the profit function are picked from.
}
  \item{clinu}{
The upper value that coefficients for the the linear terms of the profit function are picked from.
}
  \item{csql}{
The lower value that coefficients for the the squared terms of the profit function are picked from.
}
  \item{csqu}{
The upper value that coefficients for the the squared terms of the profit function are picked from.
}
  \item{ccrl}{
The lower value that coefficients for the the cross products of the profit function are picked from.
}
  \item{ccru}{
The upper value that coefficients for the the cross products of the profit function are picked from.
}
}

\value{
A matrix is returned containing the normalized profit levels reached and the number of steps it took to get to those profit levels.
}
\references{
Kane, David. Local Hillclimbing on an Economic Landscape. \emph{Evolutionary Programming V}. Santa Fe Institute, 1996.
\emph{http://www.santafe.edu/media/workingpapers/96-08-065.pdf}
}
\author{
Sohum Patnaik
}

\examples{
climbHill(type = "max", cycles = 2)

climbHill(type = "simann")

climbHill(type = "maxnn", budget = 40)

climbHill(type = "rand", input = 23)
}

\keyword{ Hillclimbing }

