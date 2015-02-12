\name{find_profit}
\alias{find_profit}

\title{
Find Profit
}
\description{
Finds the profit level of a given input vector.
}
\usage{
find_profit(ln, input, clin, csq, ccr)
}

\arguments{
  \item{ln}{
The current input vector that find_profit finds the profit of.
}
  \item{input}{
This determines the length of the numeric vector returned. It represents the number of inputs a firm
has.
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
A numeric value is returned that gives the profit level of the current input vector
}
\references{
Kane, David. Local Hillclimbing on an Economic Landscape. \emph{Evolutionary Programming V}. Santa Fe Institute, 1996.
\emph{http://www.santafe.edu/media/workingpapers/96-08-065.pdf}
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
p1 <- find_profit(ln, input, clin, csq, ccr)
}

\keyword{ profit }

