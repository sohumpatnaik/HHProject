\name{rand_int}
\alias{rand_int}
\title{
Create a Random Landscape
}
\description{
Creates a random landscape of length input that sums up to the given budget.
}
\usage{
rand_int(budget, input)
}

\arguments{
  \item{budget}{
This determines the sum of the numeric vector that is returned. It represents the budget of the firm.
}
  \item{input}{
This determines the length of the numeric vector returned. It represents the number of inputs a firm
has.
}
}

\value{
A numeric vector is returned of length input and sums up to the budget.
}

\author{
Sohum Patnaik
}


\examples{
rand_int(budget = 50, input = 20)
}

\keyword{ landscape }

