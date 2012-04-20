\name{absolute.risk}
\alias{absolute.risk}

\title{ Computes absolute risks from relative risks.
}

\description{ \code{absolute.risk} computes the absolute risks
  (probabilities) for different groups from relative risks and a population probability.
}

\usage{
absolute.risk(pop.risk, pop.percentages, relative.risks)
}

\arguments{
  \item{pop.risk}{
    This is the probability of an event over the complete population.
  }
  \item{pop.percentages}{
    This is a vector of population percentages as in \code{\link{build.population}}.
  }
  \item{relative.risks}{
    This is a vector of relative risks as in \code{\link{build.population}}.
  }
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{build.population}}
}

\examples{
## This will tell you the risks in percentages for a baseline group
## (relative risk of one)
## and the three groups making up 20%, 20% and 10% of the population
## with relative risks of 2, 5 and 10.

absolute.risk( .0001, c(.2,.2,.1), c(2,5,10) )
}

\keyword{ utilities }
