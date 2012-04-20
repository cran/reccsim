\name{build.population}
\alias{build.population}

\title{
  Builds a PopulationAtRisk object.
}

\description{
  Given a known set of population parameters, \code{build.population}
  builds a PopulationAtRisk object which may be used in \code{rccs} for
  simulating a case control study.
}

\usage{
build.population(formula, pop.size, pop.risk, pop.percentages, relative.risks)
}

\arguments{
  \item{formula}{
    A \code{formula} object describing the dependent binary variable (whatever
    the population is at risk for) and the independent variables
    (whatever is supsected/may influence the risk).
  }
  \item{pop.size}{
    The overall size of the population.
  }
  \item{pop.risk}{
    The overall population risk. I.e. the probability a random subject
    will exhibit the endogenous binary variable.
  }
  \item{pop.percentages}{
    The percentages of the risk groups (vector), i.e. the groups having any of
    the risk influencing factors. The percentage of the zero group
    (i.e. no factors) will be automatically computed.
  }
  \item{relative.risks}{
    The relative risks (vector) among the risk groups. Relative risk for the zero
    group is automatically set to one.
  }
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{interactive.population}}, \code{\link{rccs}}
}

\examples{
## We do have a population of 50 million people. Some (20\%) consume steak,
## some (20\%) consume beer, some (10\%) consume both. Some consume neither.
## Those consuming steak do have twice the risk of getting cancer. Those
## who drink beer do have five times the risk of getting cancer. Those
## consuming both do have ten times (the product, which is approximately
## the equivalent to 'no interaction' for rare events) of getting cancer.

PaR <- build.population( cancer ~ steak + beer,
                         50000000,
                         .0001,
                         c(.2,.2,.1),
                         c(2,5,10)
                        )

PaR

}

\keyword{ utilities }

