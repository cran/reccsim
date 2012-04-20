\name{risk.difference}
\alias{risk.difference}

\title{
  Computes the absolute risk difference between groups.
}

\description{
  Given a known population risk, a known population distribution across
  groups and known relative risks for these groups,
  \code{risk.difference} computes the absolute risk difference
  (probability differences) between the groups and a comparison groups
  with relative risk = 1.
}

\usage{
risk.difference(pop.risk, pop.percentages, relative.risks)
}

\arguments{
  \item{pop.risk}{
    As in \code{\link{build.population}}.
  }
  \item{pop.percentages}{
    As in \code{\link{build.population}}.
  }
  \item{relative.risks}{
    As in \code{\link{build.population}}.
  }
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{build.population}}
}

\examples{
risk.difference( .0001, c(.2,.2,.1), c(2,5,10) )
}

\keyword{ utilities }

