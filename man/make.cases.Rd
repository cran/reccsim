\name{make.cases}
\alias{make.cases}

\title{
  Generate cases from a population at risk.
}

\description{
  Generates a random data frame of cases based on the parameters held in
  a \code{PopulationAtRisk} object.
}

\usage{
make.cases(PopulationAtRisk, requireAllGroups = FALSE)
}

\arguments{
  \item{PopulationAtRisk}{
  }
  \item{requireAllGroups}{
    Setting this to \code{TRUE} forces cases for all population
    groups, i.e. the generation of cases is repeated until there are
    cases from all groups (chance for infinite loop).
  }
}



\author{
  Christian Westphal
}


\seealso{
  \code{\link{make.controls}}, \code{\link{make.case.control.study}},
  \code{\link{rccs}}
}
\examples{
## First you generate a PopulationAtRisk named PaR with
## build.population or interactive.population.
## Then you run
## make.cases( PaR )
}

\keyword{ utilities }
