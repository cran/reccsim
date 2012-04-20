\name{make.controls}
\alias{make.controls}

\title{
  Draw controls from a population at risk given observed cases.
}

\description{
  Generates a random data frame of controls based on the parameters held in
  a \code{PopulationAtRisk} object and a data frame of cases. The latter
  is only used to determine the amount of controls to be drawn.
}

\usage{
make.controls(PopulationAtRisk, cases, ctc, requireAllGroups
= FALSE )
}

\arguments{
  \item{PopulationAtRisk}{
    
  }
  \item{cases}{
    An object returned by \code{\link{make.cases}}
  }
  \item{ctc}{cases-to-controls:
    How many controls shall be drawn for each case (integer)?
  }
  \item{requireAllGroups}{
    Setting this to \code{TRUE} forces controls for all population
    groups, i.e. the drawing of controls is repeated until there are
    controls from all groups (chance for infinite loop).
    }
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{make.cases}}, \code{\link{make.case.control.study}}, \code{\link{rccs}}
}

\examples{
## First you generate a PopulationAtRisk named PaR with
## build.population or interactive.population.
## Then you run
## cases <- make.cases( PaR )
## make.controls( PaR, controls, 5 )
}

\keyword{ utilities }
