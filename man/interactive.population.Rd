\name{interactive.population}
\alias{interactive.population}

\title{
  Build a population at risk interactively.
}
\description{
  Helps you build a population at risk on at console.
}
\usage{
interactive.population(formula)
}

\arguments{
  \item{formula}{
    The formula has to describe your risk model, i.e. some outcome, say
    \code{Y}, has to depend on some exogenous factors, say
    \code{A,B}. Then formulas like \code{Y~A+B, Y~A*B, Y~A+A:B} all are
    valid risk models.
  }
}
\details{
  \code{interactive.population} is intended to teach the user about the
  creation of the \code{PopulationAtRisk} object used by \code{rccs} to
  generate random case-control studies. Usually for automatically trying
  different population parameters you will want to use the
  non-interactive function \code{build.population} to create a
  \code{PopulationAtRisk} object.
}
\value{
  An object of class \code{PopulationAtRisk}.
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{build.population}}, \code{\link{rccs}}
}
\examples{
## Build a PopulationAtRisk with risk factors
## beer and steak and outcome cancer

## PaR <- interactive.population( cancer ~ beer + steak )
}

\keyword{ datagen }

