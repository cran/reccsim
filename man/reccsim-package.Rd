\name{reccsim-package}
\alias{reccsim-package}
\alias{reccsim}
\docType{package}
\title{
  Simulate case-control studies.
}
\description{
  This package allows you to simulate case control-studies from a known
  population with binary exogenous variables and a binary endogenous variable.
}
\details{
  \tabular{ll}{
    Package: \tab reccsim\cr
    Type: \tab Package\cr
    Version: \tab 0.9-1\cr
    Date: \tab 2012-09-28\cr
    License: \tab GPL (>=2)\cr
  }
  reccsim's functions are \code{\link{rccs}} and \code{\link{build.population}} as
  main workhorses and \code{\link{interactive.population}}
  to instruct users in how the \code{PopulationAtRisk} object has to be set up.

  For simulating a case-control study you need to feed a
  \code{PopulationAtRisk} to \code{\link{rccs}} (for \code{r}andom
  \code{c}ase-\code{c}ontrol \code{s}tudy). It will then return a case-control
  study which you may use for further analysis.

  \code{\link{interactive.population}} will guide you through the construction
  of a PopulationAtRisk object, however for repeated simulation with
  different population parameters you will usually want to call
  \code{\link{build.population}} directly under specification of your
  population parameters.
}
\author{
  Christian Westphal
  
  Maintainer: Christian Westphal <westphal@staff.uni-marburg.de>
}

\references{
  Breslow, N.E. (1996) \emph{Statistics in Epidemiology: The
    Case-Control Study}. Journal of the American Statistical
  Association, Vol. 91 (433) pp. 14-28.
}


\keyword{ package }
\seealso{
  \code{\link{interactive.population}}
}
\examples{
## Create a PopulationAtRisk manually from a risk formula
## where cancer is dependent on smoking and drinking:
## Try this with a population size of 500, 0.2 drinking
## 0.2 smoking and 0.1 smoking and drinking.
## Use 2 and 5 and their product 10 for relative risks in the respective
## groups.

## PaR <- interactive.population( cancer ~ smoking + drinking )

PaR <- build.population( cancer ~ smoking + drinking,
                         50000000,
                         .0001,
                         c(.2,.2,.1),
                         c(2,5,10)
                       )

## Now the PopulationAtRisk object stored in PaR may be used
## to construct a case control study, where we use five times
## as many controls as cases:

ccs <- rccs( PaR, ctc = 5)

## This randomized case control study from the PopulationAtRisk
## is now ready for further analysis.
## Using build.population() instead of interactive.population()
## will allow automatization for studying how for e.g. the logit
## model estimator behaves for different population parameters.

## Let us have a short summary of this cas-control study:

summary( ccs )

}
