\name{rccs}
\alias{rccs}

\title{
  Construct a random case-control-study from a population at risk.
}

\description{
  This is a random case control study generator. Given a
  \code{PopulationAtRisk} object and a control factor \code{cf} it will
  create random cases based upon the population at risk and complement
  the cases with \code{cf} times as many random controls from the same
  population. It is \code{reccsim}'s main workhorse.
}

\usage{
rccs(PaR, ctc = 5, requireAllGroups.cases = FALSE,
requireAllGroups.controls = FALSE)
}

\arguments{
  \item{PaR}{
    This is a \code{PopulationAtRisk} object constructed either
    interactively via
    
    \code{interactive.population} or manually from \code{build.population}.
  }
  \item{ctc}{
    This has to be a positive integer telling \code{rccs} how many
    controls you want relative to the cases.
  }
  \item{requireAllGroups.cases}{
    Setting this to \code{TRUE} ensures there will be cases in all
    groups. The random process may very well generate a set of cases
    where not all groups are represented (depending on the
    probabilities). If \code{requireAllGroups.cases} is set to
    \code{TRUE} and this happens, the result is rejected, the simulation
    gets repeated (chance for infinite loop) and a warning is issued.
  }
  \item{requireAllGroups.controls}{
    Setting this to \code{TRUE} ensures there will be controls from all
    groups. The random process may very well draw a set of controls
    where not all groups are represented (depending on the
    probabilities). If \code{requireAllGroups.controls} is set to
    \code{TRUE} and this happens, the result is rejected, the simulation
    gets repeated (chance for infinite loop) and a warning is issued.
  }
}

\details{
  Setting \code{requireAllGroups.cases} and
  \code{requireAllGroups.controls} to \code{TRUE} ensures the existence
  of the MLE in a binary regression setting as described by Silvapulle 1981.
}

\value{
  \item{CaseControlStudy}{is of class \code{CaseControlStudy} (a list
  of length 2) containing the PopulationAtRisk at first and the case
  control study as data.frame at the second position.}
}

\references{
  Silvapulle, Mervyn J. (1981) \emph{On the Existence of Maximum
    Likelihood Estimators for the Binomial Response Models}. Journal of
  the Royal Statistical Society B, Vol. 43 (3) pp. 310-313.
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{summary}},
  \code{\link{build.population}}, \code{\link{interactive.population}}
}

\examples{
## We do have a population of 50 million people. Some (20\%) consume steak,
## some (20\%) consume beer, some (10\%) consume both. Some consume neither.
## Those consuming steak do have twice the risk of getting cancer. Those
## who drink beer do have five times the risk of getting cancer. Those
## consuming both do have ten times (the product, which is approximately
## the equivalent to 'no interaction' for rare events) of getting cancer.

PaR <- build.population( cancer ~ steak + beer, 50000000, .0001,
c(.2,.2,.1), c(2,5,10) )

## This will give a random case control study from the above population
## where you do get five times as many controls as cases.

ccs <- rccs( PaR, ctc = 5 )

summary( ccs )
}

\keyword{ datagen }

