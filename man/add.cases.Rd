\name{add.cases}
\alias{add.cases}

\title{
  Computes how many additional cases a group generates.
}

\description{
  Given a population risk and size, a population distribution across groups and
  relative group risks \code{add.cases} tells you how many additional
  cases a certain group generates compared with a comparison group.
}

\usage{
add.cases(pop.size, pop.risk, pop.percentages, relative.risks)
}

\arguments{
  \item{pop.size}{
    An integer giving the population size.
  }
  \item{pop.risk}{
    A probability giving the overall probability across the whole population.
  }
  \item{pop.percentages}{
    The population distribution across some groups as in \code{\link{build.population}}.
  }
  \item{relative.risks}{
    The group's relative risks as in \code{\link{build.population}}.
  }
}

\author{
  Christian Westphal
}

\seealso{
  \code{\link{build.population}}
}

\examples{
## There ist a population of 50 million. Overall risk for a population
## member of becoming a case is 0.0001.

## There are four groups: Two groups each making up 20 percent of the
## population and one group making up 10 percent of the population.
## The fourth group is the baseline group with a relative risk of one
## and its 50 percent of the population are calculated automatically
## from the other group's percentages.

## The 20 percent groups are attributed with a relative risk of 2 and 5
## and the 10 percent group has a relative risk of 10.

## now add.cases will tell you, how many cases could be prevented if you
## were able to lower the overall risk by removing risk factors in the
## groups and thereby reducing a groups relative risk to one.

add.cases( 50000000, .0001, c(.2,.2,.1), c(2,5,10) )
}

\keyword{ utilities }

