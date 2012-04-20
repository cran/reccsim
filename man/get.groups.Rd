\name{get.groups}
\alias{get.groups}

\title{
  Extract population groups from a formula.
}

\description{
  Gets all possible population groups (interactions) from a formula.
}

\usage{
get.groups(formula)
}

\arguments{
  \item{formula}{This is an R formula
  }
}

\author{
  Christian Westphal
}


\examples{
get.groups( cancer ~ smoking + drinking )
## will return the groups
## "smoking", "drinking" and "smoking:drinking"
}

\keyword{ utilities }

