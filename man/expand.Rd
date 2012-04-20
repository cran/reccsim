\name{expand}
\alias{expand}

\title{
  Formula expansion.
}

\description{
  Expands some formula for all possible terms and interaction terms.
  This is function is not intended for the user.
}

\usage{
expand(formula)
}

\arguments{
  \item{formula}{
  }
}


\author{
  Christian Westphal
}

\examples{
expand( cancer ~ smoking + drinking )
## will return the full model:
## cancer ~ smoking * drinking
}

\keyword{ utilities }

