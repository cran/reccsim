\name{get.main}
\alias{get.main}

\title{
  Get main effects from an R \code{formula} object.
}

\description{
  It will return all main effects from the \code{formula} (i.e. \code{y~a*b} expands
  to \code{y~ a+b+a:b}, main effects are \code{a} and \code{b}) as a character vector.
}

\usage{
get.main(formula)
}

\arguments{
  \item{formula}{
    An R \code{formula} with operators "+", "*" , ":". Conditioning "|" is not implemented.
  }
}

\value{
  An object of class \code{vector(mode="character", n)} where \code{n}
  is the number of main effects in the \code{formula}.}

\author{
  Christian Westphal
}

\examples{
get.main( cancer ~ smoking + drinking + smoking:drinking )
## will return:
## "smoking", "drinking"
}

\keyword{ utilities }

