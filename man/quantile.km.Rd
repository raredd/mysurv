\name{quantile.km}
\alias{quantile.km}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate quantiles of survival curves }
\description{
  Computes the median or other specified quantile of the survival curves
  output by km.
}
\usage{
\method{quantile}{km}(x, q = 0.5, ...)
}
\arguments{
  \item{x}{An object of class km (output from the km function) }
  \item{q}{Determines the estimated time \code{t} such that a proportion
    \code{q} of the population have survival times greater than \code{t}}
  \item{...}{Not currently used}
}
\details{
  The quantile is estimated as the point where the Kaplan-Meier step
  function estimate crosses \code{q}.  If the curve is exactly equal to
  q on a step, then the estimated quantile is at \code{1-q} of the
  distance from the min to the max point where the curve equals \code{q}.
}
\value{
  A vector giving the estimated quantile for each of the curves in
  \code{x}. 
}

\author{Bob Gray }

\seealso{\code{\link{km}}}
\examples{
set.seed(20)
rx <- c(rep('A',50),rep('B',50))
ftime <- rexp(length(rx),rate=ifelse(rx=='A',.2,.1))
ctime <- runif(length(rx))*4+3
find <- ifelse(ftime<=ctime,1,0)
ftime <- pmin(ftime,ctime)
z <- km(ftime,find,rx)
quantile.km(z)
}
\keyword{survival}
