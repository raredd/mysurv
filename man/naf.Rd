\name{naf}
\alias{naf}
\title{Replace NAs}
\description{
 Given a vector x, replaces NA values with a specified value
}
\usage{
naf(x,val=FALSE)
}
\arguments{
  \item{x}{ A vector}
  \item{val}{ Value to substitute for NAs}
}
\details{
  This function does not check the mode or type of the argument.  For
  logical vectors, NAs are replaced with FALSE by default (this is the
  primary intended application, so NAs will be excluded from a subset).
  For numeric vectors, NAs are replaced with 0 by default.  For
  character vectors, NAs are replaced with "FALSE" by default.  For
  factors, usually no replacement is done and a warning is printed. 
}
\value{
  A vector like x with NAs replaced by \code{val}.
}
\seealso{
  \code{\link{subset}}}
\examples{
naf(c(TRUE,TRUE,NA,NA))
naf(c(1,1,NA,NA))
naf(c('a','a',NA,NA))
}
\keyword{manip}% at least one, from doc/KEYWORDS
