\name{rdsas}
\alias{rdsas}
\title{ Import data from SAS}
\description{
  Creates a data.frame by reading files created by the local SAS macro sas2s.
}
\usage{
rdsas(data = "SOUTD", name = "SOUTN", sep = "\n")
}
\arguments{
  \item{data}{ Data file created by the SAS macro \code{sas2s} }
  \item{name}{ Name file created by the SAS macro \code{sas2s} (contains
  variable names and types)}
  \item{sep}{ Field delimitor in \code{data} }
}
\details{
  The SAS macro \code{sas2s} writes a single value per line.  Generally
  this can only be read in R if the only delimitor used is the end of
  line character.  The type field in \code{name} is coded 1 for numeric
  and 2 for character.  It can sometimes be fooled by SAS formats.
}
\value{
Creates a data frame with one record for each record in the
corresponding SAS data set and one column for each variable in the SAS
data set.  The names should correspond to the SAS variable names.
}
\author{Robert Gray }

\seealso{\code{\link{read.ssd}}}

\examples{
## A directly executable example cannot be given, since files created by
## the SAS macro sas2s are required.
}
\keyword{ IO }% at least one, from doc/KEYWORDS

