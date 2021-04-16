\name{tabp}
\alias{tabp}
\title{
Create formated tables
}
\description{
Creates two-way cross tabulations with counts and column percents
}
\usage{
tabp(x, y, dec = 1)
}

\arguments{
  \item{x}{
The row factor (characteristic) 
}
  \item{y}{
The column factor (population)
}
  \item{dec}{
Number of decimal digits in the percents
}
}
\details{
Creates a cross tabulation of \code{x} by \code{y} and appends an
  additional column on the right giving the marginal tabulation of
  \code{x}.  The percents within each column are also computed.
  \code{NA} values in \code{x} are excluded from the computation of
  percents (that is, they are not included in the denominator), and then
  are reported as counts in a separate row.  Case with missing values
  for \code{y} are excluded.  \code{x} and \code{y} can
  be any type of variable that can be used as an argument to
  \code{table}.  The counts, the percents and a character array giving
  the counts with the percents in parentheses are returned.
}
\value{
A list with three unnamed components, with the first giving the numeric
  matrix of counts, the second giving the numeric matrix of percents
  (with counts for the unknowns, if present) and the third givng the
  character strings with both the counts and the percents in parentheses.
}
\author{
Bob Gray
}
\examples{
set.seed(1)
x=sample(1:4,100,replace=TRUE)
x=ifelse(x==4,NA,x)
y=sample(c('A','B'),100,replace=TRUE)
tabp(x,y)
}
\keyword{misc}
