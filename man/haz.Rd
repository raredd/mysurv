\name{haz}
\alias{haz}
\title{
Kernel Smoothing of Hazard Fucntions
}
\description{
calculate kernel estimates of hazard functions
}
\usage{
haz(time, status, group, span, np = 100, lower = 0, upper, 
    subset, na.action = na.omit)
}
\arguments{
\item{time}{
Failure time variable
Missing values (\code{NA}s) are allowed.
}
\item{status}{
Failure indicator (1=failure, 0=censored)
Missing values (\code{NA}s) are allowed.
Must be same length as time.
}
\item{group}{
Group variable.  Hazards estimated separately for each group (distinct
value of \code{group}).
Missing values (\code{NA}s) are allowed.
If missing, the hazard in the overall cohort is estimated.
If given, must be same length as time.
}
\item{span}{
Span size for kernel.  \code{span} is defined in terms of the absolute
distance on the time axis.  For the estimate at \code{t}, \code{haz}
uses a window from t-span to t+span.  Default is \code{(upper-lower)/4},
which is arbitraty and probably not appropriate for many situations.
}
\item{np}{
\# points where hazard is estimated (will be equally spaced)
}
\item{lower}{
Lower limit of times where estimates calculated
}
\item{upper}{
  Upper limit of times where estimates calculated (default is \code{max(y)})
}
\item{subset}{
a logical vector specifying a subset of cases to include in the
analysis
}
\item{na.action}{
A function specifying the action to take for any cases missing any of
\code{time}, \code{status}, \code{group}, or \code{subset}.
}
}
\details{
  This function uses simple kernel smoothing with fixed length span, see
i.e. Ramlau-Hansen (1983).  A bi-weight kernel function, proportional to
\code{(1-u^2)^2}, is used.  Estimates can be plotted using
function \code{plot.cuminc}, for example.  With the default
\code{na.action}, cases with missing values for
any of the variables \code{time}, \code{status}, \code{group} or
\code{subset} are excluded. 
  }
\value{
A list with number of components equal to the number of groups.  Each
component is a list with three components, \code{$time} giving times
where estimates are calculated, \code{$est} giving the hazard estimates,
and \code{$var} giving variances. 
}
\references{
Ramlau-Hansen H (1983) Smoothing counting process intensities by means of
kernel functions, ANNALS OF STATISTICS 11:453--466.
}
\author{ Bob Gray}
\examples{
set.seed(20)
rx <- c(rep('A',50),rep('B',50))
ftime <- rexp(length(rx),rate=ifelse(rx=='A',.2,.1))
ctime <- runif(length(rx))*4+3
find <- ifelse(ftime<=ctime,1,0)
ftime <- pmin(ftime,ctime)
z <- haz(ftime,find,rx)
}
\keyword{survival}
\keyword{smooth}
% Converted by Sd2Rd version 1.21.
