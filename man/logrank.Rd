\name{logrank}
\alias{logrank}
\title{
Calculate Logrank or G-rho Tests
}
\description{
function to perform stratified logrank or G-rho tests 
}
\usage{
logrank(time, status, group, strata=NULL, rho = 0, subset=NULL,
        na.action = na.omit, data=NULL)
}
\arguments{
\item{time}{
failure time variable or a formula suitable for \code{survfit}
}
\item{status}{
Failure indicator.  Must be coded 1=failure, 0=censored.
Missing values (\code{NA}s) are allowed.
Must be same length as time.  Ignored if \code{time} is a formula.
}
\item{group}{
treatment (or group) variable
Missing values (\code{NA}s) are allowed.
Must be same length as time.
}
\item{strata}{
Stratification variable.  If missing, unstratified tests
will be used.  
Missing values (\code{NA}s) are allowed.
If given, must be same length as time.
}
\item{rho}{
Specifies the value of rho in the G-rho test (Harrington and Fleming, 1982).
rho=0 gives the logrank test, and rho=1
the Peto-Peto Wilcoxon test (and rho=-1 the test discussed by Gray and
Tsiatis, 1989).
}
\item{subset}{
a logical vector specifying a subset of cases to include in the
analysis
}
\item{na.action}{
a function specifying the action to take for any cases missing any of
time, status, group, pv.strat, pv.sub, or subset.
}
\item{data}{ A data frame.  Only used if \code{time} is a formula, in
  which case the variables are interpreted in the context of \code{data}.
}
}
\details{
  Calls \code{survdiff} to perform the test.

  Since more than 2 groups are allowed, the test statistic is chi-square
  version, and the p-values are 2-sided.

  If \code{$var} is not invertible, then \code{$stat} will be set = -1.
Cases with missing values in any of time, status, group, or strata will be
excluded.
}
\value{
  Returns a list, with components
  \item{stat}{the value of the test statistic (chi-square quadratic form)}
  \item{pv}{the p-value using a chi-square approximation to the
distribution of the statistic} 
  \item{df}{the degrees of freedom of the test} 
  \item{oe}{a matrix of dimension no. treatments x 3, with the first
    column the observed no. deaths, the 2nd column the expected no. of
    deaths, and the 3rd column the total no. of subjects, on each
    treatment}
  \item{score}{the first (no. treatments - 1) components
    of the score vector}
  \item{var}{the estimated variance-covariance matrix
    of \code{score}}
}
\references{
Harrington DP and Fleming TR (1982) A class of rank test procedures for
censored survival data, BIOMETRIKA, 69:553--566.


Gray RJ and Tsiatis AA (1989) A linear rank test for use when the main
interest is in differences in cure rates, BIOMTRICS, 45:899--904.
}
\author{Bob Gray}
\seealso{
\code{\link{survdiff}}
}
\examples{
set.seed(20)
rx <- c(rep('A',50),rep('B',50))
ftime <- rexp(length(rx),rate=ifelse(rx=='A',.2,.1))
ctime <- runif(length(rx))*4+3
find <- ifelse(ftime<=ctime,1,0)
ftime <- pmin(ftime,ctime)
z <- logrank(ftime,find,rx)
}
\keyword{survival}
% Converted by Sd2Rd version 1.21.
