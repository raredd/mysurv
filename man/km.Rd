\name{km}
\alias{km}
\alias{km2ch}
\alias{km2mat}
\title{
Kaplan-Meier Estimates of Survival Functions
}
\description{
Calculates estimates of survival functions within groups using the method of
Kaplan and Meier.
}
\usage{
km(time, status, group=NULL, tpt, pv=TRUE, pv.strat=NULL, pv.sub=NULL,
        rho=0, subset=NULL, na.action = na.omit, data=NULL)
km2ch(kmobj)
km2mat(kmobj)
      }
\arguments{
\item{time}{
failure time variable or a formula suitable for \code{survfit}
}
\item{status}{
Failure indicator (1=failure,0=censored).
Must be same length as time.  Ignored if \code{time} is a formula.
}
\item{group}{
group variable. If missing, treated as one group.
If given, must be same length as time.  Ignored if \code{time} is a formula.
}
\item{tpt}{
times of interval boundries for calculating \#events/\#risk.
}
\item{pv}{
If \code{pv=TRUE}, a logrank (or G-rho) p-value is also computed.
}
\item{pv.strat}{
Optional stratification variable for calculating the p-value.
}
\item{pv.sub}{
Optional subset to use in computing the p-value.  Both \code{pv.sub}
and \code{subset} are applied to computing the p-value
}
\item{rho}{
Value of rho for G-rho tests. Default is 0 (logrank).
}
\item{subset}{
a logical vector specifying a subset of cases to include in computing
the estimates
}
\item{na.action}{
a function specifying the action to take for any cases missing any of
time, status, group, pv.strat, pv.sub, or subset.
}
\item{data}{ A data frame.  Only used if \code{time} is a formula, in
  which case the variables are interpreted in the context of \code{data}.
}
\item{kmobj}{
  An object of class km (output from the function \code{km}).
}}
\details{
  \code{km} calls \code{survfit} to compute the Kaplan-Meier
  product-limit estimate of the survivor function and its estimated
  variance within each group, then converts the output to class \code{km}.
  \code{km2ch} converts the estimates from \code{km} to Nelson-Aalen
  estimates of the cumulative hazard.  In both cases, all corners of the
  step function estimates are included in the \code{time} and \code{est}
  components of the output, so that curves can be plotted with a call to
  \code{lines}, eg \code{lines(kmout[[1]]$time,kmout[[1]]$est)}.

  \code{km2mat} takes the output of \code{km} and first forms a common
  \code{time} variable incorporating all times where events occur in any
  of the groups.  Then each \code{est} component of the output is
  expanded to give the estimates at each time in the combined
  \code{time} vector.  The results are then placed in a matrix with
  the combined time vector in the first row, the estimates from the
  first group in the second row, and in general the estimates from the
  jth group in row j+1.

  P-values are computed using \code{survdiff}.
  
 Only the internal interval boundries should be given in \code{tpt}, since
 0 (below) and \code{max(y)} (above) are added on.  The default for
 \code{tpt} is \code{pretty(y)}, which should correspond to x-axis tick
 marks in a plot.  If \code{pv=TRUE} (and there is more than 1 group),
 an attribute \code{pv} containing the logrank (or G-rho) p-value, as
 computed by \code{logrank}, is also attached to the output. 
}
\value{
\code{km} returns a list of class \code{km}, with one component for each
group (groups are defined by the unique values of \code{group}).  Each
component in the list is itself a list with components 
\item{time}{the time axis values}
\item{est}{the Kaplan-Meier survival curve estimates at \code{time}}
\item{var}{the estimated variances}
\item{tint}{a character vector with an element for each time interval
  defined by \code{tpt} giving the \# events in each interval/\# at risk
  at the start of the interval}
\item{nnd}{the total \# cases and \# events for the group}

\code{km2ch} returns a list like the input \code{kmobj}, but with the
\code{$est} components replaced by the cumulative hazard estimates (note
that the \code{$var} components are not changed).

\code{km2mat} returns a matrix with
  the combined time vector in the first row, the estimates from the
  first group in the second row, and in general the estimates from the
  jth group in row j+1.
}
\author{Bob Gray}
\seealso{
\code{\link{plot.km}}, \code{\link[cmprsk]{timepoints}}, \code{\link{survfit}},
\code{\link{plot.survfit}} 
}
\examples{
set.seed(20)
rx <- c(rep('A',50),rep('B',50))
ftime <- rexp(length(rx),rate=ifelse(rx=='A',.2,.1))
ctime <- runif(length(rx))*4+3
find <- ifelse(ftime<=ctime,1,0)
ftime <- pmin(ftime,ctime)
z <- km(ftime,find,rx)
  }
\keyword{survival}
% Converted by Sd2Rd version 1.21.
