\name{plotcph}
\alias{plotcph}
\alias{plotmres}
\alias{plotschres}
\title{Plot residuals and estimated functions from Cox models }
\description{
  Plot residuals and estimated functions from Cox models
}
\usage{
plotcph(z, j, x, ylim, sub = rep(TRUE, length(x)), ...)

plotmres(x, z, f=.3, ...)

plotschres(x, del, z, j, f=.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{A \code{coxph} object }
  \item{j}{Integer identifying which term in the model to plot }
  \item{x}{Vector of covariate values. In \code{plotschres}, the vector
  of event and censoring times}
  \item{ylim}{The ylim of the plot }
  \item{sub}{can be used to subset the values of x }
  \item{\dots}{Arguments passed to \code{plot} }
  \item{f}{The bandwidth for \code{lowess} smoothing}
  \item{del}{The event indicator}
}
\details{
  plotcph is intended for use with eg \code{ns()}.  plotmres plots
  martingale residuals against covariate values (the covariate can
  either be in the model or not).  plotschres plots Schoenfeld
  residuals against event times.  The residual plots have a
  \code{lowess} smooth added, which uses non-robust smoothing.
}
\value{
  No value is returned; a plot is created on the current device.
}
\author{Bob Gray}
\examples{
## z1 <- coxph(Surv(ttr2,recind2)~ns(Recurrence,df=3),data=g2)
## plotcph(z1,1,g2$Recurrence,ylim=c(-2,2),
##    xlab='Recurrence Score',ylab='log hazard ratio')
}
\keyword{survival }% at least one, from doc/KEYWORDS
\keyword{hplot}
