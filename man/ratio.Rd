\name{ratio}
\alias{ratio}
\alias{wald}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Wald Statistics and Estimate Ratios }
\description{
 Given a fit of a model, wald computes the Wald test for a subset of the
 parameters equal to 0 and ratio gives a confidence interval on the
 exponential fundtion of a parameter or linear combination of parameters
}
\usage{
ratio(x, j = NULL, cntr = NULL, coef = 0.95, inv = FALSE)
wald(x, kk = 1:length(coef(x)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The output from the fit of a model where the estimated
    coefficients can be accessed with the \code{coef} function and the
    estimated variance-covariance matrix can be accessed with \code{x$var} }
  \item{j}{If \code{cntr} is specified, \code{j} can be used to specify
    a subset of the parameters that \code{cntr} is applied to.  If not,
    then \code{j} is a single value and the ratio for that parameter is
    estimated.}
  \item{cntr}{Values specifying a linear combination of the parameters
    (optional)}
  \item{coef}{The confidence level for the (two-sided) confidence interval}
  \item{inv}{If TRUE, the inverse ratio is estimated}
  \item{kk}{The hypothesis that the parameters in \code{coef(x)[kk]} are
  0 is tested}
}
\details{
  If both \code{j} and \code{cntr} are specified, then the estimated
  quantity is \code{exp(sum(coef(x)[j]*cntr))}.  \code{ratio} assumes
  the model is fitting the model on the log scale, so exponentiated
  parameters are of interest (eg as estimates of hazard ratios or odds
  ratios). 
}
\value{
  \code{ratio} returns a list with elements giving the estimated ratio,
  the lower and upper confidence limits and the Wald p-value for the
  specified linear combination of the parameters.  \code{wald} returns a
  vector giving the Wald statistic, degrees of freedom and chisquare
  p-value.
}
\examples{
##---- Should be DIRECTLY executable !! ----
}

\keyword{htest}

