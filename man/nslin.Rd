\name{nslin}
\alias{nslin}
\title{
Use a natural spline fit to test for linearity of a covariate
}
\description{
Given a coxph object with a natural spline fit, tests for linearity of
the effect
}
\usage{
nslin(z, nscol, x, df = length(nscol))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
coxph object from the fit of a model containing a natural spline component
}
  \item{nscol}{
the index of the natural spline variables in the vector of coefficients
}
  \item{x}{
The variable the spline term was computed from
}
  \item{df}{
 Probably should not be changed from the default, since it needs to
 match the model in \code{z}
}
}
\details{
The model needs to have contained a term of the form \code{ns(x,df=df)}, which
assumes the \code{df} is in addition to the constant term (which is not part of
the generated basis functions).  The
constraints on the coefficients to give a linear fit are determined, and
the Wald test for the null hypothesis of a linear effect is computed.
The variable \code{x} must contain only the values actually used in the fit in
\code{z}.  \code{nscol} should be a vector of integers indicating the position in the
coefficient vector of the spline terms.  The function can be used with
any model object for which the coefficients can be extracted with the
\code{coef()} function and the variance-covariance matrix obtained with
\code{z$var}.
}
\value{
A vector giving the value of the Wald (chi-square) statistic, the
p-value and the degrees of freedom.
}
\author{
Bob Gray
}
\seealso{
\code{\link{ns}}
}
\examples{
#z2 <- coxph(Surv(ttrn,recindn)~ns(Recurrence,df=3),data=g2p,subset=sub)
#nslin(z2,1:3,g2p$Recurrence[sub])
}
\keyword{ splines }
