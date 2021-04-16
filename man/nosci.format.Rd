\name{nosci.format}
\alias{nosci.format}
\alias{rpv}
\title{ Convert a single numeric value to a formatted text string}
\description{
Converts a single numeric value to a formatted text string.  For decimal
values with leading zeros, prohibits conversion to scientific notation
so will print as a decimal value with leading zeros.
}
\usage{
nosci.format(x, digits = 2)

rpv(x)
}
\arguments{
  \item{x}{ a single numeric value in nosci.format or a vector in rpv}
  \item{digits}{ number of significant digits }
}
\details{
  Within the limits of numeric representation, nosci.format
  gives a character string
giving the decimal representation of a number rounded to \code{digits}
significant digits.  Unlike \code{format}, this function does not
convert small numbers to scientific notation, but prints the full
decimal representation regardless of the number of leading zeros.  Also,
if the trailing significant digits have rounded to zero, then the
appropriate number of zeros are appended to the end of the string (see
the first two examples, below).

\code{rpv} rounds values \code{>=0.095} to 2 decimal places and retains
1 significant digit for vales \code{<0.095}
}
\value{
\code{nosci.format} returns a single character string giving the
formatted representation of \code{x}.  \code{rpv} gives a vector of
numeric values.
}

\seealso{ \code{\link{format}}}
\examples{
#decimal representation
nosci.format(1e-4)
#note trailing zeros
nosci.format(.000099999,3)
nosci.format(.0001892)
nosci.format(1.3)
#only suppresses scientific notation for small numbers
nosci.format(123e20)
rpv(c(.00012135,.99,.1))
}
\keyword{character}% at least one, from doc/KEYWORDS
