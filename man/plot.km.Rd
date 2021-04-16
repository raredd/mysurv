\name{plot.km}
\alias{plot.km}
%\alias{plot.km1}
\alias{plot.ch}
\alias{sfplot}
\title{
Plot survival curves
}
\description{
Plot method for objects of class "km"
}
\usage{
\method{plot}{km}(x, main = " ", curvlab = NULL, ylim = c(0, 1), xlim = NULL,
    wh = 1, xlab = "Years", ylab = "Probability", lty = 1:length(x), 
    eventlabel = "events", pv = NULL, pv.wh = NULL, pv.dig = 2,
    color = 1, medians = FALSE, medians.wh=NULL, counts=TRUE,
    xticks=NULL, lwdax=2, \dots) 
\method{plot}{ch}(x, blk = 0.35, ylim, wh, ylab = "Cumulative Hazard",
   pv = FALSE, pv.wh, \dots) 
sfplot(z,pv=NULL,\dots)
%plot.km1(x, main = " ", curvlab = NULL, ylim = c(0, 1), xlim = NULL,
%    wh = 1, xlab = "Years", ylab = "Probability", lty = 1:length(x), 
%    eventlabel = "events", pv = NULL, pv.wh = NULL, pv.dig = 2,
%    color = 1, lwdl = NULL, medians = FALSE, \dots) 
}  
\arguments{
\item{x}{
An object of class \code{km} (for \code{plot.km}) or
of class \code{ch} (for \code{plot.ch}), which are
lists, with each component representing one curve in the plot.  Each
component of \code{x} is itself a list whose first component gives the x values
and 2nd component the y values to be plotted.  Optionally, \code{x} can
include an attribute \code{pv}.
}
\item{main}{
the main title for the plot.
}
\item{curvlab}{
Curve labels for the plot.  Default is names(x), or if that is missing,
1,2,\dots.
}
\item{ylim}{
yaxis limits for plot
}
\item{xlim}{
xaxis limits for plot (default is 0 to the largest time in any of the
curves)
}
\item{wh}{
controls where the legend is placed.  If this is a vector of length 2 then it
gives the plot coordinates of the upper left corner of the legend, while if
it is the single number 1 (the default) the legend is placed (more or less)
in the lower left corner, and for any other single number the legend is put
in the upper right portion of the plot.
}
\item{xlab}{
x axis label
}
\item{ylab}{
 y axis label
}
\item{lty}{
vector of line types.  Default \code{1:nc}.  If \code{length(lty)<nc}, then
\code{lty[1]} is used for all curves.
}
\item{eventlabel}{
curve labels in the legend are augmented by \# events/\# cases.  This is the
label used for "events".  Default is "events".  Other useful choices include
"deaths", "relapses", "progressions", etc.
}
\item{pv}{
If \code{pv=TRUE}, a text string giving the \code{pv} attribute of
\code{x} is added to the plot (prepended by "P = ").
If \code{pv} is a numeric value, this value is used instead.
If \code{pv} is a character string, this value is used instead.
In \code{sfplot}, \code{pv} must be the value of the p-value.
}
\item{pv.wh}{
Vector of length 2 giving the coordinates of where to put the p-value (text
is plotted left justified).  Default is to put it above the curve legend.
}
\item{pv.dig}{
Number of significant digits to use in the p-value.  Default is 2.  This is
actually used as the "digits" argument to the function \code{format}.
The call to format also uses \code{scientific=FALSE}.
}
\item{color}{
vector of color codes.  If \code{length(color)<nc}, then \code{color[1]} is
used for all curves.
}
\item{medians}{
  if \code{medians=TRUE}, then lines are drawn indicating the medians
  and text giving the median values is added to the plot.
}
\item{medians.wh}{
  if \code{medians=TRUE}, then by default the text giving the medians is
  written to the right of the line through the medians.
  \code{medians.wh} (a vector of length 2) allows specifying an
  alternate location (left justified and vertically centered).
}
\item{counts}{
  if \code{counts=TRUE}, then the number of events and number of cases
  in each group is appended to the curve labels.
}
\item{xticks}{
  if non-NULL, specifies the location of the tick marks on the x axis
  (added via a call to axis).  Arguments such as \code{mgp} should be
  specified in \code{par()} to be applied consistently to the whole plot.
}
\item{lwdax}{
  line width used for the call to plot only.  This is provided so that
  a different line width can be used in setting up the plot and in
  drawing the curves and legend.  The value of \code{par()$lwd} is used
  for the curves, and the value of \code{lwdax} for setting up the plot.
}
%\item{lwdl}{
%  line width for the curves in the plot only.  This is provided so that
%  thicker lines can be used for the curves than for axis or other lines.
%}
\item{\dots}{
additional arguments passed to the initial call of the plot function and
to legend for matching arguments
(additional arguments to \code{plot.km} for \code{plot.ch} and \code{sfplot})
}
\item{blk}{
  In \code{plot.ch}, the upper limit on the y-axis defaults to the
  maximum of the cumulative hazard estimates \code{* (1+blk)}}
\item{z}{
  In \code{sfplot}, an object of class \code{survfit}}
}
\details{
  \code{sfplot} is a function to convert an object of class
  \code{survfit} to one of class \code{km} and then call \code{plot.km}
  to make the plot.
  }
\value{
No value is returned, except for \code{sfplot}, which returns the
converted \code{km} object.  All functions draw a plot on the current
graphics device. 
}
\author{ Bob Gray}
\seealso{
\code{\link{km}}, \code{\link{plot.survfit}}
}
\examples{
set.seed(20)
rx <- c(rep('A',50),rep('B',50))
ftime <- rexp(length(rx),rate=ifelse(rx=='A',.2,.1))
ctime <- runif(length(rx))*4+3
find <- ifelse(ftime<=ctime,1,0)
ftime <- pmin(ftime,ctime)
plot(km(ftime,find,rx))
}
\keyword{survival}
\keyword{hplot}
% Converted by Sd2Rd version 1.21.
