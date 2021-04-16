\name{foresthr}
\alias{foresthr}
\alias{forest.lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a forest plot of hazard ratios}
\description{
  Given a data array of estimated log ratios and standard errors from
  subgroups, draws a forest plot 
}
\usage{
foresthr(d, rowlab = NULL, breaks = NULL, skip = 0.02, pos =
c(0, 0.3, 0.4, 0.6), cex = 1, ndig = 2, confcoef = 0.95,
mar = c(2.1, 0.6, 4.1, 0.6), main = NULL, maxbox = 0.9, axtick = NULL,
bcol = 1, lcol = 1, bg = lcol, collabs=NULL, adjx=0)

forest.lines(x, y, sc, lwd=1, lty=1, col=1, v=NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{matrix with log estimates in the first column and variances
    of the log estimates in the second column }
  \item{rowlab}{ vector of row labels for the plot, or a character
    matrix giving the row labels and additional columns of information
    (such as sample sizes)}
  \item{breaks}{ a small gap is placed after the rows with indexes
specified in this vector}
  \item{skip}{ Specifies the size of the gap after the rows specified by
breaks (the units are opaque, but larger values give larger gaps)}
  \item{pos}{ Positions of the labels, estimated ratios, confidence
intervals, and plot, as a proportion of the plotting region}
  \item{cex}{ character expansion for the text in the plot (row labels,
estimates and confidence intervals)}
  \item{ndig}{ number of decimal places in the estimates and confidence
intervals }
  \item{confcoef}{The confidence coefficient as a proportion (must be
less than 1)}
  \item{mar}{ The \code{mar} argument to \code{par}}
  \item{main}{ The main title to the plot}
  \item{maxbox}{ The plot allots a a proportion of the vertical range to
each row.  \code{maxbox} it the proportion of the allotted vertical
range that the maximum sized box can occupy.}
  \item{axtick}{ The ordinated for x-axis tick marks }
  \item{bcol}{ The box (filled rectangle) color }
  \item{lcol}{ The color of the forest lines in the rows }
  \item{bg}{ This specifies the color that the line takes within the
box.  This argument is most useful when some lines lie entirely within boxes.}
  \item{collabs}{A vector of character strings giving the column headers
  in the plot.  Should be length \code{max(3,ncol(rowlab)+2)}}
  \item{adjx}{Vector of length equal to the number of columns in the
    plot (or length 1) giving the horizontal text justification (values
    correspond to the \code{adj} argument to \code{par}}
  \item{x}{The x coordinates of the points to join}
  \item{y}{The y coordinates of the points to join}
  \item{sc}{Scaling information output from \code{foresthr}}
  \item{lwd}{The line width (see par)}
  \item{lty}{The line type (see par)}
  \item{col}{The line color (see par)}
  \item{v}{If \code{v} is specified, a vertical line is drawn the full
height of the plot at the x coordinate specified by \code{v}, and
\code{x} and \code{y} are ignored}
}
\details{
  Calculates symmetric confidence intervals on the log scale and then
exponentiates.
 
  The text corresponding to the labels, the estimates and the confidence
intervals are left justified at \code{pos[j], j=1,2,3} of the distance
from the left edge to the right edge of the overall plotting region,
unless \code{adjx} specifies an alternate justification.  By
setting the corresponding component of \code{pos} to a number much less
than 0 or greater than 1, the corresponding column will be deleted from
the plot.

  The y-axis is not explicitly labeled, but uses a range of 0 to 1.
}
\value{
 \code{foresthr} returns a list of scaling factors that can be used by
functions such as \code{forest.line} to place additional components on
the plot. 
}
\author{Bob Gray }
\examples{
\dontrun{
plt2 <- function(u,file,maincol='white',col1='#00FFFF',col2='#0088FF',
      ff=2,pos=c(0,0.3,0.4,0.6),lab) {
  win.metafile(file,pointsize=20,width=9,height=6.75)
  par(lwd=2,font=ff,font.axis =ff, font.lab =ff, font.main =ff, font.sub =ff,
      las=1,cex.axis=.75,fg = maincol, col = maincol, 
      col.axis=maincol,col.main=maincol,col.lab=maincol,col.sub=maincol,
      ljoin = 1, lend = 2, cex.main=1.33,mgp=c(2.5,.75,0))
  ux <- foresthr(u,rowlab=lab,breaks=c(4,7,10,12,14,15)-1,
      axtick=c(0,.5,1,1.5),main='E2100 PFS Hazard Ratios',bg=col1,
      cex=.9,lcol=col1,bcol=col1,maxbox=1.02,pos=pos)
# add vertical line at the null value
  forest.lines(sc=ux,v=1,lwd=1,col=maincol)
# add dotted vertical line at the overall estimate
  forest.lines(sc=ux,v=0.51,lwd=1,col=maincol,lty=2)
  dev.off()
}
# plot with estimates and confidence intervals on the plot
plt2(u,file='forest2.wmf',lab=lab)
# plot witn only labels and lines
plt2(u,file='forest3.wmf',pos=c(0,-10,-10,0.33),lab=lab2)
}
}
\keyword{hplot}% at least one, from doc/KEYWORDS

