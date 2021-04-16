\name{myps}
\alias{myps}
\alias{mypsu}
\alias{mywmf}
\alias{mypdf}
\alias{mybitmap}
\title{ Start Postscript or Windows Metafile Graphics Devices }
\description{
  Customized interfaces to the functions \code{postscript} and
  \code{win.metafile} 
}
\usage{
myps(file, pointsize = 20, family = "Helvetica", lwd = 2.5, ...)
mypsu(file, pointsize = 20, family = "Helvetica", lwd = 2.5, ...)
mywmf(file, path = NULL, pointsize = 18, lwd = 4, maincol = "white",
width = 8, height = 6, ...)
mypdf(file, pointsize = 20, family = "Helvetica", lwd = 2.5, width=10.5,
height=8.0, maincol='black', ...)
mybitmap(file, type=c('png','tiff','jpeg'),path = NULL, 
  pointsize = 14, lwd = 2,maincol = "black", width = 640, height = 480,
  font=2, ...) 
}
\arguments{
  \item{file}{File name to write the plot(s) to.  The \code{.ps},
    \code{.emf}, etc, extension is added automatically.}
  \item{pointsize}{ Default pointsize for text in the plots }
  \item{family}{ Font family for the postscript driver }
  \item{lwd}{ Default line width for curves in the plot }
  \item{\dots}{ Additional arguments to \code{postscript}, \code{pdf} or
    \code{par}} 
  \item{path}{An absolute path or a relative path from the current
    working directory to to the directory where \code{file} should be
    written}
  \item{maincol}{The default color for all text, lines and axes}
  \item{width}{\code{width} argument to \code{pdf} or \code{win.metafile}}
  \item{height}{\code{height} argument to \code{pdf} or \code{win.metafile}}
  \item{type}{type of bitmap to create}
  \item{font}{type of font to use in mybitmap}
}
\details{
These functions are customized interfaces to the functions
\code{postscript}, \code{win.metafile}, etc.  The \code{file} argument
will have the extension, eg \code{.ps} or \code{.emf}, added automatically.
The \code{path} argument is usually not needed, if \code{setwd} has been
used to set the working directory appropriately.  \code{mypsu} is for
the old version of R under unix, which does not support all of the par
arguments used in the other versions.
}
\value{
No value is returned.  The corresponding graphics device is initialized
with the specified options.
}
\author{ Robert Gray }

\examples{
myps('plot1')
#plot commands
dev.off()
#mywmf('plot2')
}
\keyword{ device }% at least one, from doc/KEYWORDS
