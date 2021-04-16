km2ch <- function(kmobj) {
  if (!inherits(kmobj,"km")) stop("kmobj not a km object")
  maxch <- 0
  for (i in 1:length(kmobj)) {
    ind <- seq(from=1, to=length(kmobj[[i]]$time),by=2)
    ch <- -diff(kmobj[[i]]$est[ind])/kmobj[[i]]$est[ind[-length(ind)]]
    ch <- cumsum(ch)
    maxch <- max(maxch,ch[length(ch)])
    kmobj[[i]]$est <- rep(c(0,ch),rep(2,length(ind)))
  }
  attr(kmobj,"ymax") <- maxch
  class(kmobj) <- c("ch","km")
  kmobj
}

plot.ch <- function(x,blk=.35,ylim,wh,ylab = "Cumulative Hazard" ,pv=FALSE,pv.wh,...) {
  if (missing(ylim)) ylim <- c(0,attr(x,"ymax")*(1+blk))
  if (missing(wh)) wh <- c(0,ylim[2])
  if (missing(pv.wh)) pv.wh <- c(wh[1],wh[2]-length(x)*.1*diff(ylim))
  plot.km(x,...,wh=wh,ylim=ylim,pv=pv,pv.wh=pv.wh,ylab=ylab)
}
