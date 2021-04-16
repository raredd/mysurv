sfplot <- function(z,pv=NULL,...) { 
# z=object from survit.formula, ...=args to plot.km
  tm <- min(c(0,z$time))
  if (length(z$n)==1) z$strata <- c(Overall=length(z$time))
  x <- vector('list',length=length(z$strata))
  l2 <- 0
  for (i in 1:length(z$strata)) {
    l <- l2+1
    l2 <- l2+z$strata[i]
    sub <- z$n.event[l:l2]>0
    tt <- z$time[l:l2][sub]
    tt <- c(tm,rep(tt,rep(2,length(tt))),z$time[l2])
    ss <- c(1,z$surv[l:l2][sub])
    ss <- rep(ss,rep(2,length(ss)))
    vv <- c(0,z$std.err[l:l2][sub])^2
    vv <- rep(vv,rep(2,length(vv)))
    x[[i]] <- list(time=tt,est=ss,var=vv,tint=NULL,nnd=c(z$n[i],sum(z$n.event[l:l2])))
  }
  names(x) <- names(z$strata)
  class(x) <- 'km'
  if (!is.null(pv)) plot(x,pv=pv,...) else plot(x,...)
  invisible(x)
}
