km <- function (time,status,group=NULL,tpt,pv=TRUE,pv.strat=NULL,pv.sub=NULL,rho=0,subset=NULL,na.action=na.omit,data=NULL) {
# time=surv time, status=cens ind (0=cens, 1=fail), group=group
# tpt is a list of times giving internal cut points for 
# calculating #events/#risk on intervals (only used by kmplt)
# pv arguments are for computing G^rho p-values (if pv=T)
# (allow separate strata (pv.strat) and subset(pv.sub) arguments)
# subset (logical)=indicator of cases to include
# output is a list with a component for each group, which is 
# itself a list giving the times, survival curv estimates
# and variances, suitable for use with timepoints and plot.km 
  call <- match.call()
  if (class(time)=='formula') {
    z <- do.call('survfit',list(formula=time,data=data,
                 subset=substitute(subset),na.action=na.action,
                 error='greenwood',conf.type='none'))
    if (pv & length(z$strata)>1) {
      x2 <- deparse(substitute(pv.strat))
      x3 <- deparse(substitute(subset))
      x4 <- deparse(substitute(pv.sub))
      if (x3=='NULL') {
        x5 <- x4
      } else if (x4=='NULL') {
        x5 <- x3
      } else {
        x5 <- paste(x3,'&',x4)
      }
      if (x2=='NULL') {
        zp <- do.call('survdiff',list(formula=time,data=data,
                subset=parse(text=x5),na.action=na.action,rho=rho))
      } else {
        f <- paste(deparse(substitute(time)),'+ strata(',x2,')')
        zp <- do.call('survdiff',list(formula=as.formula(f),data=data,
                subset=parse(text=x5),na.action=na.action,rho=rho))
#                subset=substitute(pv.sub),na.action=na.action,rho=rho))
      }
      pv <- 1-pchisq(zp$chisq,nrow(as.matrix(zp$obs))-1)
    }
  } else {
    d <- data.frame(time=time,status=status,
      group=as.factor(if (is.null(group)) rep(1,length(time)) else group),
      pv.strat=as.factor(if (is.null(pv.strat)) rep(1,length(time)) else 
      pv.strat),pv.sub = if (is.null(pv.sub)) rep(TRUE, length(time))
      else pv.sub,subset = if (is.null(subset)) rep(TRUE, length(time))
      else subset)
    z <- survfit(Surv(time,status)~group,data=d,subset=subset,
            na.action=na.action,error='greenwood',conf.type='none')
    if (pv & length(z$strata)>1) {
      zp <- survdiff(Surv(time,status)~group+strata(pv.strat),data=d,subset=
                  pv.sub & subset,na.action=na.action,rho=rho)
      pv <- 1-pchisq(zp$chisq,nrow(as.matrix(zp$obs))-1)
    }
  }  
  tm <- min(c(0,z$time))
  if (missing(tpt)) {
    tpt <- pretty(z$time)}
  else {
    tpt <- c(tm-.00001,tpt,max(z$time))
  }
  tptl <- round(tpt,2)
  ntpt <- length(tpt)-1
  lev <- vector("character",ntpt)
  for (i in 1:ntpt) lev[i] <- paste(format(tptl[i]),format(tptl[i+1]),sep="-")
  tind <- cut(z$time,tpt)
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
    tt3 <- tapply(z$n.risk[l:l2],tind[l:l2],max)
    tt2 <- tapply(z$n.event[l:l2],tind[l:l2],sum)
    tt3 <- paste(format(tt2),'/',format(tt3),sep='')
    names(tt3) <- lev
    x[[i]] <- list(time=tt,est=ss,var=vv,tint=tt3,nnd=c(z$n[i],sum(z$n.event[l:l2])))
  }
  names(x) <- names(z$strata)
  if (pv & length(z$strata)>1) attr(x,"pv") <- pv
  class(x) <- 'km'
  x
}
