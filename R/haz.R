haz <- function (time,status,group,span,np=100,lower=0,upper,subset,na.action=na.omit) {
# time=surv time, status=cens ind (0=cens, 1=fail), group=group
# span=span size, np=number points where smooth haz is evaluated,
# lower=lower range of est, upper=upper range.
# subset restricts analysis to that subset; na.action=action for NA's in 
#  any of time status or group
# output is a list with a component for each group, which is 
# itself a list giving the timepoints where the hazard is eval
# and the corresponding smoothe hazard estimates.
# uses simple kernel smoothing with fixed length span
  d <- data.frame(time=time,status=status,
    group=as.factor(if (missing(group)) rep(1,length(time)) else group))
  if (!missing(subset)) d <- d[subset,]
  tmp <- nrow(d)
  d <- na.action(d)
  if (nrow(d) != tmp) cat(format(tmp-nrow(d)),'cases omitted due to missing values\n')
  no <- nrow(d)
# check status variable
  if (any(d$status != 0 & d$status != 1)) stop("invalid status values")
  if (missing(upper)) upper <- max(time)
  if (missing(span)) span <- (upper-lower)/4
  ugg <- table(d$group)
  d$group <- factor(d$group,names(ugg)[ugg>0])
  z <- survfit(Surv(time,status)~group,data=d)
  if (length(z$n)==1) z$strata <- c(Overall=length(z$time))
  Tl <- length(z$strata)
  zout <- as.list(1:Tl)
  l2 <- 0
  st <- seq(lower,upper,length=np)
  for (j in 1:length(z$strata)) {
    l <- l2+1
    l2 <- l2+z$strata[j]
    Ty <- z$time[l:l2]
    Tm <- z$n.event[l:l2]
    Tr <- z$n.risk[l:l2]
    Ty <- Ty[Tm>0]
    Tr <- Tr[Tm>0]
    Tm <- Tm[Tm>0]
    sf <- sv <- NULL
    if (sum(Tm)>0) {
      for (i in 1:length(st)) {
        tt=st[i]
        a=max(lower,tt-span)
        b=min(upper,tt+span)
        a1=(a-tt)/span
        b1=1
        con=b1-2*(b1^3)/3+(b1^5)/5-a1+2*(a1^3)/3-(a1^5)/5
        con=con*span
        sub <- Ty>=a & Ty<=b & Tr>0
        cc <- (tt-Ty)/span
        cc <- (1-cc^2)^2
        sf <- c(sf,sum(cc[sub]*Tm[sub]/Tr[sub])/con)
        sv <- c(sv,sum(cc[sub]^2*Tm[sub]/Tr[sub]^2)/(con*con))
      }
    }
    zout[[j]] <- list(time=st,est=sf,var=sv)
  }
  names(zout) <- names(z$strata)
  class(zout) <- 'km'
  zout
}
