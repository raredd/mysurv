logrank <- function(time,status,group,strata=NULL,rho=0,subset=NULL,na.action=na.omit,data=NULL) {
  call <- match.call()
  if (class(time)=='formula') {
    z <- do.call('survdiff',list(formula=time,data=data,subset=substitute(
      subset),na.action=na.action,rho=rho))
  } else {
    d <- data.frame(time=time,status=status,group=as.factor(group),
                    strat=as.factor(if (is.null(strata)) 
                    rep(1,length(time)) else strata))
    z <- survdiff(Surv(time,status)~group+strata(strat),data=d,subset=subset,
                  na.action=na.action,rho=rho)
  }
  z$call <- NULL
  t1 <- cbind(apply(as.matrix(z$obs),1,sum),apply(as.matrix(z$exp),1,sum),z$n)
  dimnames(t1)[[2]] <- c('observed','expected','sample size')
  list(stat=z$chisq,pv=1-pchisq(z$chisq,nrow(t1)-1),df=nrow(t1)-1,oe=t1,
  score=(t1[,1]-t1[,2])[-nrow(t1)],var=z$var[-nrow(t1),-nrow(t1)],call=call)
}

#logrank <- function(time,status,group,strata,rho=0,subset,na.action=na.omit) {
## for documentation see help file logrank.d
#  d <- data.frame(time=time,status=status,group=as.factor(group),
#    strata=as.factor(if (missing(strata)) rep(1,length(time)) else strata))
#  if (!missing(subset)) d <- d[subset,]
#  tmp <- nrow(d)
#  d <- na.action(d)
#  if (nrow(d) != tmp) cat(format(tmp-nrow(d)),'cases omitted due to missing values\n')
#  n <- nrow(d)
## check status variable
#  if (any(d$status != 0 & d$status != 1)) stop("invalid status values")
#  ugg <- table(d$group)
#  group <- factor(d$group,names(ugg)[ugg>0])
#  lbls <- levels(group)
#  group <- as.integer(group)
#  ugg <- table(d$strata)
#  strata <- as.integer(factor(d$strata,names(ugg)[ugg>0]))
#  o <- order(d$time)
#  nstr <- max(strata)
#  nt <- max(group)
#  if (nt<2) stop("Not enough treatment groups")
#  T1 <- 1:nt
#  nt1 <- nt-1
#  nt2 <- nt*nt1/2
#  oe <- matrix(0,ncol=3,nrow=nt)
#  vs <- matrix(0,ncol=nt1,nrow=nt1)
#  storage.mode(oe) <- "double"
#  storage.mode(vs) <- "double"
#  s <- rep(0,nt1)
#  wk1 <- double(length=(n+2*nt2+nt1+3*nt))
#  wk2 <- integer(length=(2*n+2*nt))
#  z2 <- .C("lr",as.double(d$time[o]),as.integer(d$status[o]),
#          as.integer(group[o]),as.integer(strata[o]),
#          as.integer(n),as.double(rho),as.integer(nstr),as.integer(nt),
#          as.integer(nt1),as.integer(nt2),as.double(s),vs,oe,wk1,wk2,
#          PACKAGE="mysurv")
#  dimnames(z2[[13]]) <- list(lbls,c("observed","expected","sample size"))
#  stat <- -1
#  a <- qr(z2[[12]])
#  if (a$rank==ncol(a$qr)) {
#    b <- diag(dim(a$qr)[1])
#    stat <- z2[[11]]%*%qr.coef(a,b)%*%z2[[11]]
#    pv <- 1-pchisq(stat,nt1)
#  }
#  list(stat=stat,pv=pv,df=nt1,oe=z2[[13]],score=z2[[11]],var=z2[[12]])
#}

