# plot.km1 <- function(x,main=" ",curvlab=NULL,ylim=c(0,1),xlim=NULL,wh=1,
# xlab="Years",ylab="Probability",lty=1:length(x),eventlabel="events",pv=NULL,
# pv.wh=NULL,pv.dig=2,color=1,lwdl=NULL,medians=FALSE,...) {
# # x is a list containing curves to be plotted. Each component of
# # x is a list with the first component containing the x values
# # and the second component the y values.  main = main title in the plot
# # curvlab=curve labels (vector), wh=where curve labels are plotted
# # 1=lower left 2=upper left 3=upper right 4=lower right
# #  if (!exists(".Device")) {stop("Graphics device must be active")}
# #  oldpar <- par(las=1)
# #  on.exit(par(oldpar))
#   nc <- length(x)
#   if (length(lty) < nc) lty <- rep(lty[1],nc)
#   if (length(color) < nc) color <- rep(color[1],nc)
#   if (is.null(curvlab)) {
#      if (mode(names(x))=="NULL") {
#        curvlab <- as.character(1:nc) }
#        else curvlab <- names(x)
#   }
#   for (i in 1:nc) if (!is.null(x[[i]]$nnd)) curvlab[i] <-
#       paste(curvlab[i],"  (",format(x[[i]]$nnd[2])," ",eventlabel,"/ ",
#              format(x[[i]]$nnd[1])," cases)",sep="")
#   if (is.null(xlim)) {
#      xmax <- 0
#      for (i in 1:nc) {
#        xmax <- max(c(xmax,x[[i]][[1]]))
#      }
#      xlim <- c(0,xmax)
#   }
#   if (length(wh)==1) {
#       if (wh==1) wh <- c(xlim[1],nc*.08+.05)
#       else wh <- c(sum(xlim)/2,ylim[2])
#   }
#   plot(x[[1]][[1]],x[[1]][[2]],type="n",ylim=ylim,xlim=xlim,
#        main=main,xlab=xlab,ylab=ylab,bty="l",...)
#   if (medians) {
#     mm <- quantile.km(x)
#     uu <- par()$usr
#     xx <- uu[2]-uu[1]
#     lines(c(uu[1],max(mm)+.05*xx),c(.5,.5))
#     lines(c(mm[1],mm[1]),c(uu[3],.5))
#     lines(c(mm[2],mm[2]),c(uu[3],.5))
#     text(c(max(mm)+.07*xx),.5,paste('Medians:',paste(format(round(min(mm),1),
#        nsmall=1),',',sep=''),format(round(max(mm),1),nsmall=1)),adj=c(0,.5))
#   }
#   if (!is.null(lwdl)) par(lwd=lwdl)
#   u <- list(...)
#   if (length(u)>0) {
#     i <- pmatch(names(u),names(formals(legend)),0)
#     do.call('legend',c(list(x=wh[1],y=wh[2],legend=curvlab,col=color,lty=lty,bty="n",bg=-999999),u[i>0]))
#   } else {
#     do.call('legend',list(x=wh[1],y=wh[2],legend=curvlab,col=color,lty=lty,bty="n",bg=-999999))
#   }
# #  legend(wh[1],wh[2],legend=curvlab,col=color,lty=lty,bty="n")
#   if (!is.null(pv))  {
#     if (is.null(pv.wh)) pv.wh <- wh+c(0,.08)
#     if (!is.numeric(pv)) {
#       if (pv) {
#         pv <- attr(x,"pv")
# #        text(pv.wh[1],pv.wh[2],paste("  P =",format(pv,digits=pv.dig,
# #                scientific=FALSE)),adj=0,xpd=T,...)}
#         text(pv.wh[1],pv.wh[2],paste("  P =",nosci.format(pv,digits=pv.dig)),adj=0,xpd=TRUE,...)}
#     } else {
# #      text(pv.wh[1],pv.wh[2],paste("  P =",format(pv,digits=pv.dig,
# #                scientific=FALSE)),adj=0,xpd=T,col=2,...)
#       text(pv.wh[1],pv.wh[2],paste("  P =",nosci.format(pv,digits=pv.dig)),adj=0,xpd=TRUE,...)
#     }
#   }
#   for (i in 1:nc) {
#     lines(x[[i]][[1]],x[[i]][[2]],col=color[i],lty=lty[i],...)
#   }
# }

plot.km <- function(x,main=" ",curvlab=NULL,ylim=c(0,1),xlim=NULL,wh=1,
xlab="Years",ylab="Probability",lty=1:length(x),eventlabel="events",pv=NULL,
pv.wh=NULL,pv.dig=2,color=1,medians=FALSE,medians.wh=NULL,counts=TRUE,
xticks=NULL,lwdax=2,...) {
  nc <- length(x)
  if (length(lty) < nc) lty <- rep(lty[1],nc)
  if (length(color) < nc) color <- rep(color[1],nc)
  if (is.null(curvlab)) {
     if (mode(names(x))=="NULL") {
       curvlab <- as.character(1:nc) }
       else curvlab <- names(x)
  }
  if (counts) {
    for (i in 1:nc) if (!is.null(x[[i]]$nnd)) curvlab[i] <-
      paste(curvlab[i],"  (",format(x[[i]]$nnd[2])," ",eventlabel,"/ ",
            format(x[[i]]$nnd[1])," cases)",sep="")
  }
  if (is.null(xlim)) {
     xmax <- 0
     for (i in 1:nc) {
       xmax <- max(c(xmax,x[[i]][[1]]))
     }
     xlim <- c(0,xmax)
  }
  if (length(wh)==1) {
      if (wh==1) wh <- c(xlim[1],nc*.08+.05)
      else wh <- c(sum(xlim)/2,ylim[2])
  }
  oldpar <- par(lwd=lwdax)
  if(is.null(xticks)) {
    plot(x[[1]][[1]],x[[1]][[2]],type="n",ylim=ylim,xlim=xlim,
         main=main,xlab=xlab,ylab=ylab,bty="l",...)
  } else {
    plot(x[[1]][[1]],x[[1]][[2]],type="n",ylim=ylim,xlim=xlim,
         main=main,xlab=xlab,ylab=ylab,bty="l",xaxt='n',...)
    axis(1,xticks)
  }
  par(oldpar)
  u <- list(...)
  if (length(u)>0) {
    i <- pmatch(names(u),names(formals(legend)),0)
    do.call('legend',c(list(x=wh[1],y=wh[2],legend=curvlab,col=color,lty=lty,bty="n",bg=-999999),u[i>0]))
  } else {
    do.call('legend',list(x=wh[1],y=wh[2],legend=curvlab,col=color,lty=lty,bty="n",bg=-999999))
  }
#  legend(wh[1],wh[2],legend=curvlab,col=color,lty=lty,bty="n")
  if (!is.null(pv))  {
    if (is.null(pv.wh)) pv.wh <- wh+c(0,.08)
    if (is.character(pv)) {
      text(pv.wh[1],pv.wh[2],pv,adj=0,xpd=TRUE)
    } else {
      if (!is.numeric(pv)) {
        if (pv) {
          pv <- attr(x,"pv")
          text(pv.wh[1],pv.wh[2],paste("P =",nosci.format(pv,digits=pv.dig)),adj=0,xpd=TRUE)}
      } else {
        text(pv.wh[1],pv.wh[2],paste("P =",nosci.format(pv,digits=pv.dig)),adj=0,xpd=TRUE)
      }
    }
  }
  for (i in 1:nc) {
    if (max(x[[i]][[1]])>xlim[2]) {
      sub <- x[[i]][[1]]<=xlim[2]
      xx <- c(x[[i]][[1]][sub],xlim[2])
      yy <- x[[i]][[2]][sub]
      yy <- c(yy,yy[length(yy)])
      lines(xx,yy,col=color[i],lty=lty[i])
    } else {
      lines(x[[i]][[1]],x[[i]][[2]],col=color[i],lty=lty[i])
    }
  }
  if (medians) {
    uu <- par()$usr
    m1 <- quantile.km(x)
    m1 <- m1[!is.na(m1)]
    if (is.null(medians.wh)) medians.wh <- c(c(max(m1)+.045*(uu[2]-uu[1])),.5)
    lines(c(uu[1],max(m1)+.03*(uu[2]-uu[1])),c(.5,.5),lwd=1)
#    lines(c(uu[1],max(m1)*1.1),c(.5,.5),lwd=1)
    for (i in 1:length(m1)) lines(c(m1[i],m1[i]),c(uu[3],.5),lwd=1)
#    lines(c(m1[2],m1[2]),c(uu[3],.5),lwd=1)
    text(medians.wh[1],medians.wh[2],paste('Medians:',
       paste(format(round(sort(m1),1)),collapse=', ')),adj=c(0,.5))
#       paste(format(round(min(m1),1)),',',sep=''),
#       format(round(max(m1),1))),adj=c(0,.5))
  }
}
