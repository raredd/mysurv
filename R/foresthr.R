foresthr <- function(d,rowlab=NULL,breaks=NULL,skip=.02,pos=c(0,.3,.4,.6),
    cex=1,ndig=2,confcoef=.95,mar=c(2.1,.6,4.1,.6),main=NULL,maxbox=.9,
    axtick=NULL,bcol=1,lcol=1,bg=lcol,collabs=NULL,adjx=0) {
  if (confcoef>=1) stop('confcoef must be < 1')
  z1 <- qnorm(.5+confcoef/2)
  s <- sqrt(d[,2])
  d2 <- round(exp(cbind(d[,1],d[,1]-z1*s,d[,1]+z1*s)),ndig)
  par(mar=mar)
  plot(10,10,xlim=c(0,1),ylim=c(0,1),type='n',ann=FALSE,xaxt='n',yaxt='n',bty='n')
  title(main=main)
  u1 <- par()$usr
  p1 <- par()$pin
  yscale <- (u1[4]-u1[3])/p1[2]
  xscale <- (u1[2]-u1[1])/p1[1]
# pin gives the physical dimension of the current plot (interior) - assume axis and titles are addressed by the mar argument so the full usr and pin region is available for the forest
# determine location (center) of each row in usr coordinates
  nr <- nrow(d)+1
#  nskp <- length(breaks)+1
  nskp <- length(breaks)
  hprin <- p1[2]*(1-nskp*skip)/nr
  hpru <- hprin*yscale
  skipu <- p1[2]*skip*yscale
  yc <- rev(hpru/40+(0:(nr-1))*hpru)
  if (!is.null(breaks)) 
    for (i in 1:length(breaks)) yc[1:(breaks[i]+1)] <- yc[1:(breaks[i]+1)]+skipu
  yc[1] <- yc[1]+skipu
# box height and width
# proportion of max for each box
  sp <- min(s)/s
  bhin <- maxbox*hprin*sp
  bhu <- bhin*yscale/2
  bwu <- bhin*xscale/2
# map confidence intervals to usr coordinates (pos[4],1): a+b*ci=usr
  if (is.null(rowlab)) rowlab <- dimnames(d)[[1]]
  rowlab <- cbind(rowlab,format(d2[,1]),paste('(',format(d2[,2]),', ',format(d2[,3]),')',sep=''))
  nc <- ncol(rowlab)
  if (is.null(collabs)) collabs <- c('Group','Ratio',paste(format(100*confcoef),'% Conf Int',sep=''))
  rowlab <- rbind(collabs,rowlab)
  if (is.null(axtick)) ciw <- range(c(d2,1)) else ciw <- range(axtick)
  b <- (1-pos[nc+1])/(ciw[2]-ciw[1])
  a <- 1-b*ciw[2]
# draw plot
#  text(pos[1:nc],rep(yc[1],nc),collabs,adj=c(0,.5),cex=cex,xpd=TRUE)
#  text(pos[1],yc[1],'Group',adj=c(0,.5),cex=cex,xpd=TRUE)
#  text(pos[2],yc[1],'Ratio',adj=c(0,.5),cex=cex,xpd=TRUE)
#  text(pos[3],yc[1],paste(format(100*confcoef),'% Conf Int',sep=''),adj=c(0,.5),cex=cex,xpd=TRUE)
  lines(pos[c(1,nc+1)],rep((yc[1]+yc[2])/2,2),lty=1,lwd=1)
  if (length(adjx)==1) adjx <- rep(adjx[1],nc)
  for (i in 1:nc) text(pos[i],yc,rowlab[,i],adj=c(adjx[i],.5),cex=cex,xpd=TRUE)
  yc <- yc[-1]
#  text(pos[1],yc,rowlab,adj=c(0,.5),cex=cex)
#  text(pos[2],yc,format(d2[,1]),adj=c(0,.5),cex=cex)
#  text(pos[3],yc,paste('(',format(d2[,2]),', ',format(d2[,3]),')',sep=''),,adj=c(0,.5),cex=cex)
  for (i in 1:(nr-1)) {
    lines(a+b*d2[i,2:3],c(yc[i],yc[i]),lty=1,col=lcol)
    rect(a+b*d2[i,1]-bwu[i],yc[i]-bhu[i],a+b*d2[i,1]+bwu[i],yc[i]+bhu[i],border=NA,col=bcol)
    lines(c(max(a+b*d2[i,2],a+b*d2[i,1]-bwu[i]),min(a+b*d2[i,3],a+b*d2[i,1]+bwu[i])),c(yc[i],yc[i]),lty=1,col=bg)
  }
  if (is.null(axtick)) axtick <- pretty(ciw)
  axis(side=1,at=a+b*axtick,labels=format(axtick))
#  lines(a+b*c(1,1),u1[3:4],lty=1,col=lcol)
  list(ciw=ciw,pos=pos,usr=u1,pin=p1)
}
#lab <- c('Male','Female','PS 0','PS 1','Stage IIIb','Stage IV/rec','Wt Loss <5%','Wt Loss >5%','Age <65','Age >65','Nonmeasurable','Measurable','Prior RT','No prior RT','White','Black','Other Race','Race Unknown','Overall')
#pdf('forest.pdf',width=6,height=7,pointsize=11)
#foresthr(w1,lab,breaks=c(2,4,6,8,10,12,14,18),axtick=c(0,.5,1,1.5),main='E4599 Subgroup Analyses')
#dev.off()

forest.lines <- function(x,y,sc,lwd=1,lty=1,col=1,v=NULL) {
  u1 <- sc$usr
  p1 <- sc$pin
  yscale <- (u1[4]-u1[3])/p1[2]
  xscale <- (u1[2]-u1[1])/p1[1]
  b <- (1-sc$pos[length(sc$pos)])/(sc$ciw[2]-sc$ciw[1])
  a <- 1-b*sc$ciw[2]
  if (!is.null(v)) lines(a+b*c(v,v),u1[3:4],lty=lty,col=col,lwd=lwd)
  else lines(a+b*x,y,lty=lty,col=col,lwd=lwd)
}
