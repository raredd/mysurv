library(mysurv)
u <- R.Version()
if (u$major>'1' | u$major == '1' & u$minor>'6.2') RNGversion("1.6.2")

set.seed(20)
n <- 100
ft <- rexp(n)
st <- sample(0:1,n,replace=T)
grp <- sample(1:4,n,replace=T)
grp2 <- factor(sample(c('a','b','c','d'),n,replace=T),levels=c('b','c','d','e','a'))
str <- sample(1:2,n,replace=T)
print(logrank(ft,st,grp,str))
print(logrank(ft,st,grp2,str))
#graphsheet(color.table=coltab2,pointsize=24)
#smotif()
uu <- km(ft,st,grp2,pv.str=str)
uu <- km(ft,st,grp2,pv.str=str,subset=(grp2 != 'd'))
print(attributes(uu))
plot(uu,wh=2,col=2:5,lty=1,pv=T,lwd=4)
print(logrank(ft,st,grp2,str,subset= (grp2 != 'd')))
uh <- haz(ft,st,grp2,subset= (grp2 != 'd'))
#plot.cuminc(uh,wh=c(1.5,1),color=2:5,lty=1,lwd=4)
uum <- km2mat(uu)
print(uum[,1:5])
print(uum[,81:ncol(uum)])

hh <- function(nt,n=100) {
  out <- rep(0,nt)
  for (i in 1:nt) {
    Ft <- rexp(n)
    St <- sample(0:1,n,replace=T)
    Grp2 <- factor(sample(c('a','b','c','d'),n,replace=T),levels=c('b','c','d','e','a'))
    Str <- sample(1:2,n,replace=T)
    out[i] <- logrank(Ft,St,Grp2,Str)$pv
  }
  out
}
print(hh(5))

